#r "paket: groupref build //"
#load "./.fake/build.fsx/intellisense.fsx"
#if !FAKE
#r "netstandard"
#r "Facades/netstandard" // https://github.com/ionide/ionide-vscode-fsharp/issues/839#issuecomment-396296095

#endif

open Cit.Helpers.Arm
open Cit.Helpers.Arm.Parameters
open Fake.Core
open Fake.DotNet
open Fake.IO
open Microsoft.Azure.Management.ResourceManager.Fluent.Core

#load "paket-files/build/CompositionalIT/fshelpers/src/FsHelpers/ArmHelper/ArmHelper.fs"

open System

let serverPath = Path.getFullName "./src/Server"
let clientPath = Path.getFullName "./src/Client"
let deployDir = Path.getFullName "./deploy"

let platformTool tool winTool =
    let tool =
        if Environment.isUnix then tool
        else winTool
    match ProcessUtils.tryFindFileOnPath tool with
    | Some t -> t
    | _ -> 
        let errorMsg =
            tool + " was not found in path. " 
            + "Please install it and make sure it's available from your path. " 
            + "See https://safe-stack.github.io/docs/quickstart/#install-pre-requisites for more info"
        failwith errorMsg

let nodeTool = platformTool "node" "node.exe"
let yarnTool = platformTool "yarn" "yarn.cmd"

let runTool cmd args workingDir =
    let arguments =
        args
        |> String.split ' '
        |> Arguments.OfArgs
    Command.RawCommand(cmd, arguments)
    |> CreateProcess.fromCommand
    |> CreateProcess.withWorkingDirectory workingDir
    |> CreateProcess.ensureExitCode
    |> Proc.run
    |> ignore

let runDotNet cmd workingDir =
    let result =
        DotNet.exec (DotNet.Options.withWorkingDirectory workingDir) cmd ""
    if result.ExitCode <> 0 then 
        failwithf "'dotnet %s' failed in %s" cmd workingDir

let openBrowser url =
    //https://github.com/dotnet/corefx/issues/10361
    Command.ShellCommand url
    |> CreateProcess.fromCommand
    |> CreateProcess.ensureExitCodeWithMessage "opening browser failed"
    |> Proc.run
    |> ignore

Target.create "Clean" (fun _ -> Shell.cleanDirs [ deployDir ])
Target.create "InstallClient" (fun _ -> 
    printfn "Node version:"
    runTool nodeTool "--version" __SOURCE_DIRECTORY__
    printfn "Yarn version:"
    runTool yarnTool "--version" __SOURCE_DIRECTORY__
    runTool yarnTool "install --frozen-lockfile" __SOURCE_DIRECTORY__
    runDotNet "restore" clientPath)
Target.create "Build" 
    (fun _ -> 
    runDotNet "build" serverPath
    runDotNet "fable webpack-cli -- --config src/Client/webpack.config.js -p" 
        clientPath)
Target.create "Run" (fun _ -> 
    let server = async { runDotNet "watch run" serverPath }
    let client =
        async 
            { 
            runDotNet 
                "fable webpack-dev-server -- --config src/Client/webpack.config.js" 
                clientPath }
    
    let browser =
        async { 
            do! Async.Sleep 5000
            openBrowser "http://localhost:8080"
        }
    
    let vsCodeSession = Environment.hasEnvironVar "vsCodeSession"
    let safeClientOnly = Environment.hasEnvironVar "safeClientOnly"
    
    let tasks =
        [ if not safeClientOnly then yield server
          yield client
          if not vsCodeSession then yield browser ]
    tasks
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore)
Target.create "Bundle" 
    (fun _ -> 
    runDotNet 
        (sprintf "publish \"%s\" -c release -o \"%s\"" serverPath deployDir) 
        __SOURCE_DIRECTORY__
    Shell.copyDir (Path.combine deployDir "public") 
        (Path.combine clientPath "public") FileFilter.allFiles)

type ArmOutput =
    { WebAppName : ParameterValue<string>
      WebAppPassword : ParameterValue<string> }

let mutable deploymentOutputs : ArmOutput option = None

Target.create "ArmTemplate" (fun _ -> 
    let environment =
        Environment.environVarOrDefault "environment" 
            (Guid.NewGuid().ToString().ToLower().Split '-' |> Array.head)
    let armTemplate = @"arm-template.json"
    let resourceGroupName = "safe-search-" + environment
    
    let authCtx =
        // You can safely replace these with your own subscription and client IDs hard-coded into this script.
        let subscriptionId = Guid "536309b7-121d-4a8a-81ed-9a5a25240086"
        let clientId = Guid "d1c56ed8-87f9-434e-aeb7-c702ba3ec9db"
        Trace.tracefn 
            "Deploying template '%s' to resource group '%s' in subscription '%O'..." 
            armTemplate resourceGroupName subscriptionId
        subscriptionId
        |> authenticateDevice Trace.trace { ClientId = clientId
                                            TenantId = None }
        |> Async.RunSynchronously
    
    let deployment =
        let location =
            Environment.environVarOrDefault "location" Region.EuropeWest.Name
        let pricingTier = Environment.environVarOrDefault "pricingTier" "F1"
        { DeploymentName = "SAFE-template-deploy"
          ResourceGroup = New(resourceGroupName, Region.Create location)
          ArmTemplate = IO.File.ReadAllText armTemplate
          Parameters =
              Simple [ "environment", ArmString environment
                       "location", ArmString location
                       "pricingTier", ArmString pricingTier ]
          DeploymentMode = Incremental }
    
    deployment
    |> deployWithProgress authCtx
    |> Seq.iter (function 
           | DeploymentInProgress(state, operations) -> 
               Trace.tracefn "State is %s, completed %d operations." state 
                   operations
           | DeploymentError(statusCode, message) -> 
               Trace.traceError 
               <| sprintf "DEPLOYMENT ERROR: %s - '%s'" statusCode message
           | DeploymentCompleted d -> deploymentOutputs <- d))

open Fake.IO.Globbing.Operators
open System.Net

// https://github.com/SAFE-Stack/SAFE-template/issues/120
// https://stackoverflow.com/a/6994391/3232646
type TimeoutWebClient() =
    inherit WebClient()
    override this.GetWebRequest uri =
        let request = base.GetWebRequest uri
        request.Timeout <- 30 * 60 * 1000
        request

Target.create "AppService" (fun _ -> 
    let zipFile = "deploy.zip"
    IO.File.Delete zipFile
    Zip.zip deployDir zipFile !!(deployDir + @"\**\**")
    let appName = deploymentOutputs.Value.WebAppName.value
    let appPassword = deploymentOutputs.Value.WebAppPassword.value
    let destinationUri =
        sprintf "https://%s.scm.azurewebsites.net/api/zipdeploy" appName
    let client =
        new TimeoutWebClient(Credentials = NetworkCredential
                                               ("$" + appName, appPassword))
    Trace.tracefn "Uploading %s to %s" zipFile destinationUri
    client.UploadData(destinationUri, IO.File.ReadAllBytes zipFile) |> ignore)

open Fake.Core.TargetOperators

"Clean" ==> "InstallClient" ==> "Build" ==> "Bundle" ==> "ArmTemplate" 
==> "AppService"
"Clean" ==> "InstallClient" ==> "Run"
Target.runOrDefaultWithArguments "Build"
