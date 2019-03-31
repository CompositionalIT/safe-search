#r "paket: groupref build //"
#load "./.fake/build.fsx/intellisense.fsx"
#if !FAKE
#r "netstandard"
//#r "Facades/netstandard" // https://github.com/ionide/ionide-vscode-fsharp/issues/839#issuecomment-396296095

#endif

open Cit.Helpers.Arm
open Cit.Helpers.Arm.Parameters
open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators
open Microsoft.Azure.Management.ResourceManager.Fluent.Core
open System
open System.IO
open System.Net

#load "paket-files/build/CompositionalIT/fshelpers/src/FsHelpers/ArmHelper/ArmHelper.fs"

let serverPath = Path.getFullName "./src/Server"
let clientPath = Path.getFullName "./src/Client"
let clientDeployPath = Path.combine clientPath "deploy"
let deployDir = Path.getFullName "./deploy"

let platformTool tool winTool =
    let tool = if Environment.isUnix then tool else winTool
    match ProcessUtils.tryFindFileOnPath tool with
    | Some t ->
        t
    | _ ->
        let errorMsg =
            tool + " was not found in path. "
            + "Please install it and make sure it's available from your path. "
            + "See https://safe-stack.github.io/docs/quickstart/#install-pre-requisites for more info"
        failwith errorMsg

let nodeTool = platformTool "node" "node.exe"
let yarnTool = platformTool "yarn" "yarn.cmd"

let runTool cmd args workingDir =
    let arguments = args |> String.split ' ' |> Arguments.OfArgs
    Command.RawCommand (cmd, arguments)
    |> CreateProcess.fromCommand
    |> CreateProcess.withWorkingDirectory workingDir
    |> CreateProcess.ensureExitCode
    |> Proc.run
    |> ignore

let runDotNet cmd workingDir =
    let result = DotNet.exec (DotNet.Options.withWorkingDirectory workingDir) cmd ""
    if result.ExitCode <> 0 then failwithf "'dotnet %s' failed in %s" cmd workingDir

let openBrowser url =
    //https://github.com/dotnet/corefx/issues/10361
    Command.ShellCommand url
    |> CreateProcess.fromCommand
    |> CreateProcess.ensureExitCodeWithMessage "opening browser failed"
    |> Proc.run
    |> ignore

Target.create "Clean" (fun _ ->
    [ deployDir
      clientDeployPath ]
    |> Shell.cleanDirs)

Target.create "InstallClient" (fun _ ->
    printfn "Node version:"
    runTool nodeTool "--version" __SOURCE_DIRECTORY__
    printfn "Yarn version:"
    runTool yarnTool "--version" __SOURCE_DIRECTORY__
    runTool yarnTool "install --frozen-lockfile" __SOURCE_DIRECTORY__)

Target.create "Build" (fun _ ->
    runDotNet "build" serverPath
    runTool yarnTool "webpack-cli -p" __SOURCE_DIRECTORY__)

Target.create "Run" (fun _ ->
    let server = async { runDotNet "watch run" serverPath }
    let client = async { runTool yarnTool "webpack-dev-server" __SOURCE_DIRECTORY__ }
    let browser = async {
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

let zipFile = "deploy.zip"

Target.create "Bundle" (fun _ ->
    let publishArgs = sprintf "publish -c Release -o \"%s\"" deployDir
    runDotNet publishArgs serverPath

    let publicDir = Path.combine deployDir "public"
    Shell.copyDir publicDir clientDeployPath FileFilter.allFiles

    File.Delete zipFile
    Zip.zip deployDir zipFile !!(deployDir + @"\**\**"))

type ArmOutput =
    { WebAppName : ParameterValue<string>
      WebAppPassword : ParameterValue<string> }

let mutable deploymentOutputs : ArmOutput option = None

Target.create "ArmTemplate" (fun _ ->
    let environment = Environment.environVarOrFail "environment"
    let armTemplate = @"arm-template.json"
    let resourceGroupName = "safe-search-" + environment

    let authCtx =
        // You can safely replace these with your own subscription and client IDs hard-coded into this script.
        let subscriptionId = Environment.environVarOrFail "subscriptionId" |> Guid.Parse
        let clientId = Environment.environVarOrFail "clientId" |> Guid.Parse
        let clientSecret = (Environment.environVarOrFail "clientSecret").Replace("\"", "")
        let tenantId = Environment.environVarOrFail "tenantId" |> Guid.Parse

        Trace.tracefn "Deploying template '%s' to resource group '%s' in subscription '%O'..." armTemplate resourceGroupName subscriptionId

        subscriptionId
        |> authenticate { ClientId = clientId; TenantId = tenantId; ClientSecret = clientSecret }

    let deployment =
        let envAsArmString key = key, ArmString (Environment.environVarOrFail key)
        let location = Environment.environVarOrDefault "location" Region.EuropeWest.Name

        { DeploymentName = sprintf "SAFE-template-deploy-%O" (Guid.NewGuid())
          ResourceGroup = New(resourceGroupName, Region.Create location)
          ArmTemplate = File.ReadAllText armTemplate
          Parameters =
              Simple [ "environment", ArmString environment
                       "location", ArmString location
                       "pricingTier", ArmString (Environment.environVarOrDefault "pricingTier" "F1")
                       "azureSearchName" |> envAsArmString
                       "azureStorageConnection" |> envAsArmString
                       "azureSearchConnection" |> envAsArmString
                       "googleMapsApiKey" |> envAsArmString ]
          DeploymentMode = Incremental }

    deployment
    |> deployWithProgress authCtx
    |> Seq.iter (function
    | DeploymentInProgress(state, operations) ->
        Trace.tracefn "State is %s, completed %d operations." state operations
    | DeploymentError(statusCode, message) ->
        Trace.traceError <| sprintf "DEPLOYMENT ERROR: %s - '%s'" statusCode message
    | DeploymentCompleted d ->
        deploymentOutputs <- d))

// https://github.com/SAFE-Stack/SAFE-template/issues/120
// https://stackoverflow.com/a/6994391/3232646
type TimeoutWebClient() =
    inherit WebClient()
    override __.GetWebRequest uri =
        let request = base.GetWebRequest uri
        request.Timeout <- 30 * 60 * 1000
        request

Target.create "AppService" (fun _ ->
    let appName = deploymentOutputs.Value.WebAppName.value
    let appPassword = deploymentOutputs.Value.WebAppPassword.value
    let destinationUri = sprintf "https://%s.scm.azurewebsites.net/api/zipdeploy" appName
    let client = new TimeoutWebClient(Credentials = NetworkCredential ("$" + appName, appPassword))
    Trace.tracefn "Uploading %s to %s" zipFile destinationUri
    client.UploadData(destinationUri, File.ReadAllBytes zipFile) |> ignore)

"Clean" ==> "InstallClient" ==> "Build" ==> "Bundle" ==> "ArmTemplate" ==> "AppService"
"InstallClient" ==> "Run"
Target.runOrDefaultWithArguments "Build"
