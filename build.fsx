#r "paket: groupref build //"
#load "./.fake/build.fsx/intellisense.fsx"
#r "netstandard"

open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators
open Farmer.Builders
open Cit.Helpers.Arm
open Cit.Helpers.Arm.Parameters
open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Microsoft.Azure.Management.ResourceManager.Fluent.Core
open Farmer
open System
open System.IO
open System.Net

#load "paket-files/build/CompositionalIT/fshelpers/src/FsHelpers/ArmHelper/ArmHelper.fs"

Target.initEnvironment ()

let sharedPath = Path.getFullName "./src/Shared"
let serverPath = Path.getFullName "./src/Server"
let deployDir = Path.getFullName "./deploy"

let npm args workingDir =
    let npmPath =
        match ProcessUtils.tryFindFileOnPath "npm" with
        | Some path -> path
        | None ->
            "npm was not found in path. Please install it and make sure it's available from your path. " +
            "See https://safe-stack.github.io/docs/quickstart/#install-pre-requisites for more info"
            |> failwith

    let arguments = args |> String.split ' ' |> Arguments.OfArgs

    Command.RawCommand (npmPath, arguments)
    |> CreateProcess.fromCommand
    |> CreateProcess.withWorkingDirectory workingDir
    |> CreateProcess.ensureExitCode
    |> Proc.run
    |> ignore

let dotnet cmd workingDir =
    let result = DotNet.exec (DotNet.Options.withWorkingDirectory workingDir) cmd ""
    if result.ExitCode <> 0 then failwithf "'dotnet %s' failed in %s" cmd workingDir

Target.create "Clean" (fun _ -> Shell.cleanDir deployDir)

Target.create "InstallClient" (fun _ -> npm "install" ".")

Target.create "Build" (fun _ ->
    dotnet (sprintf "publish -c Release -o \"%s\"" deployDir) serverPath
    npm "run build" "."
)

Target.create "Run" (fun _ ->
    dotnet "build" sharedPath
    [ async { dotnet "watch run" serverPath }
      async { npm "run start" "." } ]
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore
)

let zipFile = "deploy.zip"

Target.create "Bundle" (fun _ ->
    let publishArgs = sprintf "publish -c Release -o \"%s\"" deployDir
    dotnet publishArgs serverPath

    let publicDir = Path.combine deployDir "public"
//    Shell.copyDir publicDir clientDeployPath FileFilter.allFiles
    Shell.copyDir publicDir deployDir FileFilter.allFiles

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

open Fake.Core.TargetOperators

"Clean"
    ==> "InstallClient"
    ==> "Build"
    ==> "Bundle"
    ==> "ArmTemplate"
    ==> "AppService"

"Clean"
    ==> "InstallClient"
    ==> "Run"

Target.runOrDefaultWithArguments "Build"