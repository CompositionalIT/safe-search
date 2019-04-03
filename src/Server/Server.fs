module SafeSearch.Server

open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Saturn
open System.IO
open FSharp.Control.Tasks
open Microsoft.AspNetCore.Authentication.Cookies
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Authentication.OpenIdConnect
open System
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Giraffe

let tryGetEnv =
    System.Environment.GetEnvironmentVariable
    >> function
    | null | "" -> None
    | x -> Some x

let publicPath =
    tryGetEnv "public_path"
    |> Option.defaultValue "../Client/public"
    |> Path.GetFullPath

let webApp searcher appConfig = router {
    forward "/api/search/" (Routers.Search.createRouter searcher appConfig (SafeSearch.Storage.tryGetGeo appConfig.AzureStorage))
    forward "/api/transactions/" (Routers.Transactions.createRouter searcher appConfig.AzureStorage)
    forward "/api/postcodes/" (Routers.Postcodes.createRouter appConfig.AzureStorage)
    get "/api/login" (Auth.requiresAuthentication (challenge "AzureAD") >=> text "Logged in.") }

let configureAppInsights (services : IServiceCollection) =
    match tryGetEnv "APPINSIGHTS_INSTRUMENTATIONKEY" with
    | Some key -> services.AddApplicationInsightsTelemetry key
    | None -> services

let appConfig =
    let builder =
        let path = DirectoryInfo(Directory.GetCurrentDirectory()).Parent.FullName
        printfn "Searching for configuration in %s" path
        ConfigurationBuilder()
            .SetBasePath(path)
            .AddJsonFile("appsettings.json", optional = true)
            .AddEnvironmentVariables()
            .Build()
    { AzureStorage = ConnectionString(builder.GetConnectionString "AzureStorage")
      GoogleMapsApiKey = builder.GetConnectionString "GoogleMapsApiKey"
      AzureSearch = ConnectionString(builder.GetConnectionString "AzureSearch"), builder.["AzureSearchName"]
      AzureAdDomain = builder.GetConnectionString "AzureAdDomain"
      AzureAdClientId = builder.GetConnectionString "AzureAdClientId"
      AzureAdClientSecret = builder.GetConnectionString "AzureAdClientSecret" }

let searcher =
    Search.Azure.Management.initialize Search.Azure.Management.OnlyIfNonExistant appConfig.AzureSearch appConfig.AzureStorage
    Search.Azure.searcher appConfig.AzureSearch appConfig.AzureStorage

let configureOpenIdConnect appConfig (services : IServiceCollection) =
    services
        .AddAuthentication(fun options ->
            options.DefaultAuthenticateScheme <- CookieAuthenticationDefaults.AuthenticationScheme
            options.DefaultSignInScheme <- CookieAuthenticationDefaults.AuthenticationScheme
            options.DefaultChallengeScheme <- CookieAuthenticationDefaults.AuthenticationScheme)
        .AddCookie()
        .AddOpenIdConnect("AzureAD", fun options -> 
            options.Authority <- sprintf "https://%s" appConfig.AzureAdDomain

            options.ClientId <- appConfig.AzureAdClientId
            options.ClientSecret <- appConfig.AzureAdClientSecret

            options.ResponseType <- "code"

            options.Scope.Clear()
            options.Scope.Add("openid")

            options.CallbackPath <- PathString("/api/callback")

            options.ClaimsIssuer <- "AzureAD"

            options.Events <- OpenIdConnectEvents(
                OnRedirectToIdentityProvider = fun x ->
                    x.ProtocolMessage.RedirectUri <- "http://localhost:8080/api/callback"
                    Task.CompletedTask))
            .Services

let appAuth (app:IApplicationBuilder) =
    app
        .UseAuthentication()

let app =
    application {
        url "http://0.0.0.0:8085/"
        use_router (webApp searcher appConfig)
        memory_cache
        use_static publicPath
        use_json_serializer (Thoth.Json.Giraffe.ThothSerializer())
        service_config configureAppInsights
        service_config (configureOpenIdConnect appConfig)
        app_config appAuth
        use_gzip
    }

run app
