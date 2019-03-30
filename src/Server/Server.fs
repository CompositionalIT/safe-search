module SafeSearch.Server

open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Saturn
open System.IO

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
    forward "/api/postcodes/" (Routers.Postcodes.createRouter appConfig.AzureStorage) }

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
      AzureSearch = ConnectionString(builder.GetConnectionString "AzureSearch"), builder.["AzureSearchName"] }

let searcher =
    Search.Azure.Management.initialize Search.Azure.Management.OnlyIfNonExistant appConfig.AzureSearch appConfig.AzureStorage
    Search.Azure.searcher appConfig.AzureSearch appConfig.AzureStorage

let app =
    application {
        url "http://0.0.0.0:8085/"
        use_router (webApp searcher appConfig)
        memory_cache
        use_static publicPath
        use_json_serializer (Thoth.Json.Giraffe.ThothSerializer())
        service_config configureAppInsights
        use_gzip
    }

run app
