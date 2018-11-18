module SafeSearch.Server

open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Configuration
open Saturn
open System.IO

let publicPath = Path.GetFullPath "../Client/public"
let port = 8085us
let webApp searcher appConfig = router {
    forward "/api/property/" (Routers.Search.createRouter searcher)
    forward "/api/transactions/" (Routers.Transactions.createRouter searcher)
    forward "/api/postcodes/" (Routers.Postcodes.createRouter appConfig.AzureStorage)
}

let configureSerialization (services:IServiceCollection) =
    services.AddSingleton<Giraffe.Serialization.Json.IJsonSerializer>(Thoth.Json.Giraffe.ThothSerializer())

let appConfig =
    let builder =
        let path = Directory.GetCurrentDirectory()
        printfn "Searching for configuration in %s" path
        ConfigurationBuilder()
            .SetBasePath(path)
            .AddJsonFile("appsettings.json", optional = true)
            .AddEnvironmentVariables()
            .Build()

    { AzureStorage = builder.GetConnectionString "AzureStorage" |> ConnectionString
      AzureSearch =
        match builder.GetConnectionString "AzureSearch" |> Option.ofString, builder.["AzureSearchName"]  |> Option.ofString with
        | Some conn, Some name -> Some(ConnectionString conn, name)
        | _ -> None }
let searcher =
    match appConfig with
    | { AzureSearch = Some searchConfig } ->
        printfn "Using live searcher"
        Search.Azure.Management.initialize searchConfig
        Search.Azure.searcher searchConfig (SafeSearch.Storage.tryGetGeo appConfig.AzureStorage)
    | _ ->
        printfn "Using in memory searcher"
        Search.InMemory.searcher

let app = application {
    url ("http://0.0.0.0:" + port.ToString() + "/")
    use_router (webApp searcher appConfig)
    memory_cache
    use_static publicPath
    service_config configureSerialization
    use_gzip
}

run app
