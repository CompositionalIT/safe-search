module SafeSearch.Server

open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Configuration
open Saturn
open System.IO

let publicPath = Path.GetFullPath "../Client/public"
let port = 8085us
let webApp searcher appConfig = router {
    forward "/api/search/" (Routers.Search.createRouter searcher (SafeSearch.Storage.tryGetGeo appConfig.AzureStorage))
    forward "/api/transactions/" (Routers.Transactions.createRouter searcher appConfig.AzureStorage)
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
      AzureSearch = ConnectionString (builder.GetConnectionString "AzureSearch"), builder.["AzureSearchName"] }

let searcher =
    Search.Azure.Management.initialize Search.Azure.Management.OnlyIfNonExistant appConfig.AzureSearch appConfig.AzureStorage
    Search.Azure.searcher appConfig.AzureSearch appConfig.AzureStorage

let app = application {
    url ("http://0.0.0.0:" + port.ToString() + "/")
    use_router (webApp searcher appConfig)
    memory_cache
    use_static publicPath
    service_config configureSerialization
    use_gzip
}

run app
