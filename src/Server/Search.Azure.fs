module SafeSearch.Search.Azure

open FSharp.Control.Tasks
open Microsoft.Azure.Search
open Microsoft.Azure.Search.Models
open Microsoft.Spatial
open SafeSearch
open System
open System.ComponentModel.DataAnnotations
open System.Threading.Tasks

type SearchableProperty =
    { [<Key; IsFilterable>]
      TransactionId : string
      [<IsFacetable; IsFilterable; IsSortable>]
      Price : int Nullable
      [<IsFilterable; IsSortable>]
      DateOfTransfer : DateTime Nullable
      [<IsSearchable; IsSortable>]
      PostCode : string
      [<IsFacetable; IsFilterable>]
      PropertyType : string
      [<IsFacetable; IsFilterable>]
      Build : string
      [<IsFacetable; IsFilterable>]
      Contract : string
      [<IsSortable>]
      Building : string
      [<IsSearchable; IsSortable>]
      Street : string
      [<IsFacetable; IsFilterable; IsSearchable>]
      Locality : string
      [<IsFacetable; IsFilterable; IsSearchable; IsSortable>]
      Town : string
      [<IsFacetable; IsFilterable; IsSearchable>]
      District : string
      [<IsFacetable; IsFilterable; IsSearchable>]
      County : string
      [<IsFilterable; IsSortable>]
      Geo : GeographyPoint }

let suggesterName = "suggester"
let indexName = "properties"

[<AutoOpen>]
module Management =
    open System.Collections.Generic

    let searchClient =
        let connections = Dictionary()
        fun searchConfig ->
            if not (connections.ContainsKey searchConfig) then
                let (ConnectionString c) = (fst searchConfig)
                connections.[searchConfig] <- new SearchServiceClient(snd searchConfig, SearchCredentials c)
            connections.[searchConfig]

    let propertiesIndex searchConfig =
        let client = searchClient searchConfig
        client.Indexes.GetClient indexName

    type InitializationMode =
        | ForceReset
        | OnlyIfNonExistant

    let initialize initMode searchConfig (ConnectionString storageConfig) =
        let client = searchClient searchConfig
        // index
        match initMode, client.Indexes.Exists indexName with
        | ForceReset, _
        | _, false ->
            client.Indexes.Delete indexName
            let index =
                Models.Index (
                    Name = indexName,
                    Fields = FieldBuilder.BuildForType<SearchableProperty>(),
                    Suggesters = [|
                        Suggester (
                            Name = suggesterName,
                            SourceFields = [|
                                "Street"; "Locality";
                                "Town"; "District";
                                "County"
                            |]
                        )
                    |]
                )
            client.Indexes.Create index |> ignore

            let containerClient container = Azure.Storage.Blobs.BlobContainerClient(storageConfig, container)
            let propertiesContainer = containerClient "properties"
            propertiesContainer.CreateIfNotExists() |> ignore
            for blob in propertiesContainer.GetBlobs() do
                propertiesContainer.GetBlobClient(blob.Name).DeleteIfExists() |> ignore

            let ds =
                DataSource
                    (Container = DataContainer(Name = "properties"),
                     Credentials = DataSourceCredentials(ConnectionString = storageConfig),
                     Name = "blob-transactions",
                     Type = DataSourceType.AzureBlob)
            client.DataSources.Delete ds.Name
            client.DataSources.Create ds |> ignore
            // indexer
            let indexer =
                Indexer
                    (Name = "properties-indexer", DataSourceName = ds.Name,
                     TargetIndexName = indexName,
                     Schedule = IndexingSchedule(TimeSpan.FromMinutes 5.),
                     Parameters = IndexingParameters().ParseJsonArrays())
            client.Indexers.Delete indexer.Name
            client.Indexers.Create indexer |> ignore
        | _ ->
            ()

/// Gets the columns to search on given a property field.
let private toSearchColumns col =
    match PropertyTableColumn.TryParse col with
    | Some Street -> [ "Building"; "Street" ]
    | Some Town -> [ "Town" ]
    | Some Postcode -> [ "PostCode" ]
    | Some Date -> [ "DateOfTransfer" ]
    | Some Price -> [ "Price" ]
    | None -> []

let private toFindPropertiesResponse findFacet count page results =
    { Results =
          results
          |> Array.map (fun result ->
                 { BuildDetails =
                       { PropertyType =
                             result.PropertyType |> PropertyType.Parse
                         Build = result.Build |> BuildType.Parse
                         Contract = result.Contract |> ContractType.Parse }
                   Address =
                       { Building = result.Building
                         Street = result.Street |> Option.ofObj
                         Locality = result.Locality |> Option.ofObj
                         TownCity = result.Town
                         District = result.District
                         County = result.County
                         PostCode = result.PostCode |> Option.ofObj
                         GeoLocation =
                             result.Geo
                             |> Option.ofObj
                             |> Option.map (fun geo ->
                                    { Lat = geo.Latitude
                                      Long = geo.Longitude }) }
                   Price =
                       result.Price
                       |> Option.ofNullable
                       |> Option.defaultValue 0
                   DateOfTransfer =
                       result.DateOfTransfer
                       |> Option.ofNullable
                       |> Option.defaultValue DateTime.MinValue })
      TotalTransactions = count
      Facets =
          { Towns = findFacet "Town" |> Option.defaultValue []
            Localities = findFacet "Locality" |> Option.defaultValue []
            Districts = findFacet "District" |> Option.defaultValue []
            Counties = findFacet "County" |> Option.defaultValue []
            Prices = findFacet "Price" |> Option.defaultValue [] }
      Page = page }

open Filters

let findGeneric searchConfig (request : FindGenericRequest) =
    task {
        let query =
            let toFieldSort =
                match request.Sort.SortDirection with
                | Some Ascending | None ->
                    fun s -> ByField(s, Direction.Ascending)
                | Some Descending -> fun s -> ByField(s, Direction.Descending)
            azureSearch {
                fulltext (request.Text |> Option.map(sprintf "%s*") |> Option.toObj)
                filter (request.Filter.AllFilters
                        |> List.map whereEq
                        |> combine)
                sort (request.Sort.SortColumn
                      |> Option.map toSearchColumns
                      |> Option.defaultValue []
                      |> List.map toFieldSort)
                skip (request.Page * 20)
                top 20
                facets [ "Town"; "Locality"; "District"; "County"; "Price" ]
                includeTotalResults
            }

        let searchClient = searchClient searchConfig
        printfn "%O" query
        let! results, facets, count = Kibalta.doSearch<SearchableProperty>
                                          indexName searchClient query
        return results
               |> toFindPropertiesResponse facets.TryFind count request.Page
    }

let findByPostcode searchConfig (request : FindNearestRequest) =
    task {
        let query =
            let postcodeFilter =
                let geoFilter = whereGeoDistance "Geo" (request.Geo.Long, request.Geo.Lat) Lt (float request.MaxDistance)
                let basicFilters = request.Filter.AllFilters |> List.map whereEq
                combine (geoFilter :: basicFilters)
            azureSearch {
                filter postcodeFilter
                sort [
                    ByDistance ("Geo", request.Geo.Long, request.Geo.Lat, Direction.Ascending)
                ]
                skip (request.Page * 20)
                top 20
                facets [ "Town"; "Locality"; "District"; "County"; "Price" ]
                includeTotalResults
            }

        let searchClient = searchClient searchConfig
        let! results, facets, count = Kibalta.doSearch<SearchableProperty>
                                          indexName searchClient query
        return results
               |> toFindPropertiesResponse facets.TryFind count request.Page
    }

let getDocumentSize searchConfig =
    let index = propertiesIndex searchConfig
    index.Documents.CountAsync()

let suggest config (request : SuggestRequest) =
    task {
        let index = propertiesIndex config
        let! result = index.Documents.SuggestAsync
                          (request.Text, suggesterName,
                           SuggestParameters(Top = Nullable 10))
        return { Suggestions =
                     result.Results
                     |> Seq.map (fun x -> x.Text)
                     |> Seq.distinct
                     |> Seq.toArray }
    }

let searcher searchConfig storageConfig =
    { new Search.ISearch with
          member __.GenericSearch request = findGeneric searchConfig request
          member __.LocationSearch request = findByPostcode searchConfig request
          member __.Suggest request = suggest searchConfig request
          member __.Documents() = getDocumentSize searchConfig
          member __.Clear() = initialize ForceReset searchConfig storageConfig }
