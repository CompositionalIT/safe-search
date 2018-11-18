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
    { [<Key; IsFilterable>] TransactionId : string
      [<IsFacetable; IsSortable>] Price : int Nullable
      [<IsFilterable; IsSortable>] DateOfTransfer : DateTime Nullable
      [<IsSearchable; IsSortable>] PostCode : string
      [<IsFacetable; IsFilterable>] PropertyType : string
      [<IsFacetable; IsFilterable>] Build : string
      [<IsFacetable; IsFilterable>] Contract : string
      [<IsSortable>] Building : string
      [<IsSearchable; IsSortable>] Street : string
      [<IsFacetable; IsFilterable; IsSearchable>] Locality : string
      [<IsFacetable; IsFilterable; IsSearchable; IsSortable>] Town : string
      [<IsFacetable; IsFilterable; IsSearchable>] District : string
      [<IsFacetable; IsFilterable; IsSearchable>] County : string
      [<IsFilterable>] Geo : GeographyPoint }
    static member Default =
        { PostCode = null; PropertyType = null; Build = null; Contract = null
          Building = null; Street = null; Locality = null; Town = null
          District = null; County = null; Price = Nullable(); DateOfTransfer = Nullable();
          TransactionId = null; Geo = null }

let suggesterName = "suggester"

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
        client.Indexes.GetClient "properties"

    let initialize config =
        let client = searchClient config
        if not (client.Indexes.Exists "properties") then
            let suggester = Suggester(Name = suggesterName, SourceFields = [| "Street"; "Locality"; "Town"; "District"; "County" |])
            Index(Name = "properties", Fields = FieldBuilder.BuildForType<SearchableProperty>(), Suggesters = [| suggester |])
            |> client.Indexes.Create
            |> ignore

[<AutoOpen>]
module QueryBuilder =
    let findByDistance (geo:GeographyPoint) maxDistance =
        SearchParameters(Filter=sprintf "geo.distance(Geo, geography'POINT(%f %f)') le %d" geo.Longitude geo.Latitude maxDistance)
    let withFilter (parameters:SearchParameters) (field, value:string option) =
        parameters.Filter <-
            [ (match parameters.Filter with f when String.IsNullOrWhiteSpace f -> None | f -> Some f)
              (value |> Option.map(fun value -> sprintf "(%s eq '%s')" field (value.ToUpper()))) ]
            |> List.choose id
            |> String.concat " and "
        parameters
    
    let toSearchColumns col =
        match PropertyTableColumn.TryParse col with
        | Some Street -> [ "Building"; "Street" ]
        | Some Town -> [ "Town" ]
        | Some Postcode -> [ "PostCode" ]
        | Some Date -> [ "DateOfTransfer" ]
        | Some Price -> [ "Price" ]
        | None -> []
        
    let orderBy sort (parameters:SearchParameters) =
        sort.SortColumn |> Option.iter (fun col ->
            let searchCols = toSearchColumns col
            let direction = match sort.SortDirection with Some Ascending | None -> "asc" | Some Descending -> "desc"
            parameters.OrderBy <- searchCols |> List.map (fun col -> sprintf "%s %s" col direction) |> ResizeArray)
        parameters

    let doSearch searchConfig page searchText (parameters:SearchParameters) = task {
        let searchText = searchText |> Option.map(fun searchText -> searchText + "*") |> Option.defaultValue ""
        parameters.Facets <- ResizeArray [ "Town"; "Locality"; "District"; "County"; "Price" ]
        parameters.Skip <- Nullable(page * 20)
        parameters.Top <- Nullable 20
        parameters.IncludeTotalResultCount <- true
        let! searchResult = (propertiesIndex searchConfig).Documents.SearchAsync<SearchableProperty>(searchText, parameters)
        let facets =
            searchResult.Facets
            |> Seq.map(fun x -> x.Key, x.Value |> Seq.map(fun r -> r.Value |> string) |> Seq.toList)
            |> Map.ofSeq
            |> fun x -> x.TryFind >> Option.defaultValue []
        return facets, searchResult.Results |> Seq.toArray |> Array.map(fun r -> r.Document), searchResult.Count |> Option.ofNullable |> Option.map int }

[<Literal>]
let BATCH_INSERT_SIZE = 1000

let insertProperties onComplete config (properties:(PropertyResult * ((float * float) option)) seq) = task {
    let index = propertiesIndex config
    let createTransactionId r =
        sprintf "%s%s%s%d"
            r.Address.Building
            (r.Address.Street |> Option.toObj)
            (r.Address.PostCode |> Option.toObj)
            r.Price
        |> fun s -> s.Replace(" ", "")
        |> (Seq.filter Char.IsLetterOrDigit >> Array.ofSeq >> String)

    let toSearchableProperty (prop:PropertyResult, geo) =
        { TransactionId = createTransactionId prop
          Price = Nullable prop.Price
          DateOfTransfer = Nullable prop.DateOfTransfer
          PostCode = prop.Address.PostCode |> Option.toObj
          PropertyType = prop.BuildDetails.PropertyType |> Option.map string |> Option.toObj
          Build = prop.BuildDetails.Build.ToString()
          Contract = prop.BuildDetails.Contract.ToString()
          Building = prop.Address.Building
          Street = prop.Address.Street |> Option.toObj
          Locality = prop.Address.Locality |> Option.toObj
          Town = prop.Address.TownCity
          District = prop.Address.District
          County = prop.Address.County
          Geo = geo
                |> Option.map(fun (lat, long) -> GeographyPoint.Create(lat, long))
                |> Option.toObj }    

    let rec retry retries (x:'a -> unit Task) v = task {
        try
        return! x v
        with _ when retries > 0 ->
            printfn "Failed (%d), retrying..." retries
            return! retry (retries - 1 )x v }

    let insertBatch (batch:SearchableProperty seq) = task {
        let! request = IndexBatch.Upload batch |> index.Documents.IndexAsync
        let success, fail = request.Results |> Seq.toArray |> Array.partition(fun r -> r.Succeeded)
        onComplete (success.Length, fail.Length) }
        
    let! _ =
        properties
        |> Seq.map toSearchableProperty
        |> Seq.chunkBySize BATCH_INSERT_SIZE
        |> Seq.map (retry 3 insertBatch)
        |> Task.WhenAll
    () }

let private toFindPropertiesResponse findFacet count page results =
    { Results =
        results
        |> Array.map(fun result ->
             { BuildDetails =
                 { PropertyType = result.PropertyType |> PropertyType.Parse
                   Build = result.Build |> BuildType.Parse
                   Contract = result.Contract |> ContractType.Parse }
               Address =
                 { Building = result.Building
                   Street = result.Street |> Option.ofObj
                   Locality = result.Locality |> Option.ofObj
                   TownCity = result.Town
                   District = result.District
                   County = result.County
                   PostCode = result.PostCode |> Option.ofObj }
               Price = result.Price |> Option.ofNullable |> Option.defaultValue 0
               DateOfTransfer = result.DateOfTransfer |> Option.ofNullable |> Option.defaultValue DateTime.MinValue })
      TotalTransactions = count
      Facets =
        { Towns = findFacet "TownCity"
          Localities = findFacet "Locality"
          Districts = findFacet "District"
          Counties = findFacet "County"
          Prices = findFacet "Price" }
      Page = page }

let applyFilters (filter:PropertyFilter) parameters =
    [ "TownCity", filter.Town
      "County", filter.County
      "Locality", filter.Locality
      "District", filter.District ]
    |> List.fold withFilter parameters

let findGeneric searchConfig request = task {
    let! findFacet, searchResults, count =
        SearchParameters()
        |> applyFilters request.Filter
        |> orderBy request.Sort
        |> doSearch searchConfig request.Page request.Text
    return searchResults |> toFindPropertiesResponse findFacet count request.Page }

let findByPostcode config (tryGetGeo:string -> Task<Geo option>) request = task {
    match! tryGetGeo request.Postcode with
    | Some geo ->
        let geo = GeographyPoint.Create(geo.Lat, geo.Long)
        let! findFacet, searchResults, count =
            findByDistance geo request.MaxDistance
            |> applyFilters request.Filter
            |> doSearch config request.Page None            
        return searchResults |> toFindPropertiesResponse findFacet count request.Page
    | None ->
        return Array.empty |> toFindPropertiesResponse (fun _ -> []) None 0 }

let getDocumentSize searchConfig =
    let index = propertiesIndex searchConfig
    index.Documents.CountAsync()

let suggest config request = task {
    let index = propertiesIndex config
    let! result = index.Documents.SuggestAsync(request.Text, suggesterName, SuggestParameters(Top = Nullable(10)))
    return { Suggestions = result.Results |> Seq.map (fun x -> x.Text) |> Seq.distinct |> Seq.toArray } }

let searcher searchConfig tryGetGeo =
    { new Search.ISearch with
        member __.GenericSearch request = findGeneric searchConfig request
        member __.PostcodeSearch request = findByPostcode searchConfig tryGetGeo request
        member __.Suggest request = suggest searchConfig request
        member __.Upload onComplete properties = insertProperties onComplete searchConfig properties
        member __.Documents() = getDocumentSize searchConfig
        member __.Clear() =
            let client = searchClient searchConfig
            client.Indexes.Delete "properties"            
            initialize searchConfig
    }