module SafeSearch.Search.InMemory

open FSharp.Control.Tasks
open SafeSearch
open System.Text.RegularExpressions
open System.Threading.Tasks

let private loadData() =
    "properties.json"
    |> System.IO.File.ReadAllText
    |> Thoth.Json.Net.Decode.Auto.unsafeFromString<PropertyResult array>

let findByPostcode (request:FindNearestRequest) = task {
    return    
        { Results = [||]
          TotalTransactions = None
          Page = request.Page
          Facets =
            { Towns = []
              Localities = []
              Districts = []
              Counties = []
              Prices = [] } } }

let sortResults sortParams results =
    match sortParams.SortColumn with
    | Some col ->
        let directedCompare a b =
            match PropertyTableColumn.TryParse col with
            | Some Street -> compare a.Address.Street b.Address.Street
            | Some Town -> compare a.Address.TownCity b.Address.TownCity
            | Some Postcode -> compare a.Address.PostCode b.Address.PostCode
            | Some Date -> compare a.DateOfTransfer b.DateOfTransfer
            | Some Price -> compare a.Price b.Price
            | None -> 0
            |> fun v ->
                match sortParams.SortDirection with
                | Some Ascending | None -> v
                | Some Descending -> -v
        results |> Array.sortWith directedCompare
    | None -> results

let findGeneric (request:FindGenericRequest) = task {
    let genericFilter =
        match request.Text with
        | Some text ->
            let text = text.ToUpper()
            fun r ->
                r.Address.County.ToUpper().Contains text ||
                r.Address.District.ToUpper().Contains text ||
                r.Address.Locality |> Option.map(fun l -> l.ToUpper().Contains text) |> Option.defaultValue false ||
                r.Address.TownCity.ToUpper().Contains text ||
                r.Address.Street |> Option.map (fun s -> s.ToUpper().Contains text)  |> Option.defaultValue false
        | None -> fun _ -> true
    let facetFilter filter mapper =
        match filter with
        | Some filter -> mapper >> fun (s:string) -> s.ToUpper() = filter
        | None -> fun _ -> true

    let matches =
        loadData()
        |> Array.filter genericFilter
        |> Array.filter (facetFilter request.Filter.County (fun r -> r.Address.County))
        |> Array.filter (facetFilter request.Filter.District (fun r -> r.Address.District))
        |> Array.filter (facetFilter request.Filter.Locality (fun r -> r.Address.Locality |> Option.defaultValue ""))
        |> Array.filter (facetFilter request.Filter.Town (fun r -> r.Address.TownCity))
        |> sortResults request.Sort
            
    let getFacets mapper = Array.choose mapper >> Array.distinct >> Array.truncate 10 >> Array.toList
    return
        { Results = matches |> Array.skip (request.Page * 20) |> Array.truncate 20
          TotalTransactions = Some matches.Length
          Page = request.Page
          Facets =
            { Towns = matches |> getFacets (fun m -> Some m.Address.TownCity)
              Localities = matches |> getFacets (fun m -> m.Address.Locality)
              Districts = matches |> getFacets (fun m -> Some m.Address.District)
              Counties = matches |> getFacets (fun m -> Some m.Address.County)
              Prices = [] } }
    }

let suggest request = task {
    let terms = Regex.Split(request.Text.ToLower(), "\s+") |> Array.filter ((<>) "") |> Array.distinct
    let termMatch (s:string) = terms |> Array.exists (fun t -> s.ToLower().Contains t)
    let suggestions =
        loadData()
        |> Seq.collect (fun x ->
            let add = x.Address
            [| add.FirstLine
               add.Locality |> Option.defaultValue ""
               add.TownCity
               add.District
               add.County
               add.PostCode |> Option.defaultValue "" |]
            |> Array.filter termMatch)
        |> Seq.truncate 20
        |> Seq.toArray
    return { Suggestions = suggestions } }

let saveData (transactions:(PropertyResult * _) seq) =
    let json = Thoth.Json.Net.Encode.Auto.toString(4, transactions |> Seq.map fst)
    System.IO.File.WriteAllText(__SOURCE_DIRECTORY__ + "properties.json", json)
    Task.FromResult()

let searcher = 
    { new Search.ISearch with
        member __.GenericSearch request = findGeneric request
        member __.PostcodeSearch request = findByPostcode request
        member __.Suggest request = suggest request
        member __.Upload report properties = saveData properties
        member __.Documents() = Task.FromResult 0L
        member __.Clear() = () }