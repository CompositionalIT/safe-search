module SafeSearch.Update

open Elmish
open Elmish.Toastr
open Fable.PowerPack.Fetch
open Thoth.Json

let printNumber (n:int64) =
    let chars =
        n.ToString()
        |> Seq.toList
        |> Seq.rev
        |> Seq.chunkBySize 3
        |> Seq.map (Seq.toArray >> System.String)
        |> String.concat ","
        |> Seq.rev
        |> Seq.toArray
    System.String(chars)

module Server =
    let searchResponseDecoder = Decode.Auto.generateDecoder<PropertyResult>()
    let loadProperties uri page sort =
        let uri =
            let uri = sprintf "/api/%s/%i" uri page
            match sort with
            | { SortColumn = Some column; SortDirection = Some direction } -> sprintf "%s?SortColumn=%s&SortDirection=%O" uri column direction
            | { SortColumn = Some column; SortDirection = None } -> sprintf "%s?SortColumn=%s" uri column
            | _ -> uri        

        Cmd.ofPromise (fetchAs uri (Decode.array searchResponseDecoder)) [] (FoundProperties >> SearchMsg) ErrorOccurred

    let loadPropertiesNormal = sprintf "property/find/%s" >> loadProperties
    let loadPropertiesLocation (postcode, distance) = sprintf "property/%s/%d" postcode distance |> loadProperties

    let loadAllStats =
        let loadStats (index:IndexName) = Cmd.ofPromise (fetchAs (sprintf "/api/%s/stats" index.Endpoint) (Decode.Auto.generateDecoder())) [] ((fun stats -> LoadedIndexStats(index, stats)) >> IndexMsg) ErrorOccurred
        Cmd.batch [ loadStats PostcodeIndex; loadStats TransactionsIndex ]

let defaultModel =
    { Search =
        { SearchResults = [||]
          SearchMethod = Standard
          SearchText = ""
          SearchState = NoSearchText
          SelectedProperty = None
          Sorting =
            { SortDirection = Some Descending
              SortColumn = Some "Date" } }
      IndexStats = Map []
      Refreshing = false }

let init() = defaultModel, Server.loadAllStats

let updateIndexMsg msg model =
    match msg with
    | LoadIndexStats ->
        { model with Refreshing = true }, Server.loadAllStats
    | LoadedIndexStats (index, stats) ->
        { model with IndexStats = model.IndexStats |> Map.add index.Endpoint stats; Refreshing = false }, Cmd.none
    | StartIndexing index ->
        let cmd =
            Cmd.ofPromise (fetchAs (sprintf "api/%s/ingest" index.Endpoint) Decode.int64) [] ((fun rows -> StartedIndexing(index, rows)) >> IndexMsg) ErrorOccurred
        { model with IndexStats = model.IndexStats |> Map.add index.Endpoint { Status = Indexing 0; DocumentCount = 0L }; Refreshing = false }, cmd
    | StartedIndexing (index, documents) ->
        let messageCmd =
            Toastr.message (sprintf "Importing %s %s" (printNumber documents) index.Endpoint)
            |> Toastr.title "Import started!"
            |> Toastr.info
        model, Cmd.batch [ Server.loadAllStats; messageCmd ]

let updateSearchMsg msg model =
    match msg with
    | FoundProperties searchResponse ->
        { model with
            SearchResults = searchResponse
            SearchState = CanSearch }, Cmd.none
    | SetSearchText text ->
        { model with
            SearchText = text
            SearchState =
                if System.String.IsNullOrWhiteSpace text then NoSearchText
                else CanSearch }, Cmd.none
    | SetSearchMethod method ->
        { model with SearchMethod = method }, Cmd.none
    | FindProperties ->
        let cmd =
            match model.SearchMethod with
            | Standard -> Server.loadPropertiesNormal model.SearchText
            | Location -> Server.loadPropertiesLocation (model.SearchText, 1)
        let cmd = cmd 0 model.Sorting
        { model with SearchState = Searching }, cmd
    | SetSorting column ->
        let model =
            let sort =
                match model.Sorting with
                | { SortColumn = Some c; SortDirection = Some d } as s when c = column ->
                    { s with SortDirection = Some (match d with Ascending -> Descending | _ -> Ascending) }
                | _ ->
                    { SortColumn = Some column; SortDirection = Some Descending }
            { model with Sorting = sort }
        model, Cmd.ofMsg (SearchMsg FindProperties)
    | SelectProperty selectedProperty ->
        { model with SelectedProperty = Some selectedProperty }, Cmd.none
    | DeselectProperty ->
        { model with SelectedProperty = None }, Cmd.none

let update msg model =
    match msg with
    | IndexMsg msg -> updateIndexMsg msg model
    | SearchMsg msg ->
        let search, cmd = updateSearchMsg msg model.Search
        { model with Search = search }, cmd
    | ErrorOccurred e ->
        let messageCmd =
            Toastr.message e.Message
            |> Toastr.title "Error!"
            |> Toastr.error
        defaultModel, messageCmd
