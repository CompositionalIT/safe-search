module SafeSearch.Update

open Elmish
open Elmish.Toastr
open Fable.PowerPack.Fetch
open Thoth.Json
open Thoth.Elmish
open System

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
    let private standardResponseDecoder = Decode.Auto.generateDecoder<PropertyResult array>()
    let private locationResponseDecoder = Decode.Auto.generateDecoder<Result<Geo * (PropertyResult array), ServerError>>()
    let private loadProperties decoder (onSuccess:_ -> Result<SearchResultType, ServerError>) uri page sort  =
        let uri =
            let uri = sprintf "/api/search/%s/%i" uri page
            match sort with
            | { SortColumn = Some column; SortDirection = Some direction } -> sprintf "%s?SortColumn=%s&SortDirection=%O" uri column direction
            | { SortColumn = Some column; SortDirection = None } -> sprintf "%s?SortColumn=%s" uri column
            | _ -> uri        
        let handleResult = function Ok msg -> FoundProperties msg | Error msg -> FoundFailed msg
        Cmd.ofPromise (fetchAs uri decoder) [] (onSuccess >> handleResult >> SearchMsg) ErrorOccurred

    let loadPropertiesNormal = sprintf "standard/%s" >> loadProperties standardResponseDecoder (StandardResults >> Ok)
    let loadPropertiesLocation (postcode, distance, view) =
        sprintf "geo/%s/%d" postcode distance
        |> loadProperties locationResponseDecoder (Result.map(fun (geo, results) -> LocationResults(results, geo, view)))
    let loadAllStats =
        let loadStats (index:IndexName) =
            Cmd.ofPromise
                (fetchAs (sprintf "/api/%s/stats" index.Endpoint) (Decode.Auto.generateDecoder()))
                []
                ((fun stats -> LoadedIndexStats(index, stats)) >> IndexMsg)
                ErrorOccurred
        Cmd.batch [ loadStats PostcodeIndex; loadStats TransactionsIndex ]
    let getSuggestions text =
        Cmd.ofPromise
            (fetchAs (sprintf "/api/search/suggest/%s" text) (Decode.Auto.generateDecoder()))
            []
            (FetchedSuggestions >> SearchTextMsg >> SearchMsg)
            ErrorOccurred

let defaultModel =
    { Search =
        { SearchResults = StandardResults [||]
          SelectedSearchMethod = Standard
          SearchText = ""
          SearchState = NoSearchText
          SelectedProperty = None
          FindFailure = None
          Suggestions = [||]
          Sorting =
            { SortDirection = Some Descending
              SortColumn = Some "Date" }
          Debouncer = Debouncer.create() }
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
            Cmd.ofPromise (fetchAs (sprintf "api/%s/import" index.Endpoint) Decode.int64) [] ((fun rows -> StartedIndexing(index, rows)) >> IndexMsg) ErrorOccurred
        { model with IndexStats = model.IndexStats |> Map.add index.Endpoint { Status = Indexing 0; DocumentCount = 0L }; Refreshing = false }, cmd
    | StartedIndexing (index, documents) ->
        let messageCmd =
            Toastr.message (sprintf "Importing %s %s" (printNumber documents) index.Endpoint)
            |> Toastr.title "Import started!"
            |> Toastr.info
        model, Cmd.batch [ Server.loadAllStats; messageCmd ]

let updateSearchTextMsg msg model =
    match msg with
    | SetSearchText text ->
        let debouncerModel, debouncerCmd =
            model.Debouncer |> Debouncer.bounce (TimeSpan.FromSeconds 1.0) "user_input" FetchSuggestions
        { model with
            SearchText = text
            Suggestions = [||]
            FindFailure = None
            SearchState =
                if System.String.IsNullOrWhiteSpace text then NoSearchText
                else CanSearch
            Debouncer = debouncerModel },
            Cmd.map DebouncerSelfMsg debouncerCmd |> Cmd.map (SearchTextMsg >> SearchMsg)        
    | DebouncerSelfMsg debouncerMsg ->
        let debouncerModel, debouncerCmd = Debouncer.update debouncerMsg model.Debouncer
        { model with Debouncer = debouncerModel }, debouncerCmd |> Cmd.map (SearchTextMsg >> SearchMsg)
    | FetchSuggestions ->
        if not (String.IsNullOrWhiteSpace model.SearchText) then
            model, (Server.getSuggestions model.SearchText)
        else model, Cmd.none        
    | FetchedSuggestions response ->
        match model.SelectedSearchMethod with
        | Standard -> { model with Suggestions = response.Suggestions }, Cmd.none
        | Location -> model, Cmd.none
    | ClearSuggestions ->
        { model with Suggestions = [||] }, Cmd.none
    | SetTextAndSearch(text, searchMethod) ->
        { model with
            SearchText =
                match searchMethod with
                | Location -> text
                | Standard -> sprintf "\"%s\"" text
            Suggestions = [||]
            SearchState =
                if System.String.IsNullOrWhiteSpace text then NoSearchText
                else CanSearch
            SelectedSearchMethod = searchMethod }, Cmd.ofMsg (SearchMsg FindProperties)

let updateSearchMsg msg model =
    match msg with
    | FindProperties ->
        let cmd =
            match model.SelectedSearchMethod with
            | Standard -> Server.loadPropertiesNormal model.SearchText
            | Location -> Server.loadPropertiesLocation (model.SearchText, 1, model.SearchResults.CurrentView)
        let cmd = cmd 0 model.Sorting
        { model with
            Suggestions = [||]
            SearchState = Searching }, cmd
    | FoundProperties results ->
        { model with
            SearchResults = results
            SearchState = CanSearch }, Cmd.none
    | FoundFailed msg ->
        { model with
            SearchState = CanSearch
            FindFailure = Some msg }, Cmd.none
    | SearchTextMsg msg ->
        updateSearchTextMsg msg model
    | SetSearchMethod method ->
        { model with SelectedSearchMethod = method }, Cmd.ofMsg (SearchMsg(SearchTextMsg ClearSuggestions))
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
    | ChangeView view ->
        match model.SearchResults with
        | StandardResults _ -> model, Cmd.none
        | LocationResults (props, geo, _) ->
            { model with SearchResults = LocationResults(props, geo, view) }, Cmd.none
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
