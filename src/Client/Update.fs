module SafeSearch.Update

open Elmish
open Elmish.Toastr
open Fable.PowerPack.Fetch
open System
open Thoth.Elmish
open Thoth.Json

let printNumber (n : int64) =
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
    let private standardResponseDecoder =
        Decode.Auto.generateDecoder<SearchResponse>()
    let private locationResponseDecoder =
        Decode.Auto.generateDecoder<Result<Geo * SearchResponse, SearchError>>()

    let private loadProperties decoder (onSuccess : _ -> Result<_, _>) uri page
        sort selectedFacets =
        let uri =
            let uri = sprintf "/api/search/%s/%i?" uri page

            let uri =
                match sort with
                | { SortColumn = Some column; SortDirection = Some direction } ->
                    sprintf "%sSortColumn=%s&SortDirection=%O" uri column
                        direction
                | { SortColumn = Some column; SortDirection = None } ->
                    sprintf "%sSortColumn=%s" uri column
                | _ -> uri
            (uri, selectedFacets)
            ||> Seq.fold
                    (fun uri (facetName, (facetValue, _)) ->
                    sprintf "%s&%s=%s" uri facetName facetValue)

        let handleResult =
            function
            | Ok msg -> SearchComplete msg
            | Error msg -> FoundFailed msg

        Cmd.ofPromise (fetchAs uri decoder) [] (onSuccess
                                                >> handleResult
                                                >> SearchMsg) ErrorOccurred

    let loadPropertiesNormal =
        sprintf "standard/%s"
        >> loadProperties standardResponseDecoder (StandardResponse >> Ok)
    let loadPropertiesLocation (postcode, distance, view) =
        sprintf "geo/%s/%d" postcode distance
        |> loadProperties locationResponseDecoder
               (Result.map
                    (fun (geo, results) -> LocationResponse(results, geo, view)))

    let loadAllStats =
        let loadStats (index : IndexName) =
            Cmd.ofPromise
                (fetchAs (sprintf "/api/%s/stats" index.Endpoint)
                     (Decode.Auto.generateDecoder())) []
                ((fun stats -> LoadedIndexStats(index, stats)) >> IndexMsg)
                ErrorOccurred
        Cmd.batch [ loadStats PostcodeIndex
                    loadStats TransactionsIndex ]

    let loadGoogleKey =
        Cmd.ofPromise
            (fetchAs "/api/search/config" (Decode.option Decode.string)) []
            (LoadedConfig >> SearchMsg) ErrorOccurred

    let getSuggestions text =
        Cmd.ofPromise
            (fetchAs (sprintf "/api/search/suggest/%s" text)
                 (Decode.Auto.generateDecoder())) [] (FetchedSuggestions
                                                      >> SearchTextMsg
                                                      >> SearchMsg)
            ErrorOccurred

let defaultModel =
    { Search =
          { SearchResults = StandardResponse SearchResponse.Empty
            SelectedSearchMethod = Standard
            SearchText = ""
            SearchState = CannotSearch NoSearchText
            SelectedProperty = None
            SearchError = None
            Suggestions = [||]
            SelectedFacets = Map.empty
            IsTextDirty = false
            GoogleMapsKey = None
            Sorting =
                { SortDirection = Some Descending
                  SortColumn = Some "Date" }
            Debouncer = Debouncer.create() }
      IndexStats = Map []
      Refreshing = false }

let init() =
    defaultModel, Cmd.batch [ Server.loadAllStats; Server.loadGoogleKey ]

let updateIndexMsg msg model =
    match msg with
    | LoadIndexStats -> { model with Refreshing = true }, Server.loadAllStats
    | LoadedIndexStats(index, stats) ->
        { model with IndexStats =
                         model.IndexStats |> Map.add index.Endpoint stats
                     Refreshing = false }, Cmd.none
    | StartIndexing index ->
        let cmd =
            Cmd.ofPromise
                (fetchAs (sprintf "api/%s/import" index.Endpoint) Decode.int64)
                [] ((fun rows -> StartedIndexing(index, rows)) >> IndexMsg)
                ErrorOccurred
        { model with IndexStats =
                         model.IndexStats
                         |> Map.add index.Endpoint { Status = Indexing 0
                                                     DocumentCount = 0L }
                     Refreshing = false }, cmd
    | StartedIndexing(index, documents) ->
        let messageCmd =
            Toastr.message
                (sprintf "Importing %s %s" (printNumber documents)
                     index.Endpoint)
            |> Toastr.title "Import started!"
            |> Toastr.info
        model, Cmd.batch [ Server.loadAllStats; messageCmd ]

let private validateSearchTextMsg =
    Cmd.ofMsg (ValidateSearchText
               |> SearchTextMsg
               |> SearchMsg)

let updateSearchTextMsg msg (model : SearchDetails) =
    match msg, model.SelectedSearchMethod with
    | SetSearchText(text, UserAction), _ ->
        let debouncerModel, debouncerCmd =
            model.Debouncer
            |> Debouncer.bounce (TimeSpan.FromSeconds 1.0) "user_input"
                   FetchSuggestions

        let model =
            { model with SearchText = text
                         Suggestions = [||]
                         SearchError = None
                         IsTextDirty = false
                         Debouncer = debouncerModel }

        let commands =
            Cmd.batch
                [ validateSearchTextMsg

                  Cmd.map DebouncerSelfMsg debouncerCmd
                  |> Cmd.map (SearchTextMsg >> SearchMsg) ]

        model, commands
    | SetSearchText(text, SystemAction), _ ->
        { model with SearchText = text
                     Suggestions = [||]
                     SearchError = None
                     IsTextDirty = true }, validateSearchTextMsg
    | DebouncerSelfMsg debouncerMsg, _ ->
        let debouncerModel, debouncerCmd =
            Debouncer.update debouncerMsg model.Debouncer
        { model with Debouncer = debouncerModel },
        debouncerCmd |> Cmd.map (SearchTextMsg >> SearchMsg)
    | FetchSuggestions, _ ->
        if not (String.IsNullOrWhiteSpace model.SearchText) then
            model, (Server.getSuggestions model.SearchText)
        else model, Cmd.none
    | FetchedSuggestions response, Standard ->
        { model with Suggestions = response.Suggestions }, Cmd.none
    | FetchedSuggestions _, Location ->
        model, Cmd.none
    | ClearSuggestions, _ ->
        { model with Suggestions = [||] }, Cmd.none
    | ValidateSearchText, _ ->
        let searchState =
            match model.SelectedSearchMethod with
            | _ when System.String.IsNullOrWhiteSpace model.SearchText ->
                CannotSearch NoSearchText
            | Location when not (Validation.isValidPostcode model.SearchText) ->
                CannotSearch InvalidPostcode
            | _ -> CanSearch
        { model with SearchState = searchState }, Cmd.none

let updateSearchMsg msg model =
    match msg with
    | StartSearch ->
        match Option.ofString model.SearchText with
        | Some text ->
            let cmd =
                let cmd =
                    match model.SelectedSearchMethod with
                    | Standard -> Server.loadPropertiesNormal text
                    | Location ->
                        Server.loadPropertiesLocation
                            (model.SearchText, 1,
                             model.SearchResults.CurrentView)
                cmd 0 model.Sorting (Map.toSeq model.SelectedFacets)
            { model with Suggestions = [||]
                         SearchState = Searching }, cmd
        | None -> model, Cmd.none
    | SearchComplete results ->
        { model with SearchResults = results
                     SearchState = CanSearch
                     IsTextDirty = false },
        Cmd.ofMsg (SearchMsg(SearchTextMsg ClearSuggestions))
    | FoundFailed ((NoGeolocation postcode) as searchError) ->
        let cmd =
            Toastr.message (sprintf "Unable to locate geolocation details for postcode '%s'." postcode)
            |> Toastr.showCloseButton
            |> Toastr.title "Search Error"
            |> Toastr.error

        { model with SearchState = CanSearch
                     SearchError = Some searchError }, cmd
    | SearchTextMsg msg ->
        updateSearchTextMsg msg model
    | SetSearchMethod method ->
        { model with SelectedSearchMethod = method },
        Cmd.batch [ validateSearchTextMsg
                    Cmd.ofMsg (SearchMsg(SearchTextMsg ClearSuggestions)) ]
    | SetSorting column ->
        let model =
            let sort =
                match model.Sorting with
                | { SortColumn = Some c; SortDirection = Some d } as s when c = column ->
                    { s with SortDirection =
                                 Some(match d with
                                      | Ascending -> Descending
                                      | _ -> Ascending) }
                | _ ->
                    { SortColumn = Some column
                      SortDirection = Some Descending }
            { model with Sorting = sort }
        model, Cmd.ofMsg (SearchMsg StartSearch)
    | SelectProperty selectedProperty ->
        { model with SelectedProperty = Some selectedProperty }, Cmd.none
    | DeselectProperty ->
        { model with SelectedProperty = None }, Cmd.none
    | ChangeView view ->
        match model.SearchResults with
        | StandardResponse _ -> model, Cmd.none
        | LocationResponse(props, geo, _) ->
            { model with SearchResults = LocationResponse(props, geo, view) },
            Cmd.none
    | LoadedConfig apiKey ->
        { model with GoogleMapsKey = apiKey }, Cmd.none
    | SetFacet(facet, value, description) ->
        { model with SelectedFacets =
                         model.SelectedFacets.Add(facet, (value, description)) },
        Cmd.ofMsg (SearchMsg StartSearch)
    | RemoveFacet facet ->
        { model with SelectedFacets = model.SelectedFacets.Remove facet },
        Cmd.ofMsg (SearchMsg StartSearch)

let update msg model =
    match msg with
    | IndexMsg msg ->
        updateIndexMsg msg model
    | SearchMsg msg ->
        let search, cmd = updateSearchMsg msg model.Search
        { model with Search = search }, cmd
    | ErrorOccurred e ->
        let messageCmd =
            Toastr.message e.Message
            |> Toastr.title "Error!"
            |> Toastr.error
        defaultModel, messageCmd
