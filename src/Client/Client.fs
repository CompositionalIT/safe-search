module SafeSearch.Client

open Elmish
open Elmish.React
open Elmish.Toastr

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch
open Fulma
open Fulma.FontAwesome
open SafeSearch
open Thoth.Json
open System

type SearchState = CanSearch | NoSearchText | Searching
type IndexName = PostcodeIndex | TransactionsIndex member this.Endpoint = match this with PostcodeIndex -> "postcodes" | TransactionsIndex -> "transactions"
type SearchMethod = Normal | Location

type SearchDetails =
    { SearchText : string
      SearchState : SearchState
      SearchResults: PropertyResult array 
      SearchMethod : SearchMethod     
      Sorting : Sort }

type Model =
    { Search : SearchDetails
      IndexStats : Map<string, IndexStats>      
      Refreshing : bool }

type IndexMsg = 
| LoadIndexStats
| LoadedIndexStats of IndexName * IndexStats
| StartIndexing of IndexName
| StartedIndexing of IndexName * int64

type SearchMsg =
| FindProperties
| FoundProperties of PropertyResult array
| SetSearchText of string
| SetSorting of string
| SetSearchMethod of SearchMethod

type Msg =
| IndexMsg of IndexMsg
| SearchMsg of SearchMsg
| ErrorOccurred of exn

let searchResponseDecoder = Decode.Auto.generateDecoder<PropertyResult>()
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
let defaultModel = { Search = { SearchResults = [||]; SearchMethod = Normal; SearchText = ""; SearchState = NoSearchText; Sorting = { SortDirection = None; SortColumn = None } }; IndexStats = Map []; Refreshing = false }

let init() = defaultModel, loadAllStats

let updateIndexMsg msg model =
    match msg with
    | LoadIndexStats ->
        { model with Refreshing = true }, loadAllStats
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
        model, Cmd.batch [ loadAllStats; messageCmd ]

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
            | Normal -> loadPropertiesNormal model.SearchText
            | Location -> loadPropertiesLocation (model.SearchText, 1)
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

let viewNavBar model dispatch =
    Navbar.navbar [] [
        Navbar.Brand.a [ GenericOption.Props [ Href "https://safe-stack.github.io/docs/" ] ] [
            Navbar.Item.div [] [
                img [ Src "/Images/safe_favicon.png" ]
                str "SAFE Stack"
            ]
        ]
        Navbar.End.div [] [
            let isIndexing =
                model.IndexStats
                |> Map.exists(fun _ -> function { Status = Indexing _ } -> true | _ -> false)
            let createIngestionButton index =
                Navbar.Item.div [] [
                    Button.button [
                        if isIndexing then yield Button.Disabled true
                        yield Button.Color IsInfo
                        yield Button.OnClick (fun _ -> dispatch (StartIndexing index)) ] [
                        str (sprintf "Import %s" index.Endpoint)
                    ]
                ]
            yield createIngestionButton PostcodeIndex
            yield createIngestionButton TransactionsIndex
            yield Navbar.Item.div [] [
                Button.button [
                    if model.Refreshing then yield Button.IsLoading true
                    yield Button.Color IsInfo
                    yield Button.OnClick (fun _ -> dispatch LoadIndexStats) ] [
                    str "Refresh"
                ]
            ]

            let inProgressIndex =
                model.IndexStats
                |> Map.toSeq
                |> Seq.choose(fun (index, stats) ->
                    match stats with
                    | { Status = IndexState.Indexing pc } -> Some (index, pc)
                    | _ -> None)
                |> Seq.tryHead

            match inProgressIndex with
            | Some (index, pc) ->
                yield! [
                    Navbar.Item.div [] [ str (sprintf "Importing '%s' in progress... (%d%%)" index pc) ]
                    Navbar.Item.div [] [ Icon.faIcon [ ] [ Fa.icon Fa.I.Cog; Fa.spin ] ]
                    Navbar.Item.div [] [ Progress.progress [ Progress.Color IsInfo; Progress.Value pc; Progress.Max 100 ] [ str (sprintf "%d%%" pc) ] ]
                ]
            | _ -> yield Navbar.Item.div [] [ str "Import is idle." ]
            
            let statsText =
                model.IndexStats
                |> Map.toSeq
                |> Seq.map (fun (index, stats) -> sprintf "%s %s" (printNumber stats.DocumentCount) index)
                |> String.concat " and "
                |> sprintf "%s indexed."
            yield Navbar.Item.div [] [ str statsText ]
        ]
    ]

let viewSearchPanel model dispatch =
    let makeDropDownItem searchMethod currentSearchMethod icon isActive =
        Dropdown.Item.a [ Dropdown.Item.IsActive (isActive currentSearchMethod)
                          Dropdown.Item.Props [ OnClick(fun _ -> dispatch (SetSearchMethod searchMethod)) ] ] [
            Icon.faIcon [ Icon.Size IsSmall ] [ Fa.icon icon ]
            str (sprintf "%O Search" searchMethod) ]

    Container.container [] [                
        Heading.h3 [ ] [
            Icon.faIcon [ Icon.Modifiers [ Modifier.TextColor IColor.IsInfo ] ] [ Fa.icon Fa.I.Home ]
            span [ ] [ str " SAFE Search" ]
        ]
        Heading.h5 [ Heading.IsSubtitle ] [ str "Find your unaffordable property in the UK!" ]
        Columns.columns [] [
            Column.column [ Column.Option.Width(Screen.All, Column.IsThreeFifths) ] [
                Control.div [ Control.HasIconLeft ] [
                    Input.text [ yield Input.Option.Placeholder "Search for a property here"
                                 yield Input.Option.Color IColor.IsPrimary
                                 yield Input.Value model.SearchText
                                 match model.SearchState with
                                 | Searching -> yield Input.Disabled true
                                 | NoSearchText | CanSearch -> ()
                                 yield Input.OnChange (fun e -> dispatch (SetSearchText e.Value)) ]
                    Icon.faIcon [ Icon.Size IsSmall; Icon.IsLeft ] [ Fa.icon Fa.I.Search ]                                 
                ]
            ]
            Column.column [ Column.Option.Width(Screen.All, Column.IsOneFifth) ] [
                Button.a [ yield Button.IsFullWidth
                           yield Button.Color IsPrimary
                           match model.SearchState with
                           | NoSearchText -> yield Button.Disabled true
                           | Searching -> yield Button.IsLoading true
                           | CanSearch -> ()
                           yield Button.OnClick (fun _ -> dispatch FindProperties) ] [
                               Icon.faIcon [] [ Fa.icon Fa.I.Search ]
                               span [] [ str "Search" ]
                           ]
            ]
            Column.column [ Column.Option.Width(Screen.All, Column.IsOneFifth) ] [
                Dropdown.dropdown [ Dropdown.IsHoverable ] [
                    div [] [
                        Button.button [] [
                            span [] [ str "Search Type" ]
                            Icon.faIcon [ Icon.Size IsSmall ] [ Fa.icon Fa.I.AngleDown ]
                        ]
                        Dropdown.menu [ ] [
                            Dropdown.content [] [                                
                                makeDropDownItem Normal model.SearchMethod Fa.I.Search (function | Normal -> true | _ -> false)
                                makeDropDownItem Location model.SearchMethod Fa.I.LocationArrow (function | Location -> true | _ -> false)
                            ]
                        ]
                    ]
                ]    
            ]
        ]
    ]

let viewSearchResults model dispatch =
    match model.SearchResults with
    | [||] ->
        Container.container [ ] [
            Heading.h3 [ Heading.Modifiers [ Modifier.TextAlignment(Screen.All, TextAlignment.Centered) ] ] [
                str "No results found!"]
        ]
    | results ->
        let sortableColumn title = a [ OnClick (fun _ -> dispatch (SetSorting title)) ] [ str title ]
        let withSortIcon elementBuilder name sort =
            let icon icon =
                Icon.faIcon [ Icon.Modifiers [ Modifier.TextColor IColor.IsGrey ] ] [ Fa.icon icon ]
            [ yield elementBuilder name
              match sort with
              | { SortColumn = Some c; SortDirection = Some Ascending } when c = name ->
                yield icon Fa.I.ArrowDown
              | { SortColumn = Some c; SortDirection = Some Descending } when c = name ->
                yield icon Fa.I.ArrowUp
              | _ -> () ]
        Container.container [ ] [
            Heading.h3 [] [ str "Results" ]
            Table.table [ Table.IsFullWidth; Table.IsBordered; Table.IsHoverable; Table.IsStriped ] [
                thead [] [
                    tr [] [
                        match model.SearchMethod with
                        | Normal ->
                            yield th [ Style [ Width "1px" ] ] (withSortIcon sortableColumn "Date" model.Sorting)
                            yield th [ Style [ Width "1px" ] ] (withSortIcon sortableColumn "Price" model.Sorting)
                            yield th [] [ str "County" ]
                            yield th [] (withSortIcon sortableColumn "Town" model.Sorting)
                            yield th [] (withSortIcon sortableColumn "Street" model.Sorting)
                            yield th [] [ str "Postcode" ]
                        | Location ->
                            yield th [ Style [ Width "1px" ] ] [ str "Date" ]
                            yield th [ Style [ Width "1px" ] ] [ str "Price" ]
                            yield th [] [ str "County" ]
                            yield th [] [ str "Town" ]
                            yield th [] [ str "Street" ]
                            yield th [] [ str "Postcode" ]
                    ]
                ]
                tbody [] [
                    for result in results ->
                        tr [] [
                            td [] [ str (result.DateOfTransfer.Date.ToShortDateString()) ]
                            td [ Style [ TextAlign "right" ] ] [ str (sprintf "£%d" result.Price) ]
                            td [ Style [ WhiteSpace "nowrap" ] ] [ str result.Address.County ]
                            td [ Style [ WhiteSpace "nowrap" ] ] [ str result.Address.TownCity ]
                            td [ Style [ WhiteSpace "nowrap" ] ] [ result.Address.Street |> Option.defaultValue "" |> str ]
                            td [ Style [ WhiteSpace "nowrap" ] ] [ result.Address.PostCode |> Option.defaultValue "" |> str ]
                        ]
                ]
            ]
        ]

let view model dispatch =
    div [] [
        yield viewNavBar model (IndexMsg >> dispatch)
        let searchPanelOpts =
            match model.Search.SearchResults with
            | [||] -> [ Section.IsLarge ]
            | _ -> []
        yield Section.section searchPanelOpts [ viewSearchPanel model.Search (SearchMsg >> dispatch) ]
        yield section [] [ viewSearchResults model.Search (SearchMsg >> dispatch) ]
    ]
    
#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
