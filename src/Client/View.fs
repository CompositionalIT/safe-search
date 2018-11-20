module SafeSearch.View

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fulma
open Fulma.FontAwesome

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
                |> Seq.map (fun (index, stats) -> sprintf "%s %s" (Update.printNumber stats.DocumentCount) index)
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
                    Input.text [
                        yield Input.Option.Placeholder "Enter your search term here."
                        yield Input.Option.Color IColor.IsPrimary
                        yield Input.Value model.SearchText
                        match model.SearchState with
                        | Searching -> yield Input.Disabled true
                        | NoSearchText | CanSearch -> ()
                        yield Input.OnChange (fun e -> dispatch (SetSearchText e.Value)) ]
                    Icon.faIcon [ Icon.Size IsSmall; Icon.IsLeft ] [ Fa.icon Fa.I.Search ]                                 
                ]
                Help.help [ Help.Color IsInfo ] [
                    match model.SelectedSearchMethod with
                    | Standard -> yield str "Search for a property by street, town, postcode or district e.g. 'Tottenham'."
                    | Location -> yield str "Enter a postcode to search by location e.g. 'EC2A 4NE'"
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
                            span [] [
                                let icon = match model.SelectedSearchMethod with Standard -> Fa.I.Search | Location -> Fa.I.LocationArrow
                                yield Icon.faIcon [ Icon.Size IsSmall ] [ Fa.icon icon ]
                                yield span [] [ str (sprintf "%O Search" model.SelectedSearchMethod) ]
                            ]
                            Icon.faIcon [ Icon.Size IsSmall ] [ Fa.icon Fa.I.AngleDown ]
                        ]
                        Dropdown.menu [ ] [
                            Dropdown.content [] [                                
                                makeDropDownItem Standard model.SelectedSearchMethod Fa.I.Search (function Standard -> true | _ -> false)
                                makeDropDownItem Location model.SelectedSearchMethod Fa.I.LocationArrow (function Location -> true | _ -> false)
                            ]
                        ]
                    ]
                ]    
            ]
        ]
    ]


open Fable.Core.JsInterop
open Fable.Helpers.ReactGoogleMaps
open Fable.Helpers.ReactGoogleMaps.Props

let asCurrency = int64 >> Update.printNumber >> sprintf "£%s"

let makeMap long lat container markers zoomLevel =
    let center = Fable.Helpers.GoogleMaps.Literal.createLatLng long lat
    Box.box' [] [
        googleMap [ 
            MapProperties.MapLoadingContainer "maploadercontainer"
            MapProperties.MapContainer container
            MapProperties.DefaultCenter !^ center
            MapProperties.Center !^ center
            MapProperties.DefaultZoom zoomLevel
            MapProperties.Markers markers
        ]
    ]

let viewSearchResults model dispatch =
    match model.SearchResults.Results with
    | [||] ->
        Container.container [ ] [
            Heading.h3 [ Heading.Modifiers [ Modifier.TextAlignment(Screen.All, TextAlignment.Centered) ] ] [
                str "No results found!"]
        ]
    | results ->
        let sortableColumn title = a [ OnClick (fun _ -> dispatch (SetSorting title)) ] [ str title ]
        let maybeSortableColumn searchMethod basicBuilder sortableBuilder name sort =
            let icon icon =
                Icon.faIcon [ Icon.Modifiers [ Modifier.TextColor IColor.IsGrey ] ] [ Fa.icon icon ]
            [ match searchMethod with
              | StandardResults _ ->
                  yield sortableBuilder name
                  match sort with
                  | { SortColumn = Some c; SortDirection = Some Ascending } when c = name -> yield icon Fa.I.ArrowDown
                  | { SortColumn = Some c; SortDirection = Some Descending } when c = name -> yield icon Fa.I.ArrowUp
                  | _ -> ()
              | LocationResults _ -> yield basicBuilder name ]
        Container.container [ ] [
            yield Heading.h3 [] [ str "Results" ]
            yield Tabs.tabs [ Tabs.Size IsLarge; Tabs.IsBoxed ] [
                let makeTab active icon text viewType =
                    Tabs.tab [ Tabs.Tab.IsActive active ] [ a [ OnClick(fun _ -> dispatch (ChangeView viewType)) ] [ Icon.faIcon [ Icon.Modifiers [ Modifier.TextColor IColor.IsPrimary ] ] [ Fa.icon icon ]; str text ] ]
                yield makeTab (model.SearchResults.CurrentView = ResultsList) Fa.I.List "List" ResultsList
                match model.SearchResults with
                | LocationResults _ -> yield makeTab (model.SearchResults.CurrentView = ResultsMap) Fa.I.Map "Map" ResultsMap
                | StandardResults _ -> ()
            ]

            match model.SearchResults with
            | LocationResults(_, _, ResultsList) | StandardResults _ ->
                yield Table.table [ Table.IsFullWidth; Table.IsBordered; Table.IsHoverable; Table.IsStriped ] [
                    thead [] [
                        tr [] [
                            yield th [ Style [ Width "1px" ] ] []
                            let maybeSortableColumn = maybeSortableColumn model.SearchResults
                            yield th [ Style [ Width "1px" ] ] (maybeSortableColumn str sortableColumn "Date" model.Sorting)
                            yield th [ Style [ Width "1px" ] ] (maybeSortableColumn str sortableColumn "Price" model.Sorting)
                            yield th [] (maybeSortableColumn str sortableColumn "Street" model.Sorting)
                            yield th [] (maybeSortableColumn str sortableColumn "Town" model.Sorting)
                            yield th [] [ str "County" ]
                            yield th [] [ str "Postcode" ]
                        ]
                    ]
                    tbody [] [
                        for result in results ->
                            tr [] [
                                td [] [
                                    Icon.faIcon [
                                        Icon.Option.Props [
                                            OnClick(fun _ -> dispatch (SelectProperty result))
                                            Style [ Cursor "pointer" ]
                                        ]
                                        Icon.Modifiers [ Modifier.TextColor IColor.IsInfo ] ] [
                                        Fa.icon Fa.I.InfoCircle ]
                                ]
                                td [] [ str (result.DateOfTransfer.Date.ToShortDateString()) ]
                                td [ Style [ TextAlign "right" ] ] [ str (asCurrency result.Price) ]
                                td [ Style [ WhiteSpace "nowrap" ] ] [ result.Address.Street |> Option.defaultValue "" |> str ]
                                td [ Style [ WhiteSpace "nowrap" ] ] [ str result.Address.TownCity ]
                                td [ Style [ WhiteSpace "nowrap" ] ] [ str result.Address.County ]
                                td [ Style [ WhiteSpace "nowrap" ] ] [ result.Address.PostCode |> Option.defaultValue "" |> str ]
                            ]
                    ]
                ]
            | LocationResults(results, geo, ResultsMap) ->
                let results =
                    results
                    |> Array.distinctBy(fun r -> r.Address.PostCode)
                    |> Array.choose(fun r ->
                        r.Address.GeoLocation
                        |> Option.map(fun geo -> geo, r))
                let markers =
                    let youAreHere =
                        marker [
                            MarkerProperties.Key "YOUAREHERE"
                            MarkerProperties.Position !^ (Fable.Helpers.GoogleMaps.Literal.createLatLng geo.Lat geo.Long)
                            MarkerProperties.Icon ("images/you.png")
                            MarkerProperties.Title (sprintf "You are here") ] []

                    let properties =
                        results
                        |> Array.mapi(fun i (geo, result) ->
                            marker [
                                MarkerProperties.Key (string result.Address.PostCode)
                                MarkerProperties.Position !^ (Fable.Helpers.GoogleMaps.Literal.createLatLng geo.Lat geo.Long)
                                MarkerProperties.Icon ("images/house.png")
                                MarkerProperties.Title (sprintf "%d. %s (%s)" (i + 1) (result.Address.Street |> Option.defaultValue result.Address.Building) (result.Price |> asCurrency)) ] [])
                    Array.append [| youAreHere |] properties

                yield makeMap geo.Lat geo.Long "largeMapcontainer" markers 16
        ]
        
let viewModalProperty (propertyResult:PropertyResult) closeModal =
    let propField label values = 
        Field.div [ Field.IsHorizontal ] [
            Field.label [ Field.Label.IsNormal ] [ Label.label [ ] [ str label ] ]
            Field.body [ ] [
                for value in values do
                    yield Field.div [] [ Input.text [ Input.IsReadOnly true; Input.Option.Value (value |> Option.defaultValue "") ] ]
            ]
        ]
    Modal.modal [ Modal.IsActive true ] [
        Modal.background [ Props [ OnClick closeModal ] ] [ ]
        Modal.Card.card [ ] [
            Modal.Card.head [ ] [
                Modal.Card.title [ ] [ str "View Property Details" ]
                Delete.delete [ Delete.OnClick closeModal ] [ ]
            ]
            Modal.Card.body [] [
                form [ ] [
                    propField "Street" [ propertyResult.Address.Street ]
                    propField "Town" [
                        Some propertyResult.Address.TownCity
                        Some propertyResult.Address.County
                    ]
                    propField "Region" [
                        Some propertyResult.Address.District
                        propertyResult.Address.PostCode ]
                    propField "Date & Price" [
                        Some (propertyResult.DateOfTransfer.ToString("ddd/MM/yyyy"))
                        Some (propertyResult.Price |> asCurrency)
                    ]
                    propField "Build" [
                        Some (string propertyResult.BuildDetails.Contract)
                        propertyResult.BuildDetails.PropertyType |> Option.map string
                    ]
                ]
                Section.section [] [
                    match propertyResult.Address.GeoLocation with
                    | Some geo -> yield makeMap geo.Lat geo.Long "mapcontainer" [] 12
                    | None -> ()
                ]
            ]
            Modal.Card.foot [ ] [
                Button.button [ Button.Color IsInfo; Button.OnClick closeModal ] [
                    str "Ok"
                ]
            ]
        ]
    ]

let view model dispatch =
    div [] [
        yield viewNavBar model (IndexMsg >> dispatch)
        let searchPanelOpts =
            match model.Search.SearchResults.Results with
            | [||] -> [ Section.IsLarge ]
            | _ -> []
        yield Section.section searchPanelOpts [ viewSearchPanel model.Search (SearchMsg >> dispatch) ]
        yield section [] [ viewSearchResults model.Search (SearchMsg >> dispatch) ]
        match model.Search.SelectedProperty with
        | Some selectedProperty -> yield viewModalProperty selectedProperty (fun _ -> dispatch (SearchMsg DeselectProperty))
        | None -> ()
    ]
