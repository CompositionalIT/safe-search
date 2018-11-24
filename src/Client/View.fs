module SafeSearch.View

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fulma
open Fulma.FontAwesome

let createNavBar model dispatch =
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
            let createImportButton index =
                Navbar.Item.div [] [
                    Button.button [
                        if isIndexing then yield Button.Disabled true
                        yield Button.Color IsInfo
                        yield Button.OnClick (fun _ -> dispatch (StartIndexing index)) ] [
                        str (sprintf "Import %s" index.Endpoint)
                    ]
                ]
            yield createImportButton PostcodeIndex
            yield createImportButton TransactionsIndex
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

let createSearchPanel model dispatch =
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
                yield Control.div [ Control.HasIconLeft ] [
                    Input.text [
                        yield Input.Option.Placeholder "Enter your search term here."
                        match model.SearchError, model.SearchState with
                        | Some _, _ | None, CannotSearch InvalidPostcode -> yield Input.Option.Color IColor.IsDanger
                        | None, (Searching | CannotSearch NoSearchText | CanSearch) -> yield Input.Option.Color IColor.IsPrimary
                        yield Input.Value model.SearchText
                        match model.SearchState with
                        | Searching -> yield Input.Disabled true
                        | CannotSearch _ | CanSearch -> ()
                        yield Input.OnChange (fun e -> dispatch (SearchTextMsg(SetSearchText(e.Value, UserAction)))) ]
                    Icon.faIcon [ Icon.Size IsSmall; Icon.IsLeft ] [ Fa.icon Fa.I.Search ] 
                ]
                yield Dropdown.dropdown [ Dropdown.IsActive (not (Array.isEmpty model.Suggestions)) ] [
                    Dropdown.menu [] [
                        Dropdown.content [] [
                            for suggestion in model.Suggestions do
                                yield Dropdown.Item.a [
                                    Dropdown.Item.Props [
                                        OnClick(fun _ ->
                                            dispatch (SearchTextMsg(SetSearchText (sprintf "\"%s\"" suggestion, SystemAction)))
                                            dispatch StartSearch)
                                    ]
                                ] [ str suggestion ]
                        ]
                    ]                               
                ]
                match model.SearchError, model.SearchState with
                | Some searchError, _ ->
                    yield Help.help [ Help.Color IsDanger ] [
                        match searchError with
                        | NoGeolocation postcode -> yield str (sprintf "Unable to locate geolocation details for postcode '%s'." postcode)
                    ]
                | None, (CannotSearch NoSearchText | CanSearch) ->
                    yield Help.help [ Help.Color IsInfo ] [
                        match model.SelectedSearchMethod with
                        | Standard -> yield str "Search for a property by street, town, postcode or district e.g. 'Tottenham'."
                        | Location -> yield str "Enter a postcode to search by location e.g. 'EC2A 4NE'"
                    ]
                | None, Searching ->
                    yield Help.help [ Help.Color IsInfo ] [
                        str "Searching, please wait..."
                    ]
                | None, CannotSearch InvalidPostcode ->
                    yield Help.help [ Help.Color IsDanger ] [
                        str "This is not a valid postcode."
                    ]
            ]
            Column.column [ Column.Option.Width(Screen.All, Column.IsOneFifth) ] [
                Button.a [ yield Button.IsFullWidth
                           yield Button.Color IsPrimary
                           match model.SearchState with
                           | CannotSearch NoSearchText -> yield Button.Disabled true
                           | CannotSearch InvalidPostcode -> yield Button.Disabled true
                           | Searching -> yield Button.IsLoading true
                           | CanSearch -> ()
                           yield Button.OnClick (fun _ -> dispatch StartSearch) ] [
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

let makeMap (lat, long, originMarker) container markers zoomLevel =
    printfn "origin %A" (lat, long)
    let center = Fable.Helpers.GoogleMaps.Literal.createLatLng lat long

    Box.box' [] [
        googleMap [ 
            MapProperties.MapLoadingContainer "maploadercontainer"
            MapProperties.MapContainer container
            MapProperties.DefaultCenter !^ center
            MapProperties.Center !^ center
            MapProperties.DefaultZoom zoomLevel
            MapProperties.Markers (Array.append [| (originMarker center) |] markers)
        ]
    ]

let createSearchResults model dispatch =
    match model.SearchResults.Response with
    | { Results = [||] } ->
        Container.container [ ] [
            Heading.h3 [ Heading.Modifiers [ Modifier.TextAlignment(Screen.All, TextAlignment.Centered) ] ] [
                str "No results found!"]
        ]
    | response ->
        let sortableColumn title = a [ OnClick (fun _ -> dispatch (SetSorting title)) ] [ str title ]
        let maybeSortableColumn searchMethod basicBuilder sortableBuilder name sort =
            let icon icon =
                Icon.faIcon [ Icon.Modifiers [ Modifier.TextColor IColor.IsGrey ] ] [ Fa.icon icon ]
            [ match searchMethod with
              | StandardResponse _ ->
                  yield sortableBuilder name
                  match sort with
                  | { SortColumn = Some c; SortDirection = Some Ascending } when c = name -> yield icon Fa.I.ArrowDown
                  | { SortColumn = Some c; SortDirection = Some Descending } when c = name -> yield icon Fa.I.ArrowUp
                  | _ -> ()
              | LocationResponse _ -> yield basicBuilder name ]
        Container.container [ ] [
            yield Heading.h3 [] [ str "Results" ]
            yield Tabs.tabs [ Tabs.Size IsLarge; Tabs.IsBoxed ] [
                let makeTab active icon text viewType =
                    Tabs.tab [ Tabs.Tab.IsActive active ] [ a [ OnClick(fun _ -> dispatch (ChangeView viewType)) ] [ Icon.faIcon [ Icon.Modifiers [ ] ] [ Fa.icon icon ]; str text ] ]
                yield makeTab (model.SearchResults.CurrentView = ResultsList) Fa.I.List "List" ResultsList
                match model.SearchResults with
                | LocationResponse _ -> yield makeTab (model.SearchResults.CurrentView = ResultsMap) Fa.I.Map "Map" ResultsMap
                | StandardResponse _ -> ()
            ]

            match model.SearchResults with
            | LocationResponse(_, _, ResultsList) | StandardResponse _ ->
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
                        for result in response.Results ->
                            tr [] [
                                yield td [] [
                                    Icon.faIcon [
                                        Icon.Option.Props [
                                            OnClick(fun _ -> dispatch (SelectProperty result))
                                            Style [ Cursor "pointer" ]
                                        ]
                                        Icon.Modifiers [ Modifier.TextColor IColor.IsInfo ] ] [
                                        Fa.icon Fa.I.InfoCircle ]
                                ]
                                yield td [] [ str (result.DateOfTransfer.Date.ToShortDateString()) ]
                                yield td [ Style [ TextAlign "right" ] ] [ str (asCurrency result.Price) ]
                                yield td [ Style [ WhiteSpace "nowrap" ] ] [ result.Address.Street |> Option.defaultValue "" |> str ]
                                yield td [ Style [ WhiteSpace "nowrap" ] ] [ str result.Address.TownCity ]
                                yield td [ Style [ WhiteSpace "nowrap" ] ] [ str result.Address.County ]
                                match result.Address.PostCode with
                                | Some postcode ->
                                    yield
                                        td [ Style [ WhiteSpace "nowrap" ] ]
                                           [ a [ OnClick(fun _ ->
                                                    dispatch (SetSearchMethod Location)
                                                    dispatch (SearchTextMsg(SetSearchText(postcode, SystemAction)))
                                                    dispatch StartSearch
                                                ) ] [ str postcode ]
                                           ]
                                | None ->
                                    yield td [] []
                            ]
                    ]
                ]
            | LocationResponse(response, originGeo, ResultsMap) ->
                let results =
                    response.Results
                    |> Array.distinctBy(fun r -> r.Address.PostCode)
                    |> Array.choose(fun r -> r.Address.GeoLocation |> Option.map(fun geo -> geo, r))
                let markers =
                    results
                    |> Array.mapi(fun i (geo, result) ->
                        let markerText =
                            match result.Address with
                            | { Street = Some street; PostCode = Some postcode } -> sprintf "%s %s" street postcode
                            | { Street = Some street } -> street
                            | _ -> result.Address.Building

                        marker [
                            MarkerProperties.Key (string result.Address.PostCode)
                            MarkerProperties.Position !^ (Fable.Helpers.GoogleMaps.Literal.createLatLng geo.Lat geo.Long)
                            MarkerProperties.Icon ("images/house.png")
                            MarkerProperties.Title (sprintf "%d. %s (%s)" (i + 1) markerText (result.Price |> asCurrency)) ] [])

                let originMarker location =
                    marker [
                        MarkerProperties.Key "YOUAREHERE"
                        MarkerProperties.Position !^ location 
                        MarkerProperties.Icon ("images/you.png")
                        MarkerProperties.Title (sprintf "You are here") ] []
                yield makeMap (originGeo.Lat, originGeo.Long, originMarker) "largeMapcontainer" markers 16
        ]
        
let createPropertyPopup (propertyResult:PropertyResult) closeModal =
    let propField label values = 
        Field.div [ Field.IsHorizontal ] [
            Field.label [ Field.Label.IsNormal ] [ Label.label [ ] [ str label ] ]
            Field.body [] [
                for value in values |> List.choose id |> List.distinct do
                    yield Field.div [] [
                        Input.text [ Input.IsReadOnly true; Input.Option.Value value ]
                    ]
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
                    propField "Street" [
                        Some propertyResult.Address.Building
                        propertyResult.Address.Street
                    ]
                    propField "Town" [
                        propertyResult.Address.Locality
                        Some propertyResult.Address.TownCity
                        Some propertyResult.Address.District
                    ]
                    propField "Region" [
                        Some propertyResult.Address.County
                        propertyResult.Address.PostCode ]
                    propField "Date & Price" [
                        Some (propertyResult.DateOfTransfer.ToString("ddd/MM/yyyy"))
                        Some (propertyResult.Price |> asCurrency)
                    ]
                    propField "Build" [
                        Some (string propertyResult.BuildDetails.Contract.Description)
                        propertyResult.BuildDetails.PropertyType |> Option.map(fun p -> p.Description)
                    ]
                ]
                Section.section [] [
                    match propertyResult.Address.GeoLocation with
                    | Some geo ->
                        let originMarker location =
                            marker [
                                MarkerProperties.Key "PROPERTY"
                                MarkerProperties.Position !^ location 
                                MarkerProperties.Icon ("images/house.png") ] []
                        yield makeMap (geo.Lat, geo.Long, originMarker) "mapcontainer" Array.empty 17
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
        yield createNavBar model (IndexMsg >> dispatch)
        let searchPanelOpts =
            match model.Search.SearchResults.Response.Results with
            | [||] -> [ Section.IsLarge ]
            | _ -> []
        yield Section.section searchPanelOpts [ createSearchPanel model.Search (SearchMsg >> dispatch) ]
        yield section [] [ createSearchResults model.Search (SearchMsg >> dispatch) ]
        match model.Search.SelectedProperty with
        | Some selectedProperty -> yield createPropertyPopup selectedProperty (fun _ -> dispatch (SearchMsg DeselectProperty))
        | None -> ()
    ]