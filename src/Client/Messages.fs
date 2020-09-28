namespace SafeSearch

type InvalidSearch =
    | NoSearchText
    | InvalidPostcode

type SearchState =
    | CanSearch
    | CannotSearch of InvalidSearch
    | Searching

type IndexName =
    | PostcodeIndex
    | TransactionsIndex
    member this.Endpoint =
        match this with
        | PostcodeIndex -> "postcodes"
        | TransactionsIndex -> "transactions"

type SearchMethod =
    | Standard
    | Location

type ResultsView =
    | ResultsList
    | ResultsMap

type SearchResultType =
    | StandardResponse of SearchResponse
    | LocationResponse of SearchResponse * Geo * ResultsView

    member this.CurrentView =
        match this with
        | StandardResponse _ -> ResultsList
        | LocationResponse(_, _, view) -> view

    member this.Response =
        match this with
        | StandardResponse r -> r
        | LocationResponse(r, _, _) -> r

type SearchDetails =
    { SearchText : string
      SearchState : SearchState
      SearchResults : SearchResultType
      SearchError : SearchError option
      SelectedSearchMethod : SearchMethod
      SelectedProperty : PropertyResult option
      Sorting : Sort
      SelectedFacets : Map<string, string * string>
      IsTextDirty : bool
      Suggestions : string array
      GoogleMapsKey : string option }

type Model =
    { Search : SearchDetails
      IndexStats : Map<string, IndexStats>
      Refreshing : bool }

type TextChangeSource =
    | UserAction
    | SystemAction

type SearchTextMsg =
    | SetSearchText of string * TextChangeSource
    | FetchSuggestions
    | FetchedSuggestions of SuggestResponse
    | ValidateSearchText
    | ClearSuggestions

type SearchMsg =
    | StartSearch
    | SearchComplete of SearchResultType
    | SetSorting of string
    | SetSearchMethod of SearchMethod
    | SearchTextMsg of SearchTextMsg
    | SetFacet of Facet : string * Value : string * Description : string
    | RemoveFacet of facet : string
    | SelectProperty of PropertyResult
    | DeselectProperty
    | ChangeView of ResultsView
    | FoundFailed of SearchError
    | LoadedConfig of string option

type Msg =
    | SearchMsg of SearchMsg
    | ErrorOccurred of exn
