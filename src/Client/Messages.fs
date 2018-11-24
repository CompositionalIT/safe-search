namespace SafeSearch

open Thoth.Elmish

type InvalidSearch = NoSearchText | InvalidPostcode
type SearchState = CanSearch | CannotSearch of InvalidSearch | Searching
type IndexName = PostcodeIndex | TransactionsIndex member this.Endpoint = match this with PostcodeIndex -> "postcodes" | TransactionsIndex -> "transactions"
type SearchMethod = Standard | Location
type ResultsView = ResultsList | ResultsMap
type SearchResultType =
  | StandardResults of PropertyResult array
  | LocationResults of PropertyResult array * Geo * ResultsView
  member this.CurrentView =
      match this with
      | StandardResults _ -> ResultsList
      | LocationResults(_,_,view) -> view
  member this.Results =
    match this with
    | StandardResults r -> r
    | LocationResults (r, _, _) -> r
    
type SearchDetails =
    { SearchText : string
      SearchState : SearchState
      SearchResults: SearchResultType
      SearchError : SearchError option
      SelectedSearchMethod : SearchMethod
      SelectedProperty : PropertyResult option
      Sorting : Sort
      Suggestions : string array
      Debouncer : Debouncer.State }

type Model =
    { Search : SearchDetails
      IndexStats : Map<string, IndexStats>
      Refreshing : bool }

type IndexMsg = 
| LoadIndexStats
| LoadedIndexStats of IndexName * IndexStats
| StartIndexing of IndexName
| StartedIndexing of IndexName * int64

type TextChangeSource = User | System

type SearchTextMsg =
| SetSearchText of string
| DebouncerSelfMsg of SearchTextMsg Debouncer.SelfMessage
| FetchSuggestions
| FetchedSuggestions of SuggestResponse
| SetTextAndSearch of string * SearchMethod
| ValidateSearchText
| ClearSuggestions

type SearchMsg =
| FindProperties
| FoundProperties of SearchResultType
| SetSorting of string
| SetSearchMethod of SearchMethod
| SearchTextMsg of SearchTextMsg
| SelectProperty of PropertyResult
| DeselectProperty
| ChangeView of ResultsView
| FoundFailed of SearchError

type Msg =
| IndexMsg of IndexMsg
| SearchMsg of SearchMsg
| ErrorOccurred of exn
