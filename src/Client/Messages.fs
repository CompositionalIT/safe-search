namespace SafeSearch

type SearchState = CanSearch | NoSearchText | Searching
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
      SelectedSearchMethod : SearchMethod
      SelectedProperty : PropertyResult option
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
| FoundProperties of SearchResultType
| SetSearchText of string
| SetSorting of string
| SetSearchMethod of SearchMethod
| SelectProperty of PropertyResult
| DeselectProperty
| ChangeView of ResultsView
| SearchPostcode of string

type Msg =
| IndexMsg of IndexMsg
| SearchMsg of SearchMsg
| ErrorOccurred of exn
