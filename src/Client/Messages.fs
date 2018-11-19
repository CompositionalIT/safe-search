namespace SafeSearch

type SearchState = CanSearch | NoSearchText | Searching
type IndexName = PostcodeIndex | TransactionsIndex member this.Endpoint = match this with PostcodeIndex -> "postcodes" | TransactionsIndex -> "transactions"
type SearchMethod = Standard | Location

type SearchDetails =
    { SearchText : string
      SearchState : SearchState
      SearchResults: PropertyResult array
      SearchMethod : SearchMethod
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
| FoundProperties of PropertyResult array
| SetSearchText of string
| SetSorting of string
| SetSearchMethod of SearchMethod
| SelectProperty of PropertyResult
| DeselectProperty

type Msg =
| IndexMsg of IndexMsg
| SearchMsg of SearchMsg
| ErrorOccurred of exn
