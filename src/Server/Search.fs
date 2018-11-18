namespace SafeSearch.Search

open SafeSearch
open System.Threading.Tasks

type FindNearestRequest = { Postcode : string; MaxDistance : int; Page : int; Filter : PropertyFilter }
type FindGenericRequest = { Text : string option; Page : int; Filter : PropertyFilter; Sort : Sort }
type Geo = { Lat : float; Long : float }
type SuggestRequest = { Text : string }
type GeoPostcode =
    { PostCode : string * string
      Latitude : float
      Longitude : float }
    member this.PostCodeDescription = sprintf "%s %s" (fst this.PostCode) (snd this.PostCode)


type ISearch =
    abstract GenericSearch : FindGenericRequest -> SearchResponse Task
    abstract PostcodeSearch : FindNearestRequest -> SearchResponse Task
    abstract Suggest : SuggestRequest -> SuggestResponse Task
    abstract Upload : (int * int -> unit) -> (PropertyResult * (float * float) option) seq -> unit Task
    abstract Documents : unit -> int64 Task
    abstract Clear : unit -> unit