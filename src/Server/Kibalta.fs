namespace Microsoft.Azure.Search

open FSharp.Control.Tasks
open Microsoft.Azure.Search
open Microsoft.Azure.Search.Models
open System

type Direction =
    | Ascending
    | Descending
    member this.StringValue =
        match this with
        | Ascending -> "asc"
        | Descending -> "desc"

type SortColumn =
    | ByField of field: string * Direction
    | ByDistance of long: float * lat: float * Direction
    member this.StringValue =
        match this with
        | ByField(field, dir) -> sprintf "%s %s" field dir.StringValue
        | ByDistance(long, lat, dir) -> 
            sprintf "geo.distance(Geo, geography'POINT(%f %f)') %s" long lat dir.StringValue

module Filters =
    type FilterCombiner =
        | And
        | Or
    
    type FilterComparison =
        | Eq
        | Ne
        | Gt
        | Lt
        | Ge
        | Le
        member this.StringValue =
            match this with
            | Eq -> "eq"
            | Ne -> "ne"
            | Gt -> "gt"
            | Lt -> "lt"
            | Ge -> "ge"
            | Le -> "le"
    
    type Field =
        | Field of string
        | Distance of long: float * lat: float
        member this.AsString =
            match this with
            | Field s -> s
            | Distance(long, lat) -> sprintf "geo.distance(Geo, geography'POINT(%f %f)')" long lat
    
    type FilterExpr =
        | Filter of field: Field * FilterComparison * value: obj
        | BinaryFilter of FilterExpr * FilterCombiner * FilterExpr
        
        /// ANDs two filters together
        static member (+) (a, b) = BinaryFilter(a, And, b)
        
        /// ORs two filters together
        static member (*) (a, b) = BinaryFilter(a, Or, b)
    
    let DefaultFilter = Filter(Field null, Eq, null)
    // A helper to create a basic field filter.
    let where a comp b = Filter(Field a, comp, b)
    // A helper to create a basic geo filter.
    let whereGeo a comp b = Filter(Distance a, comp, b)
    let combine = List.fold (+) DefaultFilter
    let whereEq (a, b) = where a Eq b
    
    let rec eval =
        function 
        | Filter(_, _, null) -> "true"
        | Filter(field, comparison, value) -> 
            match value with
            | :? string as s -> sprintf "%s %s '%s'" field.AsString comparison.StringValue s
            | s -> sprintf "%s %s %O" field.AsString comparison.StringValue s
        | BinaryFilter(left, And, right) -> sprintf "%s and %s" (eval left) (eval right)
        | BinaryFilter(left, Or, right) -> sprintf "%s or %s" (eval left) (eval right)

type QueryDetails =
    { Parameters: SearchParameters
      Text: string option }

type AzureSearchBuilder() =
    
    member __.Yield _ =
        { Parameters = SearchParameters()
          Text = None }
    
    /// Sets the filter on the search query.
    [<CustomOperation"filter">]
    member __.Filter(state: QueryDetails, filter) =
        state.Parameters.Filter <- Filters.eval filter
        state
    
    /// Sets which facets should be returned from the search.    
    [<CustomOperation"facets">]
    member __.Facets(state: QueryDetails, facets: string list) =
        state.Parameters.Facets <- ResizeArray facets
        state
    
    /// Sets the number of search results to skip.
    [<CustomOperation"skip">]
    member __.Skip(state: QueryDetails, count) =
        state.Parameters.Skip <- Nullable count
        state
    
    /// Sets the number of search results to retrieve.
    [<CustomOperation"top">]
    member __.Top(state: QueryDetails, count) =
        state.Parameters.Top <- Nullable count
        state
    
    /// Sets the list of expressions by which to sort the results.
    [<CustomOperation"sort">]
    member __.Sort(state: QueryDetails, columns) =
        state.Parameters.OrderBy <- columns
                                    |> List.map (fun (c: SortColumn) -> c.StringValue)
                                    |> ResizeArray
        state
    
    /// Sets the fulltext query expression.
    [<CustomOperation"fulltext">]
    member __.FullText(state: QueryDetails, text) = { state with Text = Some text }
    
    /// Specifies whether to fetch the approximate total count of results.
    [<CustomOperation"includeTotalResults">]
    member __.IncludeTotalResults(state: QueryDetails) =
        state.Parameters.IncludeTotalResultCount <- true
        state

[<AutoOpen>]
module Kibalta =
    let azureSearch = AzureSearchBuilder()
    let makeClient name key = new SearchServiceClient(name, SearchCredentials key)
    
    let doSearch<'T when 'T: not struct> index (client: SearchServiceClient) query =
        let index = client.Indexes.GetClient index
        task { 
            let! searchResult = index.Documents.SearchAsync<'T>
                                    (Option.toObj query.Text, query.Parameters)
            let results =
                searchResult.Results
                |> Seq.toArray
                |> Array.map (fun r -> r.Document)
            
            let count =
                searchResult.Count
                |> Option.ofNullable
                |> Option.map int
            
            let facets =
                searchResult.Facets
                |> Option.ofObj
                |> Option.map (Seq.map (fun kvp -> 
                                   kvp.Key, 
                                   kvp.Value
                                   |> Seq.map (fun facetResult -> string facetResult.Value)
                                   |> Seq.toList)
                               >> Map)
                |> Option.defaultValue Map.empty
            
            return results, facets, count
        }
