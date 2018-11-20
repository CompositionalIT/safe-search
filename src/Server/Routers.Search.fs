module SafeSearch.Routers.Search

open FSharp.Control.Tasks
open Giraffe
open Microsoft.AspNetCore.Http
open Saturn
open SafeSearch
open SafeSearch.Search

let searchProperties (searcher:Search.ISearch) (tryGetGeo: string -> Geo option System.Threading.Tasks.Task) (postCode:string, distance, page) next (ctx:HttpContext) = task {
    match! tryGetGeo postCode with
    | None -> return! HttpStatusCodeHandlers.RequestErrors.notFound (text (sprintf "Could not locate geolocation for postcode %s." postCode)) next ctx
    | Some geo ->
        let! resp =
            searcher.LocationSearch
                { Filter = ctx.BindQueryString<PropertyFilter>()
                  Geo = geo
                  MaxDistance = distance
                  Page = page }
        return! json (geo, resp.Results) next ctx }

let searchSuggest (searcher:Search.ISearch) text next (ctx:HttpContext) = task {
    let! properties = searcher.Suggest { Text = text }
    return! json properties next ctx }
    
let genericSearch (searcher:Search.ISearch) (text, page) next (ctx:HttpContext) = task {
    let request =
        { Page = page
          Text = if System.String.IsNullOrWhiteSpace text then None else Some text
          Filter = ctx.BindQueryString<PropertyFilter>()
          Sort =
            { SortColumn = ctx.TryGetQueryStringValue "SortColumn"
              SortDirection = ctx.TryGetQueryStringValue "SortDirection" |> Option.bind SortDirection.TryParse } }
    
    let! searchResponse = searcher.GenericSearch request
    return! json searchResponse.Results next ctx }

let geoLookup tryGetGeo postcode next ctx = task {
    let! geo = tryGetGeo postcode
    match geo with
    | None -> return! HttpStatusCodeHandlers.RequestErrors.notFound (text (sprintf "Could not locate geolocation for postcode %s." postcode)) next ctx
    | Some (geo:Geo) -> return! json geo next ctx }

let createRouter searcher tryGetGeo = router {
    getf "find/%s/%i" (genericSearch searcher)
    getf "suggest/%s" (searchSuggest searcher)
    getf "geo/%s" (geoLookup tryGetGeo)
    getf "%s/%i/%i" (searchProperties searcher tryGetGeo)
    getf "%s/%i" (fun (postcode, distance) -> searchProperties searcher tryGetGeo (postcode, distance, 0)) }