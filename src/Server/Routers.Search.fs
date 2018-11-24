module SafeSearch.Routers.Search

open FSharp.Control.Tasks
open Giraffe
open Microsoft.AspNetCore.Http
open Saturn
open SafeSearch
open SafeSearch.Search

let searchProperties (searcher:Search.ISearch) (tryGetGeo: string -> Geo option System.Threading.Tasks.Task) (postCode:string, distance, page) next (ctx:HttpContext) = task {
    match! tryGetGeo postCode with
    | None -> return! json (Error (NoGeolocation postCode)) next ctx
    | Some geo ->
        let! resp =
            searcher.LocationSearch
                { Filter = ctx.BindQueryString<PropertyFilter>()
                  Geo = geo
                  MaxDistance = distance
                  Page = page }
        return! json (Ok(geo, resp.Results)) next ctx }

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
    | None -> return! json (Error (NoGeolocation postcode)) next ctx
    | Some (geo:Geo) -> return! json (Ok geo) next ctx }

let createRouter searcher tryGetGeo = router {
    getf "standard/%s/%i" (genericSearch searcher)
    getf "geo/%s/%i/%i" (searchProperties searcher tryGetGeo)
    getf "geo/%s/%i" (fun (postcode, distance) -> searchProperties searcher tryGetGeo (postcode, distance, 0))
    getf "postcode/%s" (geoLookup tryGetGeo)
    getf "suggest/%s" (searchSuggest searcher) }