module SafeSearch.Routers.Search

open FSharp.Control.Tasks
open Giraffe
open Microsoft.AspNetCore.Http
open Saturn
open SafeSearch
open SafeSearch.Search

let searchProperties (searcher:Search.ISearch) (postCode:string, distance, page) next (ctx:HttpContext) = task {
    let! resp =
        searcher.PostcodeSearch
            { Filter = ctx.BindQueryString<PropertyFilter>()
              Postcode = postCode.ToUpper()
              MaxDistance = distance
              Page = page }
    match resp with
    | Some (geo, properties) -> return! json (geo, properties.Results) next ctx
    | None -> return! HttpStatusCodeHandlers.RequestErrors.notFound (text "Could not locate geolocation for this property.") next ctx }

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

let createRouter searcher = router {
    getf "find/%s/%i" (genericSearch searcher)
    getf "suggest/%s" (searchSuggest searcher)
    getf "%s/%i/%i" (searchProperties searcher)
    getf "%s/%i" (fun (postcode, distance) -> searchProperties searcher (postcode, distance, 0)) }