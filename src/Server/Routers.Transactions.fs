module SafeSearch.Routers.Transactions

open FSharp.Control.Tasks
open FSharp.Data
open Giraffe
open SafeSearch
open SafeSearch.Search
open Saturn
open System
open System.IO
open Thoth.Json.Net

type PricePaid = CsvProvider<"price-paid-schema.csv", PreferOptionals = true, Schema="Date=Date">

module Postcodes =
    type Postcodes = CsvProvider<"uk-postcodes-schema.csv", PreferOptionals = true, Schema="Latitude=decimal option,Longitude=decimal option">
    let tryGeoPostcode (row:Postcodes.Row) =
        match row.Postcode.Split ' ', row.Latitude, row.Longitude with
        | [| partA; partB |], Some latitude, Some longitude ->
            Some { PostCode = partA, partB
                   Latitude = float latitude
                   Longitude = float longitude }
        | _ -> None    

    let getAllPostcodes() =
        let localPostcodesFilePath = Path.Combine(Directory.GetCurrentDirectory(), "ukpostcodes.csv")
        if not (File.Exists localPostcodesFilePath) then
            let zipPath = Path.Combine(Directory.GetCurrentDirectory(), "ukpostcodes.zip")
            use wc = new System.Net.WebClient()
            wc.DownloadFile(Uri "https://www.freemaptools.com/download/full-postcodes/ukpostcodes.zip", zipPath)
            Compression.ZipFile.ExtractToDirectory(zipPath, ".")
            File.Delete zipPath
        (Postcodes.Load localPostcodesFilePath).Rows
        |> Seq.choose(fun r ->
            match r.Latitude, r.Longitude with
            | Some lat, Some long -> Some (r.Postcode, (float lat, float long))
            | _ -> None)

let uploadTransactions(ConnectionString storageConnection) =
    let txnData, rowCount =
        let path = Path.Combine(Directory.GetCurrentDirectory(), "pp-monthly-update-new-version.csv")
        
        if not (File.Exists path) then
            let wc = new System.Net.WebClient()
            wc.DownloadFile(Uri "http://prod.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-monthly-update-new-version.csv", path)

        let lines =
            let r = System.IO.File.OpenText path
            seq { while (not r.EndOfStream) do yield r.ReadLine() } |> Seq.length
        
        (PricePaid.Load path).Rows |> Seq.cache, lines

    async {
        printfn "Build postcode lookup"        
        let geoLookup =
            let requiredPostcodes = txnData |> Seq.choose(fun r -> r.Postcode) |> Set
            Postcodes.getAllPostcodes() |> Seq.filter(fst >> requiredPostcodes.Contains) |> Map

        let encode (prop:PricePaid.Row) =
            let geo = prop.Postcode |> Option.bind geoLookup.TryFind
            Encode.object [
                yield "TransactionId", Encode.string (prop.TransactionId.ToString())
                yield "Price", Encode.int prop.Price
                yield "DateOfTransfer", Encode.datetime prop.Date
                match prop.Postcode with Some p -> yield "PostCode", Encode.string p | _ -> ()
                match prop.PropertyType |> Option.bind SafeSearch.PropertyType.Parse with Some p -> yield "PropertyType", Encode.string p.Description | _ -> ()
                yield "Build", prop.Duration |> SafeSearch.BuildType.Parse |> fun s -> s.Description |> Encode.string
                yield "Contract", prop.``Old/New`` |> SafeSearch.ContractType.Parse |> fun s -> s.Description |> Encode.string
                yield "Building", [ Some prop.PAON; prop.SAON ] |> List.choose id |> String.concat " " |> Encode.string
                match prop.Street with Some s -> yield "Street", s |> Encode.string | _ -> ()
                match prop.Locality with Some s -> yield "Locality", Encode.string s | _ -> ()
                yield "Town", Encode.string prop.``Town/City``
                yield "District", Encode.string prop.District
                yield "County", Encode.string prop.County
                match geo with
                | Some (lat, long) ->
                    yield "Geo", Encode.object [
                        "type", Encode.string "Point"
                        "coordinates", Encode.array [| Encode.float long; Encode.float lat |]
                    ]
                | None -> ()
            ]

        for (i, chunk) in (txnData |> Seq.chunkBySize 10000 |> Seq.indexed) do
            let json = chunk |> Array.map encode |> Encode.array |> Encode.toString 4
            printfn "Uploading %d..." i
            let b = Storage.Azure.Containers.properties.[sprintf "%d.json" i]
            do! b.AsCloudBlockBlob(storageConnection).UploadTextAsync(json) |> Async.AwaitTask
            ()
    } |> Async.Start 
    rowCount
let propertyResultIngester = Ingestion.buildIngester<PropertyResult * ((float * float) option)>()

let ingest (searcher:ISearch) storageConnection next ctx = task {
    searcher.Clear()
    let rowsImported = uploadTransactions storageConnection
    return! json rowsImported next ctx }

let getStats (searcher:ISearch) next ctx = task {
    let! documents = searcher.Documents()
    let! storeStatus = propertyResultIngester.GetStoreStatus()
    let indexStats =
        { DocumentCount = documents
          Status = storeStatus.AsIndexState }

    return! json indexStats next ctx }

let createRouter searcher storageConnection = router {
    get "ingest" (ingest searcher storageConnection)
    get "stats" (getStats searcher) }