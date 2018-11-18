module SafeSearch.Routers.Transactions

open FSharp.Control.Tasks
open FSharp.Data
open Giraffe
open SafeSearch
open SafeSearch.Search
open Saturn
open System
open System.IO

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
        // |> Seq.choose tryGeoPostcode
        |> Seq.choose(fun r ->
            match r.Latitude, r.Longitude with
            | Some lat, Some long -> Some (r.Postcode, (float lat, float long))
            | _ -> None)

let fetchTransactions() =
    let txnData, rowCount =
        let path = Path.Combine(Directory.GetCurrentDirectory(), "pp-monthly-update-new-version.csv")
        
        if not (File.Exists path) then
            let wc = new System.Net.WebClient()
            wc.DownloadFile(Uri "http://prod.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-monthly-update-new-version.csv", path)

        let lines =
            let r = System.IO.File.OpenText path
            seq { while (not r.EndOfStream) do yield r.ReadLine() } |> Seq.length
        
        (PricePaid.Load path).Rows |> Seq.cache, lines

    let data =
        lazy
            let requiredPostcodes = txnData |> Seq.choose(fun r -> r.Postcode) |> Set
            let geoLookup = Postcodes.getAllPostcodes() |> Seq.filter(fst >> requiredPostcodes.Contains) |> Map

            txnData
            |> Seq.map(fun t ->
                { Address =
                    { Building = [ Some t.PAON; t.SAON ] |> List.choose id |> String.concat " "
                      Street = t.Street
                      Locality = t.Locality
                      TownCity = t.``Town/City``
                      District = t.District
                      County = t.County
                      PostCode = t.Postcode }
                  BuildDetails =
                    { PropertyType = t.PropertyType |> Option.bind PropertyType.Parse
                      Build = t.Duration |> BuildType.Parse
                      Contract = t.``Old/New`` |> ContractType.Parse }
                  Price = t.Price
                  DateOfTransfer = t.Date }, t.Postcode |> Option.bind geoLookup.TryFind )
    rowCount, data
let propertyResultIngester = Ingestion.buildIngester<PropertyResult * ((float * float) option)>()

let ingest (searcher:ISearch) next ctx = task {
    searcher.Clear()
    let! rowsImported =
        let rowsToImport, transactions = fetchTransactions()
        propertyResultIngester.IngestData(rowsToImport, transactions, searcher.Upload)
    return! json rowsImported next ctx }

let getStats (searcher:ISearch) next ctx = task {
    let! documents = searcher.Documents()
    let! storeStatus = propertyResultIngester.GetStoreStatus()
    let indexStats =
        { DocumentCount = documents
          Status = storeStatus.AsIndexState }

    return! json indexStats next ctx }

let createRouter searcher = router {
    get "ingest" (ingest searcher)
    get "stats" (getStats searcher) }