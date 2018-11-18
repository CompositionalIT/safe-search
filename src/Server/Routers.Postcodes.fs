module SafeSearch.Routers.Postcodes

open FSharp.Azure.StorageTypeProvider
open FSharp.Azure.StorageTypeProvider.Table
open FSharp.Control.Tasks
open FSharp.Data
open Giraffe
open SafeSearch
open Saturn
open System
open System.IO
open System.Threading.Tasks

[<Literal>]
let PostCodesSchema = __SOURCE_DIRECTORY__ + @"\uk-postcodes-schema.csv"
type Postcodes = CsvProvider<PostCodesSchema, PreferOptionals = true, Schema="Latitude=decimal option,Longitude=decimal option">
type GeoPostcode =
    { PostCode : string * string
      Latitude : float
      Longitude : float }
    member this.PostCodeDescription = sprintf "%s %s" (fst this.PostCode) (snd this.PostCode)

[<Literal>]
let AzureSchema = __SOURCE_DIRECTORY__ + @"\..\server\tables.json"
type Azure = AzureTypeProvider<tableSchema = AzureSchema>
let postcodesTable = Azure.Tables.postcodes

let tryGeoPostcode (row:Postcodes.Row) =
    match row.Postcode.Split ' ', row.Latitude, row.Longitude with
    | [| partA; partB |], Some latitude, Some longitude ->
        Some { PostCode = partA, partB
               Latitude = float latitude
               Longitude = float longitude }
    | _ -> None    

let insertPostcodes connectionString onComplete (postcodes:GeoPostcode seq) = task {
    let! _ = postcodesTable.AsCloudTable(connectionString).CreateIfNotExistsAsync()
    let entities =
        postcodes
        |> Seq.map(fun p ->
            let partA, partB = p.PostCode
            Azure.Domain.postcodesEntity(Partition partA, Row partB, p.PostCodeDescription, p.Latitude, p.Longitude))

    for batch in entities |> Seq.chunkBySize 5000 do
        let! resp = postcodesTable.InsertAsync(batch, TableInsertMode.Insert, connectionString)
        let succeeded, failed = resp |> Seq.collect snd |> Seq.toArray |> Array.partition(function | SuccessfulResponse _ -> true | _ -> false)
        printfn "%d %d" succeeded.Length failed.Length
        onComplete(succeeded.Length, failed.Length) }

let localPostcodesFilePath = Path.Combine(Directory.GetCurrentDirectory(), "ukpostcodes.csv")
let downloadPostcodes() =
    let zipPath = Path.Combine(Directory.GetCurrentDirectory(), "ukpostcodes.zip")

    if not (File.Exists localPostcodesFilePath) then
        use wc = new System.Net.WebClient()
        wc.DownloadFile(Uri "https://www.freemaptools.com/download/full-postcodes/ukpostcodes.zip", zipPath)
        Compression.ZipFile.ExtractToDirectory(zipPath, ".")
        File.Delete zipPath

let getPostcodes fileName =
    let lines =
        let r = File.OpenText fileName
        seq { while (not r.EndOfStream) do yield r.ReadLine() } |> Seq.length

    lines, (Postcodes.Load fileName).Rows

let postcodesIngester = Ingestion.buildIngester<GeoPostcode>()

let ingest (ConnectionString connectionString) next ctx = task {
    let rowsToImport, postcodes =
        downloadPostcodes()
        getPostcodes localPostcodesFilePath
    let postcodes = lazy (postcodes |> Seq.choose tryGeoPostcode)

    let! rowsImported = postcodesIngester.IngestData(rowsToImport, postcodes, insertPostcodes connectionString)

    return! json rowsImported next ctx }

open SafeSearch.Ingestion


let getStats (ConnectionString connectionString) next ctx = task {
    let (|PostcodesLocal|NoLocalPostcodes|) file = if File.Exists file then PostcodesLocal else NoLocalPostcodes
    let (|NoRowsInTable|RowsInTable|) = function [||] -> NoRowsInTable | _ -> RowsInTable
    let! storeStatus = postcodesIngester.GetStoreStatus()
    let! documentCount =
        match storeStatus with
        | Idle -> task {
            let! firstRow = task {
                match! postcodesTable.AsCloudTable(connectionString).ExistsAsync() with
                | true -> return! postcodesTable.Query().ExecuteAsync(1, connectionString)
                | false -> return [||] }
            match firstRow, localPostcodesFilePath with
            | NoRowsInTable, _ -> return 0
            | RowsInTable, NoLocalPostcodes -> return 0
            | RowsInTable, PostcodesLocal -> return getPostcodes localPostcodesFilePath |> fst }
        | Downloading _ -> Task.FromResult 0
        | Ingesting (_, docsDone) -> Task.FromResult docsDone
        | Loaded docs -> Task.FromResult docs
    let indexStats =
        { DocumentCount = int64 documentCount
          Status = storeStatus.AsIndexState }
    return! json indexStats next ctx }

let createRouter azureConfig = router {
    get "ingest" (ingest azureConfig)
    get "stats" (getStats azureConfig) }