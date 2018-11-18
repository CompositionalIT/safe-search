module SafeSearch.Ingestion

open System.Threading.Tasks

type OnComplete = (int * int) -> unit
type Importer<'T> = OnComplete -> 'T seq -> unit Task
type ProgressFunc = int -> unit
type StoreStatus =
    | Idle
    | Downloading of int
    | Ingesting of int * int
    | Loaded of int
    member this.AsIndexState =
        match this with
        | Idle | Loaded _ -> IndexState.Idle
        | Downloading _ -> IndexState.Indexing 0
        | Ingesting (remaining, loaded) ->
            let percent = (100. / (float (remaining + loaded))) * (float loaded) |> int
            IndexState.Indexing percent

type Message<'T> =
    | Ingest of rows:int * 'T seq * 'T Importer * ProgressFunc
    | Report of (StoreStatus -> unit)
    | MarkCompleted of int
    | UploadedDocuments of int * int

let buildImportStore<'T> () = MailboxProcessor.Start(fun inbox ->
    let reporter docs = inbox.Post (UploadedDocuments docs)

    let rec processMsg state = async {
        let! msg = inbox.Receive()
        printfn "%A %A" state msg
        match msg, state with
        | UploadedDocuments (docs, failed), Ingesting (uploading, uploaded) ->
            let total = docs + failed
            return! processMsg (Ingesting(uploading - total, uploaded + docs))
        
        | UploadedDocuments (docs, failed), Downloading rows ->
            let total = docs + failed
            return! processMsg (Ingesting(rows - total, docs))

        | Ingest (rowCount, data, importer:'T Importer, reply), (Idle | Loaded _) ->
            async {
                try
                do! data |> importer reporter |> Async.AwaitTask
                with ex -> printfn "%A" ex
                inbox.Post(MarkCompleted rowCount) }
            |> Async.Start
            reply rowCount
            return! processMsg (Downloading rowCount)
        
        | MarkCompleted rows, _ ->
            return! processMsg (Loaded rows)
        
        | Report reply, _ ->
            reply state
            return! processMsg state
        
        | Ingest _, (Downloading _ | Ingesting _)
        | UploadedDocuments _, (Idle | Loaded _) ->
            return! processMsg state }

    processMsg StoreStatus.Idle)

type Ingester<'T> =
    abstract member IngestData : rowsToImport:int * 'T seq * 'T Importer -> int Task
    abstract member GetStoreStatus : unit -> StoreStatus Task

let buildIngester<'T>() =
    let store = buildImportStore<'T>()
    store.Error |> Event.add (printfn "INGESTION EXCEPTION: %A")

    { new Ingester<'T> with
        member __.IngestData(rows, data, importer) =
            store.PostAndAsyncReply (fun t -> Ingest(rows, data, importer, t.Reply)) |> Async.StartAsTask
        member __.GetStoreStatus() =
            store.PostAndAsyncReply (fun c -> Report c.Reply) |> Async.StartAsTask
    }