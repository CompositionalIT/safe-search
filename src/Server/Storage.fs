module SafeSearch.Storage

open FSharp.Azure.StorageTypeProvider
open FSharp.Control.Tasks
open SafeSearch.Search

type Azure = AzureTypeProvider<tableSchema="tables.json", blobSchema="blobs.json">

let tryGetGeo (ConnectionString connectionString) (postcode:string) = task {
    match postcode.Split ' ' with
    | [| postcodeA; postcodeB |] ->
        let! result = Azure.Tables.postcodes.GetAsync(Table.Row postcodeB, Table.Partition postcodeA, connectionString)
        return result |> Option.map(fun result -> { Lat = result.Lat; Long = result.Long })
    | _ -> return None }
