module SafeSearch.Storage

open FSharp.Control.Tasks

//type Azure = AzureTypeProvider<tableSchema="tables.json", blobSchema="blobs.json">

let tryGetGeo (ConnectionString connectionString) (postcode:string) = task {
//    match postcode.Split ' ' with
//    | [| postcodeA; postcodeB |] ->
//        let! result = Azure.Tables.postcodes.GetAsync(Table.Row (postcodeB.ToUpper()), Table.Partition (postcodeA.ToUpper()), connectionString)
//        return result |> Option.map(fun result -> { Lat = result.Lat; Long = result.Long })
//    | _ -> return None
    return None
}
