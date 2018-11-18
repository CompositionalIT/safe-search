namespace SafeSearch

type ConnectionString = ConnectionString of string
type Configuration =
    { AzureStorage : ConnectionString
      AzureSearch : (ConnectionString * string) option }

