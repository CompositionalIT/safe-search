namespace SafeSearch

type ConnectionString = ConnectionString of string

type Configuration =
    { AzureStorage : ConnectionString
      AzureSearch : ConnectionString * string
      GoogleMapsApiKey : string
      AzureAdDomain : string
      AzureAdClientId : string
      AzureAdClientSecret : string }
