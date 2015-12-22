\dontrun{
  
  library(AzureML)
  
  ws <- workspace(
    id = "<workspace id>",
    auth = "<authorization token>",
    api_endpoint = "https://studioapi.azureml.net",
    management_endpoint = "https://management.azureml.net"
  )
  
}
