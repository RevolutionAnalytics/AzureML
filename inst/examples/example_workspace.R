\dontrun{
  
  library(AzureML)
  
  # AML workspace id:
  #  settings -> name -> workspace id
  # AML authorization token:
  #  settings > authorization tokens -> primary authorization token
  ws <- workspace(
    id = "<workspace id>",
    auth = "<authorization token>",
    api_endpoint = "https://studioapi.azureml.net",
    management_endpoint = "https://management.azureml.net"
  )
  
}
