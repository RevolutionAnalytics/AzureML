\dontrun{
  keyfile <- system.file("tests/testthat/config.json", package = "azureml")
  
  library(azureml)
  
  # workspace_id <- ""
  # authorization_token <- ""
  dataset <- "New York weather"
  
  ws <- workspace(config = keyfile)
  
  download.datasets(ws, dataset)
}
