\dontrun{
  keyfile <- system.file("tests/testthat/config.json", package = "azureml")
  
  library(azureml)
  
  # workspace_id <- ""
  # authorization_token <- ""
  dataset <- "New York weather"
  
  ws <- workspace(config = keyfile)
  
  frame <- download.datasets(datasets(ws), dataset)
  head(frame)
}
