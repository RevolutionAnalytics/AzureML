\dontrun{
  keyfile <- system.file("tests/testthat/config.json", package = "AzureML")
  jsondata <- jsonlite::fromJSON(keyfile)
  list2env(jsondata, envir = globalenv())
  
  library(AzureML)
  
  # workspace_id <- ""
  # authorization_token <- ""
  dataset <- "New York weather"
  
  ws <- workspace(
    id = workspace_id,
    auth = authorization_token
  )
  
  ws$datasets
  datasets(ws)
  
  ds <- match(dataset, ws$datasets$Name)
  frame <- download.datasets(ws$datasets[ds, ])
  head(frame)
}
