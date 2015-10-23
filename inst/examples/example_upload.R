\dontrun{
  keyfile <- system.file("tests/testthat/config.json", package = "azureml")
  
  library(azureml)
  
  # workspace_id <- ""
  # authorization_token <- ""
  dataset <- "New York weather"
  
  ws <- workspace(config = keyfile)
  
  # Upload the R airquality data.frame to the workspace.
  upload.dataset(airquality, ws, "airquality")

  # Example datasets (airquality should be among them now)
  head(datasets(ws))

  # Now delete what we've just uploaded
  delete.dataset(ws, "airquality")
}
