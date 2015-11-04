\dontrun{
  library(azureml)
  
  dataset <- "New York weather"
  
  ws <- workspace()
  
  # Upload the R airquality data.frame to the workspace.
  upload.dataset(airquality, ws, "airquality")

  # Example datasets (airquality should be among them now)
  head(datasets(ws))

  # Now delete what we've just uploaded
  delete.dataset(ws, "airquality")
}
