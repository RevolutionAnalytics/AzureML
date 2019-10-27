\dontrun{
  library(AzureML)
  
  ws <- workspace()
  
  # Upload the R airquality data.frame to the workspace.
  upload.dataset(airquality, ws, "airquality")

  # Make an update by using only the first 10 rows
  update.dataset(airquality[1:10, ], ws, "airquality")
  
  # Download the updated dataset to check its content
  download.datasets(ws, name = "airquality")

  # Now delete what we've just uploaded
  delete.datasets(ws, "airquality")
}
