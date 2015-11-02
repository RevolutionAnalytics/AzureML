\dontrun{
  keyfile <- system.file("tests/testthat/config.json", package = "AzureML")
  
  library(AzureML)
  
  # workspace_id <- ""
  # authorization_token <- ""
  name <- "Blood donation data"
  
  ws <- workspace(config = keyfile)
  
  # The following three alternatives produce the same output:
  frame1 <- download.datasets(ws, name)
  frame2 <- download.datasets(datasets(ws), name)

  # Note that one can examine all the names, sizes, etc. of the datasets
  # in ws by examining d:
  d <- datasets(ws)
  frame3 <- download.datasets(subset(d, Name == name))

  head(frame1)
}
