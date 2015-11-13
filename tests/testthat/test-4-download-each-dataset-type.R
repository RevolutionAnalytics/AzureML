if(interactive()) library("testthat")

settingsFile <- "~/.azureml/settings.json" 
if(file.exists(settingsFile))
{
  context("Download one file of every type")
  
  ws <- workspace()
  ds <- datasets(ws, filter = "samples")
  unique(ds$DataTypeId)
  
  oneOfEach <- do.call(
    rbind, 
    lapply(
      split(ds, ds$DataTypeId),
      function(x){
        x[which.min(x$Size), ]
      }
    )
  )
  
  zip <- oneOfEach[oneOfEach$DataTypeId %in% c("Zip"), ]
  oneOfEach <- oneOfEach[!oneOfEach$DataTypeId %in% c("Zip"), ]
  oneOfEach$DataTypeId
  
  for(type in oneOfEach$DataTypeId){
    test_that(sprintf("Can download dataset of type %s", type), {
      dl <- download.datasets(ws, name = oneOfEach$Name[oneOfEach$DataTypeId == type])
      expect_is(dl, "data.frame")
      expect_true(nrow(dl) > 0)
    })
  }
  test_that("Throws error if trying to download Zip", {
    expect_error(
      download.datasets(ws, zip)
    )
  })
  
} else
{
  message("To run tests, add a file ~/.azureml/settings.json containing AzureML keys, see ?workspace for help")
  message("No tests ran")
}
