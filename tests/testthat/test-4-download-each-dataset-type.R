if(interactive()) library("testthat")

settingsFile <- "~/.azureml/settings.json"
if(file.exists(settingsFile))
{
  context("Download one file of each DataTypeId")

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

  Zip <- oneOfEach[oneOfEach$DataTypeId %in% c("Zip"), ]
  oneOfEach <- oneOfEach[!oneOfEach$DataTypeId %in% c("Zip"), ]
  oneOfEach$DataTypeId

  for(type in oneOfEach$DataTypeId){
    test_that(sprintf("Can download dataset of type %s", type), {
      dl <- download.datasets(ws, name = oneOfEach$Name[oneOfEach$DataTypeId == type])
      expect_is(dl, "data.frame")
      expect_true(nrow(dl) > 0)
    })
  }
  
  dl <- download.datasets(ws, Zip)
  class(dl)
  expect_is(dl, "raw")
  

} else
{
  message("To run tests, add a file ~/.azureml/settings.json containing AzureML keys, see ?workspace for help")
  message("No tests ran")
}
