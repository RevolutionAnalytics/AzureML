if(interactive()) library("testthat")

settingsFile <- AzureML.config.default
context("Download one file of each DataTypeId")

test_that("setup global variables", {
  AzureML:::skip_if_missing_config(settingsFile)
  ws <<- workspace()
  ds <- datasets(ws, filter = "samples")
  testIdx <- grepl("[Tt]est", ds$Name)
  ds <- ds[!testIdx, ]
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
  oneOfEach <<- oneOfEach[!oneOfEach$DataTypeId %in% c("Zip"), ]
  # oneOfEach$DataTypeId
})

if(exists("oneOfEach")){
  # oneOfEach will only exist if the previous test ran successfully, 
  # i.e. if the settings.json file could be found
  for(type in oneOfEach$DataTypeId){
    test_that(sprintf("Can download dataset of type %s", type), {
      AzureML:::skip_if_missing_config(settingsFile)
      dl <- download.datasets(ws, name = oneOfEach$Name[oneOfEach$DataTypeId == type])
      expect_is(dl, "data.frame")
      expect_true(nrow(dl) > 0)
    })
  }
}

type <- "zip"
test_that(sprintf("Can download dataset of type %s", type), {
  AzureML:::skip_if_missing_config(settingsFile)
  dl <- download.datasets(ws, Zip)
  class(dl)
  expect_is(dl, "raw")
})
