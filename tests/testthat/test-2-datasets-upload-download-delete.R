if(interactive()) library("testthat")

settingsFile <- AzureML.config.default

context("Upload and delete dataset")

test_that("datasets(ws) returns results", {
  AzureML:::skip_if_missing_config(settingsFile)
  
  ws <<- workspace()
  
  x <- datasets(ws)
  expect_is(x, "data.frame")
})

timestamped_name <- paste0("dataset-test-upload-",
                           format(Sys.time(), format="%Y-%m-%d--%H-%M-%S"))

test_that("Can upload dataset to workspace", {
  AzureML:::skip_if_missing_config(settingsFile)
  upload.dataset(airquality, ws, timestamped_name)
  ds <- datasets(ws, filter = "my")
  expect_true(timestamped_name %in% ds$Name)
})

test_that("Uploading dataset with duplicate name gives helpful error", {
  AzureML:::skip_if_missing_config(settingsFile)
  expect_error(upload.dataset(airquality, ws, timestamped_name),
               sprintf("A dataset with the name '%s' already exists in AzureML", timestamped_name)
  )
})

test_that("Can download dataset", {
  AzureML:::skip_if_missing_config(settingsFile)
  dl <- download.datasets(ws, name = timestamped_name)
  expect_equal(dl, airquality)
})

test_that("Can delete dataset from workspace", {
  AzureML:::skip_if_missing_config(settingsFile)
  z <- delete.datasets(ws, timestamped_name)
  expect_true(timestamped_name %in% z$Name && z$Deleted[z$Name == timestamped_name])
  # Force refresh - sometime this fails in non-interactive
  Sys.sleep(1); refresh(ws, what = "datasets")
  ds <- datasets(ws, filter = "my")
  expect_true(nrow(ds) == 0 || !timestamped_name %in% ds$Name)
})



test_that("Invalid input throws helpful error", {
  expect_error(download.datasets('HSAFundsData.csv'),
               "You specified a dataset name that is not in the workspace. See help file for `download.datasets`"
  )
})






