if(interactive()) library("testthat")

settingsFile <- "~/.azureml/settings.json" 
if(file.exists(settingsFile))
{
  context("Upload and delete dataset")
  
  timestamped_name <- paste0("dataset-test-upload-", 
                             format(Sys.time(), format="%Y-%m-%d--%H-%M-%S"))
  
  test_that("Can upload dataset to workspace", {
    ws <- workspace()
    upload.dataset(airquality, ws, timestamped_name)
    ds <- datasets(ws, filter = "my")
    expect_true(timestamped_name %in% ds$Name)
  })
  
  test_that("Can download dataset", {
    ws <- workspace()
    dl <- download.datasets(ws, name=timestamped_name)
    expect_equal(dl, airquality)
  })
  
  test_that("Can delete dataset from workspace", {
    ws <- workspace()
    z <- delete.datasets(ws, timestamped_name)
    expect_true(timestamped_name %in% z$Name)
    # refresh(ws)
    ds <- datasets(ws, filter = "my")
    expect_false(timestamped_name %in% ds$Name)
  })
  
} else
{
  message("To run tests, add a file ~/.azureml/settings.json containing AzureML keys, see ?workspace for help")
  message("No tests ran")
}
