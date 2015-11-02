if(interactive()) library("testthat")

keyfile <- system.file("tests/testthat/config.json", package = "AzureML")
if(keyfile == ""){
  message("To run tests, add a file tests/testthat/config.R containing AzureML keys")
  message("No tests ran")
} else {

  context("Upload and delete dataset")
  
  timestamped_name <- paste0("dataset-test-upload-", format(Sys.time(), format="%Y-%m-%d--%H-%M-%S"))
  
  test_that("Can upload dataset to workspace", {
    ws <- workspace(config = keyfile)
    upload.dataset(airquality, ws, timestamped_name)
    ds <- datasets(ws, filter = "my")
    expect_true(timestamped_name %in% ds$Name)
  })

  test_that("Can delete dataset from workspace", {
    ws <- workspace(config = keyfile)
    delete.datasets(ws, timestamped_name)
    ds <- datasets(ws, filter = "my")
    expect_false(timestamped_name %in% ds$Name)
  })
  
}
