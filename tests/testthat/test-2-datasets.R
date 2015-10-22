if(interactive()) library("testthat")

keyfile <- system.file("tests/testthat/config.json", package = "azureml")
if(keyfile == ""){
  message("To run tests, add a file tests/testthat/config.R containing AzureML keys")
  message("No tests ran")
} else {

  context("Read dataset")
  
  test_that("Can read dataset from workspace", {
    
    ws <- workspace(config = keyfile)
    
    ds <- datasets(ws)
    expect_is(ds, "Datasets")
    expect_is(ds, "data.frame")
    
    expect_identical(ws$datasets, datasets(ws))
    expect_true(nrow(ds) > 0)
    
    expect_true("Iris Two Class Data" %in% ds$Name)
    idx <- match("Iris Two Class Data", ds$Name)
    irisaz <- download.datasets(ws$datasets, "Iris Two Class Data")
    expect_is(irisaz, "data.frame")
    
    expect_equal(dim(irisaz), c(100, 5))
    
  })
  
  # Update to AML
  
  ## create
  ## update
  
}

