if(interactive()) library("testthat")

keyfile <- system.file("tests/testthat/config.json", package = "azureml")
if(keyfile == ""){
  message("To run tests, add a file tests/testthat/config.R containing AzureML keys")
  message("No tests ran")
} else {

  context("Read dataset")
  test_that("Can upload dataset to workspace", {
    ws <- workspace(config = keyfile)
    # Note!
    # The 'airquality' dataset might already exist, especially if you ran this test
    # before without manually deleting it. In such cases we expect a 409 (conflict)
    # error. We check for that kind of error here and tolerate it.
    tryCatch(upload.dataset(airquality, ws, "airquality"), error=function(e)
    {
      if(grepl("HTTP ERROR 409", as.character(e))) return()
      stop(e)
    })
    
    ds <- datasets(ws)
    expect_true("airquality" %in% ds$Name)
    
  })
  
  # Update to AML
  
  ## create
  ## update
  
}
