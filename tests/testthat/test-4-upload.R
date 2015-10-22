if(interactive()) library("testthat")

keyfile <- system.file("tests/testthat/config.json", package = "azureml")
if(keyfile == ""){
  message("To run tests, add a file tests/testthat/config.R containing AzureML keys")
  message("No tests ran")
} else {

  context("Upload dataset")
  test_that("Can upload dataset to workspace", {
    ws <- workspace(config = keyfile)
    # Note!
    # The 'airquality' dataset might already exist, especially if you ran this test
    # before without manually deleting it. In such cases we expect a 409 (conflict)
    # error. We check for that kind of error here and tolerate it.
    
    airquality_exists <- "airquality" %in% datasets(ws)$Name
    
    if(!airquality_exists){
      upload.dataset(airquality, ws, "airquality")
      ds <- datasets(ws)
      expect_true("airquality" %in% ds$Name)
    } else {
      expect_error(upload.dataset(airquality, ws, "airquality"))
    }
    
  })
  
  # Update to AML
  
  ## create
  ## update
  
}
