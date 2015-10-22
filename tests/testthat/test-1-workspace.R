if(interactive()) library("testthat")

keyfile <- system.file("tests/testthat/config.json", package = "azureml")
if(keyfile == ""){
  message("To run tests, add a file tests/testthat/config.R containing AzureML keys")
  message("No tests ran")
} else {
  jsondata <- jsonlite::fromJSON(keyfile)
  list2env(jsondata, envir = globalenv())
  
  #  ------------------------------------------------------------------------
  
  context("Read dataset")
  
  test_that("Can connect to workspace with supplied id and auth", {
    
    ws <- workspace(workspace_id, authorization_token)
    
    expect_is(ws, c("Workspace"))
    expect_equal(ls(ws), c("datasets", "experiments", "id"))
    expect_equal(ws$id, workspace_id)
  })
  
  test_that("Can connect to workspace with config file", {
    skip_on_cran()
    skip_on_travis()
    
    ws <- workspace(config = keyfile)
    
    expect_is(ws, c("Workspace"))
    expect_equal(ls(ws), c("datasets", "experiments", "id"))
    expect_equal(ws$id, workspace_id)
  })
  
}

