if(interactive()) library("testthat")

keyfile <- system.file("tests/testthat/config.json", package = "azureml")
if(keyfile == ""){
  message("To run tests, add a file tests/testthat/config.R containing AzureML keys")
  message("No tests ran")
} else {
  jsondata <- jsonlite::fromJSON(keyfile)
  workspace_id <- jsondata$id
  authorization_token <- jsondata$authorization_token

  #  ------------------------------------------------------------------------
  
  context("Connect to workspace")
  
  test_that("Can connect to workspace with supplied id and auth", {
    skip_on_cran()
    skip_on_travis()
    
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

