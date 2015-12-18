if(interactive()) library("testthat")

skip_if_missing_config <- function(f){
  if(!file.exists(settingsFile)) {
    msg <- paste("To run tests, add a file ~/.azureml/settings.json containing AzureML keys.",
                 "See ?workspace for help",
                 sep = "\n")
    message(msg)
    skip("settings.json file is missing")
  }
}

settingsFile <- AzureML.config.default

  context("Connect to workspace")

  test_that("Can connect to workspace with supplied id and auth", {
    skip_if_missing_config(settingsFile)
    
    js <- jsonlite::fromJSON(settingsFile)
    id <- js$workspace$id
    auth <- js$workspace$authorization_token
    
    expect_true(!is.null(id))
    expect_true(!is.null(auth))
    
    ws <- workspace(id, auth)
    
    expect_is(ws, c("Workspace"))
    expect_equal(ls(ws), c("datasets", "experiments", "id", "services"))
    expect_equal(ws$id, id)
  })
  
  test_that("Can connect to workspace with config file", {
    skip_if_missing_config(settingsFile)

    ws <- workspace()
    
    expect_is(ws, c("Workspace"))
    expect_equal(ls(ws), c("datasets", "experiments", "id", "services"))
  })
  
context("Reading from settings.json file")

test_that("Add api_endpoint and management_endpoint if missing from config", {
  tf <- tempfile(fileext = ".json")
  on.exit(unlink(tf))
  makeConfig("x", "y", file = tf)
  ws <- workspace(config = tf)
  expect_equal(ws$id, "x")
  expect_equal(ws$.api_endpoint, default_api(ws$.api_endpoint)[["api_endpoint"]])
  expect_equal(ws$.management_endpoint, default_api(ws$.api_endpoint)[["management_endpoint"]])
})

test_that("Throw helpful error if config file does not exist", {
  expect_error(workspace(config = "file_does_not_exist"),
               "config file is missing: 'file_does_not_exist'")
})

test_that("Throws helpful error if config is invalid json", {
  tf <- tempfile(fileext = ".json")
  on.exit(unlink(tf))
  writeLines("garbage", con = tf)
  msg <- tryCatch(workspace(config = tf), error = function(e)e)$message
  expect_true(
    grepl("Your config file contains invalid json", msg)
  )
})

