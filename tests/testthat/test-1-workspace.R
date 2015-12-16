if(interactive()) library("testthat")

settingsFile <- "~/.azureml/settings.json"
if(file.exists(settingsFile))
{
  context("Connect to workspace")
  
  test_that("Can connect to workspace with supplied id and auth", {
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
    skip_on_cran()
    skip_on_travis()
    
    ws <- workspace()
    
    expect_is(ws, c("Workspace"))
    expect_equal(ls(ws), c("datasets", "experiments", "id", "services"))
  })
  
} else {
  message("To run tests, add a file ~/.azureml/settings.json containing AzureML keys, see ?workspace for help")
  message("No tests ran")
}

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

test_that("Add api_endpoint and management_endpoint if missing from config", {
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

