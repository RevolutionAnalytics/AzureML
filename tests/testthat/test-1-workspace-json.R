#  ------------------------------------------------------------------------

context("workspace - reading from settings.json file")

test_that("workspace() adds api_endpoint and management_endpoint if missing from config", {
  tf <- tempfile(fileext = ".json")
  on.exit(unlink(tf))
  write.AzureML.config("x", "y", file = tf)
  ws <- AzureML::workspace(config = tf, .validate = FALSE)
  expect_equal(ws$id, "x")
  expect_equal(
    ws$.api_endpoint, 
    default_api(ws$.api_endpoint)[["api_endpoint"]]
  )
  expect_equal(
    ws$.management_endpoint, 
    default_api(ws$.api_endpoint)[["management_endpoint"]]
  )
})

test_that("workspace() throws helpful error if config file does not exist", {
  expect_error(
    AzureML::workspace(config = "file_does_not_exist"),
    "config file is missing: 'file_does_not_exist'"
  )
})

test_that("workspace() throws helpful error if config is invalid json", {
  tf <- tempfile(fileext = ".json")
  on.exit(unlink(tf))
  writeLines("garbage", con = tf)
  msg <- tryCatch(AzureML::workspace(config = tf), error = function(e)e)$message
  expect_true(
    grepl("Your config file contains invalid json", msg)
  )
})

