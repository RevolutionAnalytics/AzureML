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
