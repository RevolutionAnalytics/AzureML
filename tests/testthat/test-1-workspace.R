if(interactive()) library("testthat")

if(file.exists("~/.azureml/settings.json"))
{
  context("Connect to workspace")
  
  test_that("Can connect to workspace with supplied id and auth", {
    skip_on_cran()
    skip_on_travis()
    
    ws <- workspace()
    
    expect_is(ws, c("Workspace"))
    expect_equal(ls(ws), c("datasets", "experiments", "id", "services"))
    expect_equal(ws$id, workspace_id)
  })
  
  test_that("Can connect to workspace with config file", {
    skip_on_cran()
    skip_on_travis()
    
    ws <- workspace()
    
    expect_is(ws, c("Workspace"))
    expect_equal(ls(ws), c("datasets", "experiments", "id", "services"))
    expect_equal(ws$id, workspace_id)
  })
  
} else {
  message("To run tests, add a file ~/.azureml/settings.json containing AzureML keys, see ?workspace for help")
  message("No tests ran")
}
