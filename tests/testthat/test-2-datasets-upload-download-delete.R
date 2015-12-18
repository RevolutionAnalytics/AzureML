if(interactive()) library("testthat")

settingsFile <- AzureML.config.default
if(file.exists(settingsFile))
{
  context("Upload and delete dataset")
  ws <- workspace()
  
  timestamped_name <- paste0("dataset-test-upload-",
                             format(Sys.time(), format="%Y-%m-%d--%H-%M-%S"))

  test_that("Can upload dataset to workspace", {
    upload.dataset(airquality, ws, timestamped_name)
    ds <- datasets(ws, filter = "my")
    expect_true(timestamped_name %in% ds$Name)
  })
  
  test_that("Uploading dataset with duplicate name gives helpful error", {
    expect_error(upload.dataset(airquality, ws, timestamped_name),
                 sprintf("A dataset with the name '%s' already exists in AzureML", timestamped_name)
    )
  })

  test_that("Can download dataset", {
    dl <- download.datasets(ws, name=timestamped_name)
    expect_equal(dl, airquality)
  })

  test_that("Can delete dataset from workspace", {
    z <- delete.datasets(ws, timestamped_name)
    expect_true(timestamped_name %in% z$Name && z$Deleted[z$Name == timestamped_name])
    # Force refresh - sometime this fails in non-interactive
    Sys.sleep(1); refresh(ws, what = "datasets")
    ds <- datasets(ws, filter = "my")
    expect_false(timestamped_name %in% ds$Name)
  })

} else
{
  message("To run tests, add a file ~/.azureml/settings.json containing AzureML keys, see ?workspace for help")
  message("No tests ran")
}
