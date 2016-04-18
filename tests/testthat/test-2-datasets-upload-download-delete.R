if(interactive()) library("testthat")

settingsFile <- AzureML.config.default

context("Upload and delete dataset")

test_that("datasets(ws) returns results", {
  AzureML:::skip_if_missing_config(settingsFile)
  
  ws <<- workspace()
  
  x <- datasets(ws)
  expect_is(x, "data.frame")
})

timestamped_name <- paste0("dataset-test-upload-",
                           format(Sys.time(), format="%Y-%m-%d--%H-%M-%S"))

test_that("Can upload dataset to workspace", {
  AzureML:::skip_if_missing_config(settingsFile)
  upload.dataset(airquality, ws, timestamped_name)
  ds <- datasets(ws, filter = "my")
  expect_true(timestamped_name %in% ds$Name)
})

test_that("Uploading dataset with duplicate name gives helpful error", {
  AzureML:::skip_if_missing_config(settingsFile)
  expect_error(upload.dataset(airquality, ws, timestamped_name),
               sprintf("A dataset with the name '%s' already exists in AzureML", timestamped_name)
  )
})

test_that("Can download dataset", {
  AzureML:::skip_if_missing_config(settingsFile)
  dl <- download.datasets(ws, name = timestamped_name)
  expect_equal(dl, airquality)
})

test_that("Can delete dataset from workspace", {
  AzureML:::skip_if_missing_config(settingsFile)
  z <- delete.datasets(ws, timestamped_name)
  expect_true(timestamped_name %in% z$Name && z$Deleted[z$Name == timestamped_name])
  # Force refresh - sometime this fails in non-interactive
  Sys.sleep(1); refresh(ws, what = "datasets")
  ds <- datasets(ws, filter = "my")
  expect_true(nrow(ds) == 0 || !timestamped_name %in% ds$Name)
})

### Additional tests of download.datasets(.).  We could expand the same dataset formats
### to other tests (upload, delete, etc).

## csv and .tsv files:
test_that("Can download .csv and .tsv files", {
#  ws <- workspace()  # defined globally in 1st test above
  datasets(ws)
  names <- c("Blood donation data", "Energy Efficiency Regression data", "MNIST Test 10k 28x28 dense")
  
  # Case 1, as described in help description for download.datasets(.)
  frame1_mult <- download.datasets(ws, names)
  expect_equal(names(frame1_mult), names)
  
  # Case 2, as described in help description for download.datasets(.)
  frame2_mult <- download.datasets(datasets(ws), names)
  expect_equal(names(frame2_mult), names)
  
  # Case 3, as described in help description for download.datasets(.)
  d <- datasets(ws) 
  frame3_mult <- download.datasets(subset(d, Name %in% names))    
  expect_equal(names(frame3_mult), names)                       
})

test_that("Can download .zip files", {
#  ws <- workspace()  # defined globally in 1st test above
  datasets(ws)
  zip_names <- c("text.preprocessing.zip", "fraudTemplateUtil.zip")
  
  # Case 1, as described in help description for download.datasets(.)
  zipframe1_mult <- download.datasets(ws, zip_names) 
  expect_equal(names(zipframe1_mult), names)
  
  # Case 2, as described in help description for download.datasets(.)
  zipframe2_mult <- download.datasets(datasets(ws), zip_names)
  expect_equal(names(zipframe2_mult), names)
  
  # Case 3, as described in help description for download.datasets(.)
  d <- datasets(ws) 
  zipframe3_mult <- download.datasets(subset(d, Name %in% zip_names))    
  expect_equal(names(zipframe3_mult), names)
})

test_that("Can download .arff files", {
#  ws <- workspace()  # defined globally in 1st test above
  datasets(ws)
  arf_names <- c("Breast cancer data", "Forest fires data", "Iris Two Class Data")
  
  # Case 1, as described in help description for download.datasets(.)
  arf_frame1_mult <- download.datasets(ws, arf_names)
  expect_equal(names(arf_frame1_mult), names)
  
  # Case 2, as described in help description for download.datasets(.)
  arf_frame2_mult <- download.datasets(datasets(ws), arf_names)
  expect_equal(names(arf_frame2_mult), names)
  
  # Case 3, as described in help description for download.datasets(.)
  d <- datasets(ws) 
  arf_frame3_mult <- download.datasets(subset(d, Name %in% arf_names))    
  expect_equal(names(arf_frame3_mult), names)
})



