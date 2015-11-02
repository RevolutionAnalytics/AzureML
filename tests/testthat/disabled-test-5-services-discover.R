# source("AzureML/tests/testthat/0-config.R")
# if(interactive()) library(testthat)
#
# test_that("Can discover endpoints starting from workspace ID", {
#  skip_on_cran() # requires internet connection
#
#
#  webservices <- getWebServices(testID, testAuth)
#  Sys.sleep(1)
#  testWS <- getWSDetails(testID, testAuth, webservices[[1]]$Id)
#  Sys.sleep(1)
#  endpoints <- getEndpoints(testID, testAuth, testWS$Id)
#  Sys.sleep(1)
#  testEP <- getEPDetails(testID, testAuth, testWS$Id, endpoints[[1]]$Name)
#
#  expect_equal(length(webservices), 1)
#  expect_equal(length(testWS), 7)
#  expect_equal(length(endpoints),1)
#  expect_equal(length(testEP), 14)
#  expect_equal(webservices[[1]]$Id, testWS$Id)
#  expect_equal(testWS$Id, endpoints[[1]]$WorkspaceId)
#  expect_equal(endpoints[[1]]$WebServiceId, testEP$WebServiceId)
#  expect_equal(endpoints[[1]]$Name, testEP$Name)
# })
#
#
# test_that("API location is returned and able to be used immediately", {
#  skip_on_cran() # requires internet connection
#
#  webservices <- getWebServices(testID, testAuth)
#  Sys.sleep(1)
#  endpoints <- getEndpoints(testID, testAuth, webservices[[1]]$Id)
#  Sys.sleep(1)
#  response <- consumeDataTable(endpoints[[1]]$PrimaryKey, endpoints[[1]]$ApiLocation, list("FlowerId", "SepalLength", "SepalWidth", "PetalLength", "PetalWidth", "Species"), list(1, 6.5, 5.5, 3.5, 4.5, 0))
#
#  expect_true(is.data.frame(response))
#  expect_equal(as.numeric(response[1,8]), 1)
# })
#
#
# test_that("Discovery function handles error correctly", {
#  skip_on_cran()
#  expect_error(getWebServices("foo", testAuth), "InvalidWorkspaceIdInvalid workspace ID provided. Verify the workspace ID is correct and try again.")
# })
