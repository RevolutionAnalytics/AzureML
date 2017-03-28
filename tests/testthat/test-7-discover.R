if(interactive()) library(testthat)

context("Discover API")
settingsFile <- "~/.azureml/settings.json" 
workspace <- function(..., .validate = FALSE) {
  # AzureML::workspace(..., .validate = .validate)
  js <- decrypt_vault()
  id <- js$workspace$id
  auth <- js$workspace$authorization_token
  AzureML::workspace(id, auth, .validate = .validate)
}

test_that("discoverSchema() can discover endpoints starting from workspace ID", {
  
  skip_if_missing_config(settingsFile)
  skip_if_offline()
  
  ws <<- workspace()
  refresh(ws, what = "services")
  timestamped_name <<- paste0("webservice-test-publish-", 
                              format(Sys.time(), format="%Y-%m-%d--%H-%M-%S"))
  
  add <- function(x, y) x + y
  
  publishWebService(ws, 
                    fun = add, 
                    name = timestamped_name, 
                    inputSchema = list(x = "numeric", 
                                       y = "numeric"), 
                    outputSchema = list(ans = "numeric")
  )
  ss <- services(ws)
  
  expect_is(ss, "Service")
  expect_is(ss, "data.frame")
  
  Sys.sleep(3)
  testWS <- services(ws, name = timestamped_name)
  testEP_1 <- endpoints(ws, service_id = testWS)
  testEP_2 <- endpoints(ws, service_id = testWS)[1, ]
  
  expect_is(testEP_1, "Endpoint")
  
  expect_true(nrow(ss) >= 1)
  expect_true(length(testWS) >= 6)
  expect_equal(length(endpoints), 1)
  
  expect_identical(testEP_1, testEP_2)
  expect_true(length(testEP_1) >= 13)
  expect_equal(testWS$Id, testEP_1$WebServiceId)
})


test_that("API location is returned and able to be used immediately", {
  skip_if_missing_config(settingsFile)
  skip_if_offline()
  
  ss <- services(ws, name = timestamped_name)
  testEP <- endpoints(ws, ss)[1, ]
  res <- consume(testEP, list(x=pi, y=2), retryDelay = 2)
  expect_is(res, "data.frame")
  expect_equal(res$ans, pi + 2, tolerance = 1e-5)

    deleteWebService(ws, timestamped_name)
})


test_that("Discovery function handles error correctly", {
  skip_if_missing_config(settingsFile)
  skip_if_offline()
  
  expect_error(
    services(ws, "foo-does-not-exist"), 
    "Invalid web service ID provided. Verify the web service ID is correct and try again."
  )
})

