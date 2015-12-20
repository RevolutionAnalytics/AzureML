if(interactive()) library(testthat)

context("Discover API")

settingsFile <- "~/.azureml/settings.json" 
if(file.exists(settingsFile))
{
  
  ws <- workspace()
  
  test_that("discoverSchema() returns help page information", {
    
    schemaUrl <- "https://studio.azureml.net/apihelp/workspaces/xxxxx/webservices/yyyyy/endpoints/zzzzz"
    expect_equal(
      getDetailsFromUrl(schemaUrl),
      c("xxxxx", 
        "yyyyy",
        "zzzzz")
    )
    
    schemaUrl <- "https://studio.azureml.net/apihelp/workspaces/f5e8e9bc4eed4034b78567449cfca779/webservices/d42667a354e34a3f98888ba86300fc2f/endpoints/b4caf0f0ebfd451bbc187741894e213b/score"
    
    expect_equal(
      getDetailsFromUrl(schemaUrl),
      c("f5e8e9bc4eed4034b78567449cfca779", 
        "d42667a354e34a3f98888ba86300fc2f",
        "b4caf0f0ebfd451bbc187741894e213b")
    )
    
    url <- "https://ussouthcentral.services.azureml.net/workspaces/f5e8e9bc4eed4034b78567449cfca779/services/b4caf0f0ebfd451bbc187741894e213b/execute?api-version=2.0&format=swagger"
    expect_error(
      getDetailsFromUrl(url)
    )
    
    capture.output(schema <- discoverSchema(schemaUrl))
    schema$sampleInput$Gender <- "male"
    schema$sampleInput$PortEmbarkation <- "C"
    
    expect_equal(length(schema), 4)
    expect_equivalent(schema$requestUrl, url)
    expect_equivalent(schema$columnNames, 
                      list("Survived", 
                           "PassengerClass", 
                           "Gender", 
                           "Age", 
                           "SiblingSpouse", 
                           "ParentChild", 
                           "FarePrice", 
                           "PortEmbarkation")
    )
    expect_equivalent(schema$sampleInput, 
                      list(Survived = 1, 
                           PassengerClass = 1, 
                           Gender = "male", 
                           Age = 1, 
                           SiblingSpouse = 1, 
                           ParentChild = 1, 
                           FarePrice = 1, 
                           PortEmbarkation = "C"))
  })
  
  timestamped_name <- paste0("webservice-test-publish-", 
                             format(Sys.time(), format="%Y-%m-%d--%H-%M-%S"))
  
  
  test_that("discoverSchema() can discover endpoints starting from workspace ID", {
    
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
    expect_equal(length(testWS), 6)
    expect_equal(length(endpoints), 1)
    
    expect_identical(testEP_1, testEP_2)
    expect_equal(length(testEP_1), 13)
    expect_equal(testWS$Id, testEP_1$WebServiceId)
  })
  
  
  test_that("API location is returned and able to be used immediately", {
    ss <- services(ws, name = timestamped_name)
    testEP <- endpoints(ws, ss)[1, ]
    res <- consume(testEP, list(x=pi, y=2), retryDelay = 2)
    expect_is(res, "data.frame")
    expect_equal(res$ans, pi + 2, tolerance = 1e-5)
  })
  
  deleteWebService(ws, timestamped_name)
  
  test_that("Discovery function handles error correctly", {
    expect_warning(services(ws, "foo-does-not-exist"))
  })
  
} else
{
  message("To run tests, add a file ~/.azureml/settings.json containing AzureML keys, see ?workspace for help")
  message("No tests ran")
}