if(interactive()) library(testthat)

settingsFile <- "~/.azureml/settings.json" 
if(file.exists(settingsFile))
{
  test_that("discoverSchema returns help page information", {
    
    schemaUrl <- "https://studio.azureml.net/apihelp/workspaces/dd01c7e4a424432c9a9f83142d5cfec4/webservices/5abf96d700af4af3a4bf44160b6d3064/endpoints/9c09b5f4d6224afcbfef7368e98a7676"
    expect_equal(
      getDetailsFromUrl(schemaUrl),
      c("dd01c7e4a424432c9a9f83142d5cfec4", 
        "5abf96d700af4af3a4bf44160b6d3064",
        "9c09b5f4d6224afcbfef7368e98a7676")
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
  
  
  test_that("Can discover endpoints starting from workspace ID", {
    
    ws <- workspace()
    # refresh(ws)
    ss <- services(ws)
    
    idx <- tail(which(ss$Name == "addme"), 1) # Use the last published
    sid <- ss$Id[idx]
    
    testWS <- services(ws, sid)
    ep <- endpoints(ws, sid)
    testEP <- endpoints(ws, sid, ep$Name)
    
    expect_true(nrow(ss) >= 1)
    expect_equal(length(testWS), 6)
    expect_equal(length(endpoints),1)
    expect_equal(length(testEP), 13)
    expect_equal(ss$Id[idx], testWS$Id)
    expect_equal(testWS$Id, ep$WebServiceId)
    expect_equal(ep$WebServiceId, testEP$WebServiceId)
    expect_equal(ep$Name, testEP$Name)
  })
  
  
  test_that("API location is returned and able to be used immediately", {
    ws <- workspace()
    # refresh(ws)
    ss <- services(ws)
    
    idx <- tail(which(ss$Name == "addme"), 1) # Use the last published
    sid <- ss$Id[idx]
    ep <- endpoints(ws, sid)
    testEP <- endpoints(ws, sid, ep$Name)
    res <- consume(testEP, list(x=pi, y=2))
    expect_is(res, "data.frame")
    expect_equal(as.numeric(res$ans), pi + 2, tolerance = 1e-5)
  })
  
  
  test_that("Discovery function handles error correctly", {
    ws <- workspace()
    expect_warning(
      services(ws, "foo-does-not-exist") 
    )
  })
  
} else
{
  message("To run tests, add a file ~/.azureml/settings.json containing AzureML keys, see ?workspace for help")
  message("No tests ran")
}
