if(interactive()) library(testthat)

settingsFile <- "~/.azureml/settings.json" 
if(file.exists(settingsFile))
{
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
    expect_error(services(ws, "foo-does-not-exist"), 
                 "InvalidWorkspaceIdInvalid workspace ID provided.")
  })
  
} else
{
  message("To run tests, add a file ~/.azureml/settings.json containing AzureML keys, see ?workspace for help")
  message("No tests ran")
}
