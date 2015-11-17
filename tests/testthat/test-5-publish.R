if(interactive()) library(testthat)

context("Publish API")

test_that(".getexports finds function and creates zip string", {
  
  
  funEnv <- new.env()
  assign("add", function(x, y) x + y, envir = funEnv)
  
  exportEnv = new.env()
  .getexports(substitute(add), e = exportEnv, env = funEnv)
  
  expect_equal(
    ls(exportEnv), 
    "add"
  )
  
  za <- zipAvailable()
  if(!za) skip(zipNotAvailableMessage)
  expect_true(za)
  
  z <- packageEnv(exportEnv)
  expect_is(z, "character")
  expect_true(nchar(z) > 1)
  
})



test_that("publishWebService works with simple function", {
  ws <- workspace()
  add <- function(x,y) x + y
  endpoint <- publishWebService(ws, 
                                fun = add, 
                                name = "addme", 
                                inputSchema = list(x="numeric", 
                                                   y="numeric"), 
                                outputSchema = list(ans="numeric"))
  
  # Wait 15 seconds to allow the AzureML server to finish whatever it's doing
  Sys.sleep(15)
  
  expect_is(endpoint, "data.frame")
  expect_is(endpoint, "Endpoint")
  expect_is(endpoint$WorkspaceId, "character")
  expect_is(endpoint$WebServiceId, "character")
  expect_equal(ws$id, endpoint$WorkspaceId)
  
  # Now test if we can consume the service we just published
  res <- consume(endpoint, list(x=pi, y=2))
  expect_is(res, "data.frame")
  expect_equal(as.numeric(res$ans), pi + 2, tolerance = 1e-5)
})



#  # Train the model
#  model <- naiveBayes(as.factor(Species) ~., dataset)
#})


#test_that("publishWebService returns a working web service", {
#  skip_on_cran()
#  testID = ""
#  testAuth = ""
#  require(quantmod) || install.packages(quantmod)
#  library(quantmod)

#  getSymbols('MSFT')
#  data = as.data.frame(cbind(MSFT[,4],MSFT[,5]/100000))
#  model = lm(MSFT.Close~.,data=data)

#  MSFTpredict <- function(close, volume) {
#    return(predict(model, data.frame("MSFT.Close"=close, "MSFT.Volume"=volume)))
#  }

#  msftWebService <- publishWebService("MSFTpredict", "MSFTdemo", list("close"="float", "volume"="float"), list("number"="float"), testID, testAuth)
#  msftEndpoints <- msftWebService[[2]]
#  response <- consumeDataTable(msftEndpoints[[1]]["PrimaryKey"], msftEndpoints[[1]]$ApiLocation, list("close", "volume"), list(25, 300), list(30, 100))

#  expect_equal(as.numeric(response[1,1]), 1)
#  expect_equal(as.numeric(response[2,1]), 1)
#})


#test_that("publishWebService handles bad input correctly", {
#  add <- function (x, y) {
#    print("This will add x and y")
#    return(x + y)
#  }

#  expect_error(publishWebService("add", "addTest", list(), list(), testID, testAuth), "Input schema does not contain the proper input. You provided 0 inputs and 2 were expected")
#  expect_error(publishWebService("add", "addTest", list("x"="foo", "y"="bar"), list("z"="int"), testID, testAuth), "data type \"foo\" not supported")
#})


#test_that("publishWebService handles various errors correctly", {
#  add <- function (x, y) {
#    print("This will add x and y")
#    return(x + y)
#  }

#  expect_error(publishWebService("add", "add", list("x"="float", "y"="float"), list("z"="float"), "foo", "bar"), "InvalidWorkspaceIdInvalid workspace ID provided. Verify the workspace ID is correct and try again.")
#})

