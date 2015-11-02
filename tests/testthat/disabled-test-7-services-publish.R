context("Publish API")



test_that("packDependencies handles recursive packaging", {
  
  assign("add", function(x, y) x + y, 
         envir = .GlobalEnv
  )
  
  results <- packDependencies("add")
  expect_equal(nchar(results[[1]]), 32)
  expect_equal(results[[2]], "")
})



test_that("publishPreprocess", {
  expect_equivalent(
    publishPreprocess(list(x="int", y="string", z="float")), 
    list(x=list("type"="integer", "format"="int32"), 
         y=list("type"="string", "format"="string"), 
         z=list("type"="number", "format"="float")))
  expect_error(publishPreprocess(list("x"="dataframe")))
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


