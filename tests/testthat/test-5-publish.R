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
  
  timestamped_name <- paste0("webservice-test-publish-", 
                             format(Sys.time(), format="%Y-%m-%d--%H-%M-%S"))
  
  expect_error({
    endpoint <- publishWebService(ws, 
                                  fun = "add", 
                                  name = timestamped_name, 
                                  inputSchema = list(x="numeric", 
                                                     y="numeric"), 
                                  outputSchema = list(ans="numeric")
    )
    if(is.Endpoint(endpoint)) deleteWebService(ws, timestamped_name)
  }, 
  "You must specify 'fun' as a function, not a character"
  )
  
  
  
  
  timestamped_name <- paste0("webservice-test-publish-", 
                             format(Sys.time(), format="%Y-%m-%d--%H-%M-%S"))
  
  endpoint <- publishWebService(ws, 
                                fun = add, 
                                name = timestamped_name, 
                                inputSchema = list(x="numeric", 
                                                   y="numeric"), 
                                outputSchema = list(ans="numeric"))
  
  
  expect_is(endpoint, "data.frame")
  expect_is(endpoint, "Endpoint")
  expect_is(endpoint$WorkspaceId, "character")
  expect_is(endpoint$WebServiceId, "character")
  expect_equal(ws$id, endpoint$WorkspaceId)
  
  # Wait 15 seconds to allow the AzureML server to finish whatever it's doing
  Sys.sleep(3)
  
  # Now test if we can consume the service we just published
  res <- consume(endpoint, list(x=pi, y=2), retryDelay = 2)
  expect_is(res, "data.frame")
  expect_equal(res$ans, pi + 2, tolerance = 1e-5)
  
  deleteWebService(ws, timestamped_name)
})


test_that("publishWebService works with data frame input", {
  ws <- workspace()

  timestamped_name <- paste0("webservice-test-publish-", 
                             format(Sys.time(), format="%Y-%m-%d--%H-%M-%S"))

  library(lme4)
  set.seed(1)
  train <- sleepstudy[sample(nrow(sleepstudy), 120),]
  m <- lm(Reaction ~ Days + Subject, data = train)
  
  # Deine a prediction function to publish based on the model:
  sleepyPredict <- function(newdata){
    predict(m, newdata=newdata)
  }
  
  ws <- workspace()
  endpoint <- publishWebService(ws, fun = sleepyPredict, name=timestamped_name,
                          inputSchema = sleepstudy,
                          data.frame=TRUE)
  

  expect_is(endpoint, "data.frame")
  expect_is(endpoint, "Endpoint")
  expect_is(endpoint$WorkspaceId, "character")
  expect_is(endpoint$WebServiceId, "character")
  expect_equal(ws$id, endpoint$WorkspaceId)


  # Wait 15 seconds to allow the AzureML server to finish whatever it's doing
  Sys.sleep(3)
  
  # Now test if we can consume the service we just published
  res <- consume(endpoint, sleepstudy, retryDelay = 2)$ans
  expect_is(res, "numeric")
  expect_equal(length(res), nrow(sleepstudy))

  deleteWebService(ws, timestamped_name)
})


