## ----train---------------------------------------------------------------
add <- function(x,y) {
  return(x+y)
}

## ----publish, eval=FALSE-------------------------------------------------
#  response <- publishWebService("add", "addOnline",
#                                list("x"="float", "y"="float"), list("z"="float"), myWsID, myAuth)

## ----access, eval=FALSE--------------------------------------------------
#  webservice <- response[[1]]
#  endpoints <- response[[2]]
#  webServiceURL <- endpoints[[1]]$ApiLocation
#  webServiceKey <- endpoints[[1]]$PrimaryKey
#  webPageHelpURL <- endpoints[[1]]$HelpLocation

## ----help, eval=FALSE----------------------------------------------------
#  helpPageUrl <- endpoints[[1]]$HelpLocation

## ----update, eval=FALSE--------------------------------------------------
#  add1 <- function(x) {
#    return(x)
#  }
#  response <- updateWebService("add1", "addOnline", webservice$Id,
#                                list("x"="float"), list("z"="float"), myWsID, myAuth)

## ----webservice, eval=FALSE----------------------------------------------
#  webservices <- getWebServices(myWsId, myAuth)

## ----endpoints, eval=FALSE-----------------------------------------------
#  endpoints <- getEndpoints(myWsId, myAuth, webservices[[1]]$Id)

## ----discover, eval=FALSE------------------------------------------------
#  schema <- discoverSchema(endpoints[[1]]$HelpLocation)

## ----file, eval=FALSE----------------------------------------------------
#  response <- consumeFile(endpoints[[1]]$PrimaryKey, endpoints[[1]]$ApiLocation, "data.csv")
#  response <- consumeFile(endpoints[[1]]$PrimaryKey, schema[[1]]$requestUrl, "data.csv")

## ----df, eval=FALSE------------------------------------------------------
#  df <- data.frame("x"=c(1,2), "y"=c(3,4))
#  response <- consumeDataframe(endpoints[[1]]$PrimaryKey, endpoints[[1]]$ApiLocation, df)
#  response <- consumeDataframe(endpoints[[1]]$PrimaryKey, schema$requestUrl, df)

## ----lists, eval=FALSE---------------------------------------------------
#  response <- consumeLists(endpoints[[1]]$PrimaryKey, endpoints[[1]]$ApiLocation,
#                           list("x"=1, "y"=2), list("x"=3, "y"=4))
#  response <- consumeLists(endpoints[[1]]$PrimaryKey, schema$requestUrl,
#                           schema$sampleInput)

