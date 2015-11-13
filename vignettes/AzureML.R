## ----train---------------------------------------------------------------
add <- function(x, y) {
  x + y
}

## ----publish, eval=FALSE-------------------------------------------------
#  ws <- workspace()
#  api <- publishWebService(
#    ws,
#    fun = "add",
#    name = "add",
#    inputSchema = list(
#      x = "numeric",
#      y = "numeric"
#    ),
#    outputSchema = list(
#      z = "numeric"
#    )
#  )

## ----access, eval=FALSE--------------------------------------------------
#  class(api)
#  names(api)

## ----help, eval=FALSE----------------------------------------------------
#  helpPageUrl <- api$HelpLocation
#  helpPageUrl

## ----update, eval=FALSE--------------------------------------------------
#  api <- updatehWebService(
#    ws,
#    fun = "add",
#    name = "add",
#    inputSchema = list(
#      x = "numeric",
#      y = "numeric"
#    ),
#    outputSchema = list(
#      z = "numeric"
#    )
#    wsid = api$WorkspaceId   # <<-- Note you must add wsid to update!
#  )

## ----webservice, eval=FALSE----------------------------------------------
#  webservices <- services(ws, name = "add")

## ----endpoints, eval=FALSE-----------------------------------------------
#  ep <- endpoints(ws, webservices[1, ]$Id)
#  class(ep)
#  names(ep)

## ----discover, eval=FALSE------------------------------------------------
#  discoverSchema(ep$HelpLocation)

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

