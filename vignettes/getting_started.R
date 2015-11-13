## ---- eval=FALSE---------------------------------------------------------
#  library(AzureML)
#  ws <- workspace(
#    id = "your workspace ID",
#    auth = "your authorization token"
#  )

## ------------------------------------------------------------------------
library(AzureML)
ws <- workspace()
ws

## ------------------------------------------------------------------------
head(datasets(ws))

# Or, equivalently:
head(ws$datasets)

## ------------------------------------------------------------------------
ws$datasets$Owner

## ------------------------------------------------------------------------
airports <- download.datasets(ws, name = "Airport Codes Dataset", quote="\"")
head(airports)

## ---- echo=FALSE---------------------------------------------------------
# Remove dataset if it exists, otherwise we get an error on upload
if("Air quality" %in% datasets(ws, filter = "my")$Name){
   delete.datasets(ws, name = "Air quality")
}

## ------------------------------------------------------------------------
upload.dataset(airquality, ws, name = "Air quality")

# Let's see what we've got:
head(download.datasets(ws, name = "Air quality"))

## ------------------------------------------------------------------------
delete.datasets(ws, name="Air quality")

## ------------------------------------------------------------------------
e <- experiments(ws, filter = "samples")
head(e)

## ------------------------------------------------------------------------
refresh(ws, "experiments")

## ----train---------------------------------------------------------------
add <- function(x, y) {
  x + y
}

## ----publish-------------------------------------------------------------
ws <- workspace()
api <- publishWebService(
  ws,
  fun = add, 
  name = "add",
  inputSchema = list(
    x = "numeric", 
    y = "numeric"
  ), 
  outputSchema = list(
    ans = "numeric"
  )
)

## ----access--------------------------------------------------------------
class(api)
names(api)

## ----help----------------------------------------------------------------
helpPageUrl <- api$HelpLocation
helpPageUrl

## ----update--------------------------------------------------------------
api <- updateWebService(
  ws,
  fun = add, 
  name = "add",
  inputSchema = list(
    x = "numeric", 
    y = "numeric"
  ), 
  outputSchema = list(
    ans = "numeric"
  ),
  wsid = api$WorkspaceId   # <<-- Note you must add wsid to update!
)

## ----webservice----------------------------------------------------------
webservices <- services(ws, name = "add")

## ----endpoints-----------------------------------------------------------
ep <- endpoints(ws, webservices[1, ]$Id)
class(ep)
names(ep)

## ----discover------------------------------------------------------------
discoverSchema(ep$HelpLocation)

## ----df------------------------------------------------------------------
df <- data.frame(
  x = 1:5,
  y = 6:10
)
s <- services(ws, name = "add")
s <- tail(s, 1) # use the last published function, in case of duplicate function names
ep <- endpoints(ws, s$Id)
consume(ep, df)

