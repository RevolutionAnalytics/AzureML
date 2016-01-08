## ---- eval=FALSE---------------------------------------------------------
#  library(AzureML)
#  ws <- workspace(
#    id = "your workspace ID",
#    auth = "your authorization token"
#  )

## ------------------------------------------------------------------------
library('AzureML')
ws <- workspace()
ws

## ------------------------------------------------------------------------
head(datasets(ws))     # Or, equivalently: head(ws$datasets)

## ------------------------------------------------------------------------
head(ws$datasets$Owner, n=20)

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
  name = "AzureML-vignette-silly",
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
(helpPageUrl <- api$HelpLocation)

## ----update--------------------------------------------------------------
api <- updateWebService(
  ws,
  fun = function(x, y) x - y,
  inputSchema = list(
    x = "numeric",
    y = "numeric"
  ),
  outputSchema = list(
    ans = "numeric"
  ),
  serviceId = api$WebServiceId   # <<-- Required to update!
)

## ----webservice----------------------------------------------------------
(webservices <- services(ws, name = "AzureML-vignette-silly"))

## ----endpoints-----------------------------------------------------------
ep <- endpoints(ws, webservices[1, ])
class(ep)
names(ep)

## ----df------------------------------------------------------------------
df <- data.frame(
  x = 1:5,
  y = 6:10
)
s <- services(ws, name = "AzureML-vignette-silly")
s <- tail(s, 1) # use the last published function, in case of duplicate function names
ep <- endpoints(ws, s)
consume(ep, df)

## ----delete--------------------------------------------------------------
deleteWebService(ws, name = "AzureML-vignette-silly")

## ------------------------------------------------------------------------
library(AzureML)
library(MASS)
library(gbm)

ws <- workspace()
test <- Boston[1:5, 1:13]

set.seed(123)
gbm1 <- gbm(medv ~ .,
            distribution = "gaussian",
            n.trees = 5000,
            interaction.depth = 8,
            n.minobsinnode = 1,
            shrinkage = 0.01,
            cv.folds = 5,
            data = Boston,
            n.cores = 1) # You can set this to n.cores = NULL to use all cores
best.iter <- gbm.perf(gbm1, method="cv", plot=FALSE)

mypredict <- function(newdata)
{
  require(gbm)
  predict(gbm1, newdata, best.iter)
}

# Example use of the prediction function
print(mypredict(test))

# Publish the service
ep <- publishWebService(ws = ws, fun = mypredict, name = "AzureML-vignette-gbm",
                        inputSchema = test)

# Consume test data, comparing with result above
print(consume(ep, test))

