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

