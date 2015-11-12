## ---- eval = FALSE-------------------------------------------------------
#  library(AzureML)
#  ws <- workspace(
#    id = "your workspace ID",
#    auth = "your authorization token"
#  )
#  

## ------------------------------------------------------------------------
library(AzureML)
ws <- workspace()
ws

## ------------------------------------------------------------------------
d <- datasets(ws)
head(d)

## ------------------------------------------------------------------------
airports <- download.datasets(ws, name = "Airport Codes Dataset", quote="\"")
head(airports)

## ------------------------------------------------------------------------
e <- experiments(ws, filter = "samples")
head(e)

