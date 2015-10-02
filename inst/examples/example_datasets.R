keyfile <- system.file("tests/testthat/config.json", package = "azureml")
jsondata <- jsonlite::fromJSON(keyfile)
list2env(jsondata, envir = globalenv())



# Access dataset ----------------------------------------------------------

library(azureml)

# workspace_id <- ""
# authorization_token <- ""
dataset <- "Book Reviews from Amazon"

ws <- workspace(
  id = workspace_id,
  authorization.token = authorization_token
)
ds <- datasets(ws)[[dataset]]
frame <- as.data.frame(ds)
head(frame)


# Access module output from experiment ------------------------------------

library(azureml)

# workspace_id <- ""
# authorization_token <- ""
experiment <- "dd01c7e4a424432c9a9f83142d5cfec4.f-id.d2f351dd4cec4c06a4592ac83f7af55a"
node_id <- '2a472ae1-ecb1-4f40-ae4e-cd3cecb1003f-268'

ws <- workspace(
  id = workspace_id,
  authorization.token = authorization_token
)
ex = experiments(ws)[[experiment]]
ds <- dataset(ex, node = node_id, port = "Results dataset", data.type = "GenericCSV")
frame <- as.data.frame(ds)
head(frame)

