if(interactive()) library("testthat")

keyfile <- system.file("tests/testthat/config.json", package = "azureml")
if(keyfile == ""){
  message("To run tests, add a file tests/testthat/config.R containing AzureML keys")
  message("No tests ran")
} else {
  jsondata <- jsonlite::fromJSON(keyfile)
  list2env(jsondata, envir = globalenv())
  
  #  ------------------------------------------------------------------------
  
  context("Read dataset")
  
  test_that("Can read dataset from workspace", {
    
    ws <- workspace(workspace_id, authorization_token)
    
    expect_is(ws, c("Workspace", "R6"))
    expect_equal(names(ws), c(".__enclos_env__", "clone", "initialize"))
    
    # This gets a dataset from AML
    ds <- datasets(ws)
    expect_is(ds, c("Datasets", "R6"))
    
    dsl <- as.list(ds)
    expect_is(dsl, "list")
    expect_true(length(dsl) > 0)
    
    nds <- names(dsl)
    expect_is(nds, "character")
    expect_true(length(nds) == length(dsl))
    
    idx <- match("Iris Two Class Data", names(datasets(ws)))
    irisaz <- datasets(ws)[[idx]]
    expect_is(irisaz, c("SourceDataset", "Dataset", "R6"))
    
    irisdf <- as.data.frame(irisaz)
    expect_is(irisdf, "data.frame")
    expect_equal(dim(irisdf), c(100, 5))
    
    irisdf <- NULL
    irisaz = datasets(ws)[["Iris Two Class Data"]]
    irisdf <- as.data.frame(irisaz)
    expect_is(irisdf, "data.frame")
    expect_equal(dim(irisdf), c(100, 5))
  })
  
  # Update to AML
  
  ## create
  ## update
  
}

