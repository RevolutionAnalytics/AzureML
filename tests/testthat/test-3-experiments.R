if(interactive()) library("testthat")

keyfile <- system.file("tests/testthat/config.json", package = "azureml")
if(keyfile == ""){
  message("To run tests, add a file tests/testthat/config.R containing AzureML keys")
  message("No tests ran")
} else {
  jsondata <- jsonlite::fromJSON(keyfile)
  list2env(jsondata, envir = globalenv())

  #  ------------------------------------------------------------------------
  
  context("Read dataset from experiment")
  
  test_that("Can read dataset from workspace", {
    
    # Get data from node in an experiment
    
    ws <- workspace(workspace_id, authorization_token)
    
    we <- experiments(ws)
    expect_is(we, c("Experiments", "R6"))
    el <- as.list(we)
    expect_is(el, "list")
    expect_true(length(el) > 0)
    
    en <- names(experiments(ws))
    expect_is(en, "character")
    expect_true(length(en) > 0)
    expect_true(length(en) == length(el))
    
    experiment = experiments(ws)[[exp_id]]
    class(experiment)
    expect_is(experiment, c("Experiment", "R6"))
    ds = dataset(
      x = experiment,
      node = node_id,
      port='Results dataset',
      data.type='GenericCSV')
    expect_is(ds, c("IntermediateDataset", "Dataset", "R6"))
    frame = as.data.frame(ds)
    expect_is(frame, "data.frame")
    expect_true(nrow(frame) > 0)
  })
}