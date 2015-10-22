if(interactive()) library("testthat")

keyfile <- system.file("tests/testthat/config.json", package = "azureml")
if(keyfile == ""){
  message("To run tests, add a file tests/testthat/config.R containing AzureML keys")
  message("No tests ran")
} else {
  
  keydata <- jsonlite::fromJSON(keyfile)
  exp_id <- keydata$exp_id
  node_id <- keydata$node_id
  
  context("Read dataset from experiment")
  
  test_that("Can read dataset from workspace", {
    
    ws <- workspace(config = keyfile)
    
    we <- experiments(ws)
    expect_is(we, "Experiments")
    expect_is(we, "data.frame")
    
    expect_identical(we, ws$experiments)
    
    
    en <- we$Description
    expect_is(en, "character")
    expect_true(length(en) > 0)
    
    expect_true(exp_id %in% we$ExperimentId)
    idx <- match(exp_id, we$ExperimentId)
    experiment = experiments(ws)[idx, ]
    class(experiment)
    expect_is(experiment, "Experiments")
    expect_is(experiment, "data.frame")
    
    frame = download.intermediate.datasets(ws, experiment = exp_id, node_id = node_id,
                                          port_name='Results dataset',
                                          data_type_id='GenericCSV')
    
    expect_is(frame, "data.frame")
    expect_true(nrow(frame) > 0)
  })
}