# This is written as a rather bogus test as it requires a specific exp_id
# which is unlikely to be generally available. This is a hard test to
# configure.
#

#if(interactive()) library("testthat")
#
#if(file.exists("~/.azureml/settings.json"))
#{
#  context("Read dataset from experiment")
#  
#  test_that("Can read dataset from workspace", {
#    
#    ws <- workspace()
#    
#    we <- experiments(ws)
#    expect_is(we, "Experiments")
#    expect_is(we, "data.frame")
#    
#    expect_identical(we, ws$experiments)
#    
#    
#    en <- we$Description
#    expect_is(en, "character")
#    expect_true(length(en) > 0)
#    
#    expect_true(exp_id %in% we$ExperimentId)
#    idx <- match(exp_id, we$ExperimentId)
#    experiment = experiments(ws)[idx, ]
#    class(experiment)
#    expect_is(experiment, "Experiments")
#    expect_is(experiment, "data.frame")
#    
#    frame = download.intermediate.dataset(ws, experiment = exp_id, node_id = node_id,
#                                          port_name='Results dataset',
#                                          data_type_id='GenericCSV')
#    
#    expect_is(frame, "data.frame")
#    expect_true(nrow(frame) > 0)
#  })
#} else {
#  message("To run tests, add a file ~/.azureml/settings.json containing AzureML keys, see ?workspace for help")
#  message("No tests ran")
#}
