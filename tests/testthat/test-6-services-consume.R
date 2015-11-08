context("Consume API")

schemaUrl <- "https://studio.azureml.net/apihelp/workspaces/f5e8e9bc4eed4034b78567449cfca779/webservices/d42667a354e34a3f98888ba86300fc2f/endpoints/b4caf0f0ebfd451bbc187741894e213b/score"



key <- "qh1cUv695D29eQkRV+zor8VTOWcEoxVTjMZWA4H7X0o8NEAUHZM13CHjOoqRNRGzXgQPxHMw6607YKI0vbhRxA=="


test_that("discoverSchema returns help page information", {
  url <- "https://ussouthcentral.services.azureml.net/workspaces/f5e8e9bc4eed4034b78567449cfca779/services/b4caf0f0ebfd451bbc187741894e213b/execute?api-version=2.0&format=swagger"

  capture.output(schema <- discoverSchema(schemaUrl))
  schema$sampleInput$Gender <- "male"
  schema$sampleInput$PortEmbarkation <- "C"

  expect_equal(length(schema), 4)
  expect_equivalent(schema$requestUrl, url)
  expect_equivalent(schema$columnNames, 
                    list("Survived", "PassengerClass", "Gender", "Age", 
                      "SiblingSpouse", "ParentChild", "FarePrice", "PortEmbarkation"))
  expect_equivalent(schema$sampleInput, 
                    list(Survived = 1, 
                         PassengerClass = 1, 
                         Gender = "male", 
                         Age = 1, 
                         SiblingSpouse = 1, 
                         ParentChild = 1, 
                         FarePrice = 1, 
                         PortEmbarkation = "C"))
})


test_that("consume a list, non-R web function", {
  capture.output(schema <- discoverSchema(schemaUrl))
  schema$sampleInput$Gender <- "male"
  schema$sampleInput$PortEmbarkation <- "C"

  capture.output(response <- consume(endpoint=list(PrimaryKey=key, 
                                                   ApiLocation=schema$requestUrl),
                                     schema$sampleInput))
  expect_equal(as.numeric(response[1, 1]), 1)
  expect_equal(as.numeric(response[1, 2]), 0.875)
})


test_that("consume a data frame, non-R web service", {
  capture.output(schema <- discoverSchema(schemaUrl))
  schema$sampleInput$Gender <- "male"
  schema$sampleInput$PortEmbarkation <- "C"

  df <- data.frame(Survived = "1", 
                   PassengerClass = "1", 
                   Gender = "male", 
                   Age = 1, 
                   SiblingSpouse = 1, 
                   ParentChild = 1, 
                   FarePrice = 1, 
                   PortEmbarkation = "C")
  capture.output(response <- consume(endpoint=list(PrimaryKey=key, 
                                                   ApiLocation=schema$requestUrl), 
                                     df))
  expect_that(as.numeric(response[1,1]), equals(1))
  expect_that(as.numeric(response[1,2]), equals(.875))
})
