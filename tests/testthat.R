Sys.setenv("R_TESTS" = "")
library(testthat, quietly = TRUE)
test_check("AzureML")
