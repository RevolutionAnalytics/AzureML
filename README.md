# azureml
Interface to Azure ML

# Installation instructions

The `azureml` package is not yet available on CRAN.

To install the package and all its dependencies, try:

```r
library(devtools)

# Install webapi dependencies ---------------------------------------------

if(!require("httr")) install.packages("httr")
if(!require("jsonlite")) install.packages("jsonlite")
if(!require("RCurl")) install.packages("RCurl")
# if(!require("magrittr")) install.packages("magrittr")
# if(!require("Rcpp")) install.packages("Rcpp")
# if(!require("dplyr")) install.packages("dplyr")

# install_github("Hadley/purrr")
devtools::install_github("RevolutionAnalytics/webapi")


# Install azureml dependencies --------------------------------------------

if(!require("R6")) install.packages("R6")
if(!require("stringi")) install.packages("stringi")
if(!require("uuid")) install.packages("uuid")

devtools::install_github("RevolutionAnalytics/azureml")
```
