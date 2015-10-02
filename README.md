# azureml
Interface to Azure ML

# Installation instructions

The `azureml` package is not yet available on CRAN.

To install the package and all its dependencies, try:

```r
# Install devtools
if(!require("devtools")) install.packages("devtools")

# Install webapi dependencies ---------------------------------------------

if(!require("httr")) install.packages("httr")
if(!require("jsonlite")) install.packages("jsonlite")
if(!require("RCurl")) install.packages("RCurl")

devtools::install_github("RevolutionAnalytics/webapi")


# Install azureml dependencies --------------------------------------------

if(!require("R6")) install.packages("R6")
if(!require("stringi")) install.packages("stringi")
if(!require("uuid")) install.packages("uuid")

devtools::install_github("RevolutionAnalytics/azureml")
```
