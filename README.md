# azureml
Interface to Azure ML

# Installation instructions

The `azureml` package is not yet available on CRAN.

To install the package and all its dependencies, try:

```r
# Install devtools
if(!require("devtools")) install.packages("devtools")

# Install webapi dependencies ---------------------------------------------

if(!require("jsonlite")) install.packages("jsonlite")
if(!require("curl")) install.packages("curl")
devtools::install_github("RevolutionAnalytics/azureml")
```
