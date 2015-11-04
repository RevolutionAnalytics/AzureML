# azureml
An R interface to AzureML experiments, datasets, and web services.

Use this package to upload and download datasets to and from AzureML,
to interrogate experiments, to publish new R-based web services, and
to run R data through existing web services and retrieve the output.

# Installation instructions

Install the development version of the package directly from GitHub
with:
```r
# Install devtools
if(!require("devtools")) install.packages("devtools")
devtools::install_github("RevolutionAnalytics/azureml")
```
The package requires the `jsonlite, curl, miniCRAN,` and `uuid` packages.

# Overview

This package provides an interface to publish web services on Microsoft Azure
Machine Learning (Azure ML) from your local R environment. The main
functions in the package cover:

- Workspace: connect to and manage AzureML workspaces
- Datasets: upload and download datasets to and from AzureML workspaces
- Publish: define a custom function or train a model and publish it as an Azure Web Service
- Consume: use available web services from R in a variety of convenient formats

This is a technology preview. The APIs used by the package are still subject to
change. Please send any bugs or comments you have to the maintainers listed.

## Getting Started

To get started, please go [here](https://studio.azureml.net) and create a free
account (not guest) or use your existing Azure ML account.  After logging in,
under the "Settings" tab, copy and paste your Workspace ID from the "Name"
sub-tab into your R console. From the "Authorization Tokens" sub-tab, copy your
Primary Authorization Token into your R console. You will need this information
to access all package functionality.


