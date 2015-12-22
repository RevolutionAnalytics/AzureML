---
title: "Getting Started with the AzureML Package"
date: "2015-12-17"
output:
  html_document
vignette: >
  %\VignetteIndexEntry{Getting Started with the AzureML package}
  %\VignetteEngine{knitr::rmarkdown}
---

Use this package to upload and download datasets to and from AzureML, to
interrogate experiments, to publish R functions as AzureML web services, and to
run R data through existing web services and retrieve the output.

# Installation instructions

Install the development version of the package directly from GitHub with:

```r
# Install devtools
if(!require("devtools")) install.packages("devtools")
devtools::install_github("RevolutionAnalytics/azureml")
```

The package depends on:

- `jsonlite`
- `curl`
- `miniCRAN`
- `base64enc`
- `uuid`

Some of the examples use data and functions in:

- `lme4`
- `ggplot2`


# Overview

AzureML provides an interface to publish web services on Microsoft Azure
Machine Learning (Azure ML) from your local R environment. The main functions
in the package cover the following topics:

- Workspace: connect to and manage AzureML workspaces
- Datasets: upload and download datasets to and from AzureML workspaces
- Publish: publish R functions as AzureML web services, and update or delete existing services
- Consume: apply any AzureML web service to your R data

## Getting Started

To get started, please navigate to [AzureML Studio](https://studio.azureml.net)
and create a free account (not guest) or use your existing AzureML account.
After logging in, under the "Settings" tab, copy and paste your Workspace ID
from the "Name" sub-tab into your R console. From the "Authorization Tokens"
sub-tab, copy your Primary Authorization Token into your R console. You will
need this information to access all package functionality.

The package defines a `Workspace` class that represents an AzureML work space.
Most of the functions in the package refer to a Workspace object directly or
indirectly. Use the `workspace()` function to create Workspace objects, either
by explicitly specifying an AzureML workspace ID and authorization token.
Workspace objects are simply R environments that actively cache details about
your AzureML sessions.

## Obtaining AzureML Credentials

Before using the package, it is necessary to first obtain the security
credentials to your Azure Machine Learning workspace. You can find this be
logging in at [https://studio.azureml.net](Azure ML web site). If you do not
have an account, you can create a free account (not guest) to use these APIs.

Once logged in, you will be brought to the Studio landing page. Using the
left-hand menu, navigate to the 'Settings' tab to find your Workspace ID. Note
this, or copy it into your R session and store it is a variable, e.g. myWsID.

<img src="workspaceID.png" width="80%">

Next, within the 'Settings' tab, use the overhead menu to navigate to the
'Authorization Tokens' tab and similarly note your Primary Authorization Token.

<img src="authToken.png" width="80%">


```r
library(AzureML)
ws <- workspace(
  id = "your workspace ID",
  auth = "your authorization token"
)
```

or alternatively create a file in `~/.azureml/settings.json` with the JSON
structure (`api_endpoint` and `management_endpoint` are optional):

```json
{"workspace": {
   "id"                  : "test_id",
   "authorization_token" : "test_token",
   "api_endpoint"        : "api_endpoint",
   "management_endpoint" : "management_endpoint"
}}
```

See `?workspace` for more details.

## Examining workspace datasets, experiments and services

The `datasets()`, `experiments()`, and `services()` functions return data
frames that contain information about those objects available in the workspace.

The package caches R data frame objects describing available datasets,
experiments and services in the workspace environment. That cache can be
refreshed at any time with the `refresh()` function. The data frame objects
make it relatively easy to sort and filter the datasets, experiments, and
services in arbitrary ways.  The functions also include filtering options for
specific and common filters, like looking up something by name.

Use the `download.datasets()` and `upload.dataset()` functions to download or
upload data between R and your Azure workspace. The
`download.intermediate.dataset()` function can download ephemeral data from a
port in an experiment that is not explicitly stored in your Azure workspace.

Use `delete.datasets()` to remove and delete datasets from the workspace.

The `endpoints()` function describes Azure web service endpoints, and works with
supporting help functions like `endpointHelp()`.

The `publishWebService()` function publishes a custom R function as an AzureML
web service, available for use by any client. The `updateWebService()` and
`deleteWebServce()` update or delete existing web services, respectively.

Use the `consume()` function to evaluate an Azure ML web service with
new data uploaded to AzureML from your R environment.

# Examples

Work with the AzureML package begins by defining a workspace object. The
example below uses the configured workspace ID and authorization token in the
`~/.azureml/settings.json` file.  Alternatively specify these settings
explicitly in the `workspace()` function as outlined above. All of the examples
require this step.







































