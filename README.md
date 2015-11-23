# AzureML

An R interface to [AzureML](https://studio.azureml.net/) experiments, datasets, and web services.

Use this package to upload and download datasets to and from AzureML, to interrogate experiments, to publish new R-based web services, and to run R data through existing web services and retrieve the output.


# Installation instructions

Install the development version of the package directly from GitHub with:

```r
# Install devtools
if(!require("devtools")) install.packages("devtools")
devtools::install_github("RevolutionAnalytics/AzureML")
```

The package has dependencies on the following R packages:

- `jsonlite`
- `curl`
- `miniCRAN`
- `base64enc`
- `uuid`

In addition, you need a zip utility installed and your path must include the location of this zip utility.  On Linux machines this is usually included by default.  However, on Windows, you may have to install this yourself, e.g. by installing RTools and editing your path to include the RTools location.


# Overview

This package provides an interface to publish web services on Microsoft Azure Machine Learning (Azure ML) from your local R environment. The main functions in the package cover:

- Workspace: connect to and manage AzureML workspaces
- Datasets: upload and download datasets to and from AzureML workspaces
- Publish: define a custom function or train a model and publish it as an Azure Web Service
- Consume: use available web services from R in a variety of convenient formats


# System requirements

To publish web services, you need to have an external zip utility installed. This utility should be in the available in the path. See `?zip` for more details.

On windows, it's sufficient to install [RTools](https://cran.r-project.org/bin/windows/Rtools/).

Note: the utility should be called `zip`, since `zip()` looks for a file called `zip` in the path. Thus, `publishWebservice()` may fail, even if you have a program like `7-zip` installed.

# Wiki

The [project wiki](https://github.com/RevolutionAnalytics/AzureML/wiki) contains additional information, such as [bug bash instructions](https://github.com/RevolutionAnalytics/AzureML/wiki/Bug-bash-instructions)


# Vignette

See the package vignette and help documentation for examples and more information.

You can view the vignette at [Getting Started with the AzureML Package](https://htmlpreview.github.io/?https://github.com/RevolutionAnalytics/AzureML/blob/master/vignettes/getting_started.html).


# Bug reports

This is a technology preview. The APIs used by the package are still subject to change. Please report any issues at the [github issue tracker](https://github.com/RevolutionAnalytics/AzureML/issues).
