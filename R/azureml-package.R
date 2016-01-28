# Copyright (c) 2015-2016 Microsoft Corporation
# All rights reserved.
#   
# The MIT License (MIT)
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#   
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.



#' Interface to Azure ML Studio datasets and experiments.
#' 
#' Allows you to work with Azure ML Studio datasets and experiments directly from R.
#' 
#' @section Summary of functions:
#'
#' 1. Create a reference to an Azure ML workspace
#' 
#' \itemize{
#' \item Workspace: \code{\link{workspace}}
#' }
#'
#' 2. Datasets
#'
#' \itemize{
#'    \item List available datasets: \code{\link{datasets}}
#'    \item Download datasets: \code{\link{download.datasets}}
#'    \item Upload a dataset: \code{\link{upload.dataset}}
#'    \item Delete datasets: \code{\link{delete.datasets}}
#' }
#'
#' 3. Experiments
#'
#' \itemize{
#'    \item Get experiments: \code{\link{experiments}}
#'    \item Get data from an experiment port: \code{\link{download.intermediate.dataset}}
#' }
#'
#' 4. Web Services
#' 
#' \itemize{
#'    \item List available services: \code{\link{services}}
#'    \item Consume a web service (run data through it and retrieve result): \code{\link{consume}}
#'    \item Publish an R function as a web service: \code{\link{publishWebService}}
#'    \item Update an existing web service: \code{\link{updateWebService}}
#'    \item List web service endpoints: \code{\link{endpoints}}
#' }
#' 
#' 5. Configure a settings file with your AzureML secrets
#' 
#' The \code{\link{workspace}} function optionally reads your AzureML credentials from a settings file located at \code{~/.azureml/settings.json}. You can read and write this file using:
#' 
#' \itemize{
#'    \item Write: \code{\link{write.AzureML.config}}
#'    \item Read: \code{\link{read.AzureML.config}}
#' }
#' 
#' @name AzureML-package
#' @aliases AzureML
#' @docType package
#' @keywords package
NULL
