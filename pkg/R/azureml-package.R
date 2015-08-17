#' Interface to Azure ML Studio datasets and experiments.
#' 
#' Allows you to work with Azure ML Studio datasets and experiments directly from R.
#' 
#' @section Summary of functions:
#'
#' 1. Create a link to an Azure ML workspace
#' 
#' \itemize{
#' \item Link to workspace: \code{\link{workspace}}
#' }
#'
#' 2. Datasets
#'
#' \itemize{
#'    \item Get datasets: \code{\link{datasets}}
#' }
#'
#' 3. Experiments
#'
#' \itemize{
#'    \item Get experiments: \code{\link{experiments}}
#' }
#' 
#' @import R6
#' @import purrr
#' @import webapi
#' @import stringi
#' @importFrom foreign read.arff
#' @importFrom foreign write.arff
#' @importFrom uuid UUIDgenerate
#' @importFrom jsonlite fromJSON
#' 
#' @name azureml-package
#' @aliases azureml
#' @docType package
#' @keywords package
NULL
