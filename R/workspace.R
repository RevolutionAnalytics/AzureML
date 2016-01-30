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



default_api <- function(api_endpoint = "https://studioapi.azureml.net"){
  x <- api_endpoint
  
  api_endpoint <- function(x){
    switch(x, 
           "https://studio.azureml.net"     = "https://studioapi.azureml.net",
           "https://studioapi.azureml.net"  = "https://studioapi.azureml.net",
           "https://studio.azureml-int.net"    = "https://studio.azureml-int.net",
           "https://studioapi.azureml-int.net" = "https://studio.azureml-int.net",
           x
    )
  }
  
  mgt_api <- function(x){
    if(api_endpoint(x) == "https://studio.azureml-int.net")
      "https://management.azureml-int.net"
    else
      sub("studio(.*?).azureml(.*?).net", "management.azureml.net", x)
  }
  
  defaults <- list(
    api_endpoint        = api_endpoint(x),
    management_endpoint = mgt_api(x),
    studioapi           = if(api_endpoint(x) == "https://studio.azureml-int.net")
      "https://studioapi.azureml-int.net/api"
    else
      paste0(api_endpoint(x), "/api")
  )
  defaults
}

#' Create a reference to an AzureML Studio workspace.
#'
#' Create a reference to an AzureML Studio workspace, returning a \code{Workspace} object that is an R environment containing details and data associated with the AzureML work space. Data corresponding to services, experiments, and datasets in the workspace are cached in the result object environment. See \code{\link{refresh}} about updating cached data.
#' 
#' 
#' @section Finding your AzureML credentials:
#' You can find your Azure Machine Learning \code{workspace id} and \code{authorization token} in the Azure Machine Learning Studio interface.
#' 
#' Workspace ID
#' 
#' \if{html}{\figure{workspace_id.png}{options: width="60\%" alt="Figure: workspace_id.png"}}
#' \if{latex}{\figure{workspaceId.pdf}{options: width=7cm}}
#' 
#' 
#' 
#' Authorization token
#' 
#' \if{html}{\figure{authorization_token.png}{options: width="60\%" alt="Figure: authorization_token.png"}}
#' \if{latex}{\figure{authorizationToken.pdf}{options: width=7cm}}
#'
#' @section Using a \code{settings.json} file:
#'  If any of the \code{id}, \code{auth}, \code{api_endpoint} or \code{management_endpoint} arguments are missing, the function attempts to read values from the \code{config} file with JSON format:
#'  \preformatted{
#'  {"workspace":{
#'    "id": "enter your AzureML workspace id here",
#'    "authorization_token": "enter your AzureML authorization token here",
#'    "api_endpoint": "https://studioapi.azureml.net",
#'    "management_endpoint": "https://management.azureml.net"
#'  }}
#' }

#' 
#' 
#' @param id Optional workspace id from ML studio -> settings -> WORKSPACE ID. See the section "Finding your AzureML credentials" for more details.
#' @param auth Optional authorization token from ML studio -> settings -> AUTHORIZATION TOKENS. See the section "Finding your AzureML credentials" for more details.
#' @param api_endpoint Optional AzureML API web service URI. Defaults to \code{https://studioap.azureml.net} if not provided and not specified in config.  See note.
#' @param management_endpoint Optional AzureML management web service URI. Defaults to \code{https://management.azureml.net} if not provided and not specified in config.  See note.
#' @param config Optional settings file containing id and authorization info. Used if any of the other arguments are missing. The default config file is \code{~/.azureml/settings.json}, but you can change this location by setting \code{options(AzureML.config = "newlocation")}. See the section "Using a settings.json file" for more details.
#' @param ... ignored
#' @param .validate If TRUE, makes a request to the AzureML API to retrieve some data. This validates whether the workspace id and authorization token are valid. Specifically, the function calls \code{\link{datasets}}. This should normally be set to TRUE. Set this to FALSE for testing, or if you know that your credentials are correct and you don't want to retrieve the datasets.
#'
#' 
#' 
#' 
#' @return An R environment of class \code{Workspace} containing at least the following objects:
#' \itemize{
#'   \item{experiments: Collection of experiments in the workspace represented as an \code{Experiments} object. See \code{\link{experiments}}}
#'   \item{datasets: Collection of datasets in the workspace represented as a \code{Datasets} object. See \code{\link{datasets}}}
#'   \item{services: Collection of web services in the workspace represented as a \code{Services} object. See \code{\link{services}}}
#' }
#' 
#' @importFrom jsonlite fromJSON
#' @export
#' @family dataset functions
#' @family experiment functions
#' @family discovery functions
#' @family consumption functions
#' @family publishing functions
#' @seealso \code{\link{datasets}}, \code{\link{experiments}}, \code{\link{refresh}},
#'          \code{\link{services}}, \code{\link{consume}}, \code{\link{publishWebService}}
workspace <- function(id, auth, api_endpoint, management_endpoint,
                      config = getOption("AzureML.config"), ..., .validate = TRUE)
{
  
  args <- list(...)
  if(!is.null(args$validate)) {
    message("You used an argument 'validate'. Did you mean '.validate'?\nIgnoring this argument.")
  }
  
  
  # If workspace_id or auth are missing, read from config. Stop if unavailable.
  if(missing(id) || missing(auth)) {
    x <- validate.AzureML.config(config, stopOnError = TRUE)
    if(inherits(x, "error")) stop(x$message)
    settings <- read.AzureML.config(config)
    
    if(missing(id)){
      id <- settings[["workspace"]][["id"]]
    }
    if(missing(auth)){
      auth <- settings[["workspace"]][["authorization_token"]]
    }
  }
  
  # If workspace_id or auth are missing, read from config, if available.
  if(missing(api_endpoint) || missing(management_endpoint)){
    x <- try(validate.AzureML.config(config, stopOnError = FALSE))
    if(!inherits(x, "error")){
      settings <- read.AzureML.config(config)
      
      if(missing(api_endpoint)){
        api_endpoint <- settings[["workspace"]][["api_endpoint"]]
      }
      if(missing(management_endpoint)){
        management_endpoint <- settings[["workspace"]][["management_endpoint"]]
      }
    }
  }
  
  # Assign a default api_endpoint if this was not provided
  default_api <- if(missing(api_endpoint) || is.null(api_endpoint)) {
    default_api()
  } else {
    default_api(api_endpoint)
  }
  if(missing(api_endpoint) || is.null(api_endpoint)){
    api_endpoint <- default_api[["api_endpoint"]]
  }
  
  # Assign a default management_endpoint if this was not provided
  if(missing(management_endpoint) || is.null(management_endpoint)){
    management_endpoint <- default_api[["management_endpoint"]]
  }
  
  # Test to see if api_endpoint is a valid url
  if(.validate){
    resp <- tryCatch(
      suppressWarnings(curl::curl_fetch_memory(api_endpoint)),
      error = function(e)e
    )
    if(inherits(resp, "error")) stop("Invalid api_endpoint: ", api_endpoint)
    
    # Test to see if management_endpoint is a valid url
    resp <- tryCatch(
      suppressWarnings(curl::curl_fetch_memory(management_endpoint)),
      error = function(e)e
    )
    if(inherits(resp, "error")) stop("Invalid management_endpoint: ", management_endpoint)
  }
  
  # It seems all checks passed. Now construct the Workspace object
  
  e <- new.env()
  class(e) <- "Workspace"
  e$id <- id
  e$.auth <- auth
  e$.api_endpoint <- api_endpoint
  e$.management_endpoint <- management_endpoint
  e$.studioapi <- default_api[["studioapi"]]
  e$.headers <- list(
    `User-Agent` = "R",
    `Content-Type` = "application/json;charset=UTF8",
    `x-ms-client-session-id` = "DefaultSession",
    `x-ms-metaanalytics-authorizationtoken` = auth
  )
  delayedAssign("experiments", get_experiments(e), assign.env = e)
  delayedAssign("datasets", get_datasets(e), assign.env = e)
  delayedAssign("services", services(e), assign.env = e)
  
  if(.validate){
    d <- get_datasets(e)
    e$datasets <- d
  }
  
  e
}


#'  Refresh data in an AzureML workspace object.
#'
#' Contact the AzureML web service and refresh/update data in an AzureML workspace object.
#' 
#' @param ws An AzureML workspace reference returned by \code{\link{workspace}}.
#' @param what Select "everything" to update all cached data, or other values to selectively update those values.
#' 
#' @return NULL is invisibly returned--this function updates data in the \code{w} environment.
#' @seealso \code{\link{workspace}}
#' @export
refresh <- function(ws, what=c("everything", "datasets", "experiments", "services"))
{
  what = match.arg(what)
  if(what %in% c("everything", "experiments")) ws$experiments <-  get_experiments(ws)
  if(what %in% c("everything", "datasets")) ws$datasets <- get_datasets(ws)
  if(what %in% c("everything", "services")) ws$services <- services(ws)
  invisible()
}

#' List datasets in an AzureML workspace.
#'
#' List datasets in an AzureML workspace, optionally filtering on sample or my datasets.
#' 
#' @inheritParams refresh
#' @param filter Optionally filter result, returing all, mine, or sample datasets.
#' 
#' @return A data.frame with class \code{Datasets} listing available datasets in the workspace.
#' @note \code{datasets(w)} is equivalent to \code{w$datasets}. Since \code{w$datasets} is simply
#' an R data.frame, you can alternatively filter on any variable as desired.
#' @seealso \code{\link{workspace}}, \code{\link{experiments}}, \code{\link{download.datasets}}
#' 
#' @export
#' @family dataset functions
#' @example inst/examples/example_datasets.R
datasets <- function(ws, filter=c("all", "my datasets", "samples"))
{
  stopIfNotWorkspace(ws)
  filter = match.arg(filter)
  if(filter == "all") return(suppressWarnings(ws$datasets))
  samples = filter == "samples"
  i = grep(paste("^", ws$id, sep=""), ws$datasets[,"Id"], invert=samples)
  ws$datasets[i, ]
}

#' List experiments in an AzureML workspace.
#'
#' List experiments in an AzureML workspace, optionally filtering on sample or my experiments.
#' 
#' @inheritParams datasets
#' 
#' @return A data.frame with class \code{Experiments} listing available experiments in the workspace.
#' @note \code{experiments(w)} is equivalent to \code{w$experiments}. Since \code{w$experiments} is simply an R data.frame, you can alternatively filter on any variable as desired.
#' @seealso \code{\link{workspace}}, \code{\link{datasets}}, \code{\link{download.intermediate.dataset}}
#' 
#' @export
#' @family experiment functions
#' @example inst/examples/example_experiments.R
experiments <- function(ws, filter=c("all", "my datasets", "samples"))
{
  filter = match.arg(filter)
  if(filter == "all") return(suppressWarnings(ws$experiments))
  samples = filter == "samples"
  i = grep(paste("^", ws$id, sep=""), ws$experiments[,"ExperimentId"], invert=samples)
  ws$experiments[i, ]
}
