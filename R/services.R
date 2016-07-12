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


#' List Available Web Services.
#'
#' Return a list of web services available to the specified Microsoft Azure Machine Learning workspace.
#' The result is cached in the workspace environment similarly to datasets and experiments.
#'
#' @inheritParams refresh
#' @param service_id optional web service id. If supplied, return the web service information for just the specified service id. Leave undefined to return a data.frame of all services.
#' @param name optional web service name. If supplied, return the web service information for services with matching names. Leave undefined to return all services.
#' @param host the AzureML web services URI
#' 
#' @return Returns a data.frame with variables:
#' \itemize{
#'   \item Id
#'   \item Name
#'   \item Description
#'   \item CreationTime
#'   \item WorkspaceId
#'   \item DefaultEndpointName
#' }
#' Each row of the returned data.frame corresponds to a service.
#' @note \code{getWebServices} is an alias for \code{services}.
#' @family discovery functions
#' @examples
#' \dontrun{
#' workspace_id <- ""          # Your AzureML workspace id
#' authorization_token <- ""   # Your AzureML authorization token
#'
#' ws <- workspace(
#'   id = workspace_id,
#'   auth = authorization_token
#' )
#'
#' # Equivalent:
#' services(ws)
#' getWebServices(ws)
#' }
#' @export
services <- function(ws, service_id, name, host = ws$.management_endpoint)
{
  stopIfNotWorkspace(ws)
  h <- new_handle()
  headers <- c(ws$.headers,
               `Authorization` = sprintf("Bearer %s",ws$.auth),
               `Accept` = "application/json"
  )
  handle_setheaders(h, .list = headers)
  
  if(missing(service_id)) {
    service_id <- ""
  } else {
    service_id <- sprintf("/%s", service_id)
  }
  
  uri <- sprintf("%s/workspaces/%s/webservices%s", host, ws$id, service_id)
  r <- try_fetch(uri = uri, handle = h, delay = 0.25, .retry = 3)
  #   if(inherits(r, "error")){
  #     msg <- paste("No results returned from datasets(ws).", 
  #                  "Please check your workspace credentials and api_endpoint are correct.")
  #     stop(msg)
  #   }
  
  ans <- fromJSON(rawToChar(r$content))
  if(inherits(ans, "error") || is.null(ans)) {
    msg <- "service not found"
    warning(msg, immediate. = TRUE)
    return(simpleError(msg))
  }
  
  attr(ans, "workspace") = ws
  if(!missing(name)) {
    ans = ans[ans$Name == name,]
  }
  if(is.null(ans)) ans = data.frame()
  class(ans) = c("Service", "data.frame")
  # Cache the result in the workspace
  if(service_id == "") ws$services = ans
  ans
}


#' @rdname services
#' @export
getWebServices <- services

#' List AzureML Web Service Endpoints
#'
#' Return a list of web services endpoints for the specified web service id.
#'
#' @inheritParams refresh
#' @param host The AzureML web services URI
#' @param service_id A web service Id, for example returned by \code{\link{services}}; alternatively a row from the services data frame identifying the service.
#' @param endpoint_id An optional endpoint id. If supplied, return the endpoint information for just that id. Leave undefined to return a data.frame of all end points associated with the service.
#' 
#' @return Returns a data.frame with variables:
#' \itemize{
#'  \item Name
#'  \item Description
#'  \item CreationTime
#'  \item WorkspaceId
#'  \item WebServiceId
#'  \item HelpLocation
#'  \item PrimaryKey
#'  \item SecondaryKey
#'  \item ApiLocation
#'  \item Version
#'  \item MaxConcurrentCalls
#'  \item DiagnosticsTraceLevel
#'  \item ThrottleLevel
#'  }
#' Each row of the data.frame corresponds to an end point.
#' @note \code{getEndPoints} is an alias for \code{endpoints}.
#' @family discovery functions
#' @examples
#' \dontrun{
#' workspace_id <- ""          # Your AzureML workspace id
#' authorization_token <- ""   # Your AsureML authorization token
#'
#' ws <- workspace(
#'   id = workspace_id,
#'   auth = authorization_token
#' )
#'
#' s <- services(ws)
#' endpoints(ws, s$Id[1])
#' 
#' # Note that you can alternatively just use the entire row that
#' # describes the service.
#' endpoints(ws, s[1,])
#'
#' # Equivalent:
#' getEndpoints(ws, s$Id[1])
#' }
#' @export
endpoints <- function(ws, service_id, endpoint_id, host = ws$.management_endpoint)
{
  stopIfNotWorkspace(ws)
  # if(is.list(service_id) || is.data.frame(service_id)) service_id = service_id$Id[1]
  if(is.Service(service_id)) service_id = service_id$Id[1]
  
  h = new_handle()
  headers = list(`User-Agent`="R",
                 `Content-Type`="application/json;charset=UTF8",
                 `Authorization`=sprintf("Bearer %s", ws$.auth),
                 `Accept`="application/json")
  handle_setheaders(h, .list=headers)
  
  if(missing(endpoint_id)) endpoint_id = ""
  else endpoint_id = sprintf("/%s", endpoint_id)
  
  # if(is.list(service_id)) service_id = service_id$Id[1]
  uri <- sprintf("%s/workspaces/%s/webservices/%s/endpoints%s", 
                 host, 
                 ws$id, 
                 service_id, 
                 endpoint_id
  )
  
  r <- try_fetch(uri, handle = h)
  ans <- fromJSON(rawToChar(r$content))
  
  
  # Adjust the returned API location for completeness:
  if(length(ans) > 0) {
    suffix <- "/execute?api-version=2.0&details=true&format=swagger"
    ans$ApiLocation <- paste0(ans$ApiLocation, suffix)
  }
  class(ans) <- c("Endpoint", "data.frame")
  ans
}

#' @rdname endpoints
#' @export
getEndpoints = endpoints


#' Display AzureML Web Service Endpoint Help Screens.
#'
#' Download and return help for the specified AzureML web service endpoint.
#'
#' @param ep an AzureML web service endpoint from the \code{\link{endpoints}} function.
#' @param type the type of help to display.
#' 
#' @return Returns the help text. If \code{type = "apidocument"}, then returns the help as a list from a parsed JSON document describing the service.
#' @family discovery functions
#' @examples
#' \dontrun{
#' ws <- workspace()
#'
#' s <- services(ws)
#' e <- endpoints(ws, s[1,])
#' endpointHelp(e)
#'
#' Particularly useful way to see expected service input and output:
#' endpointHelp(e)$definitions
#' 
#' }
#' @export
endpointHelp <- function(ep, type = c("apidocument", "r-snippet", "score", "jobs", "update"))
{
  if(!inherits(ep, "Endpoint")) stop("Object ep must be an endpoint")
  type <- match.arg(type)
  rsnip <- FALSE
  if(type == "r-snippet") {
    type <- "score"
    rsnip <- TRUE
  }
  uri <- ep$HelpLocation[1]
  
  # XXX This is totally nuts, and not documented, but help hosts vary depending on type.
  # Arrghhh...
  if(type == "apidocument"){
    uri <- gsub("studio.azureml.net/apihelp", "management.azureml.net", uri)
    uri <- gsub("studio.azureml-int.net/apihelp", "management.azureml-int.net", uri)
  }
  
  uri <- paste(uri, type, sep = "/")
  r <- try_fetch(uri, handle = new_handle())
  txt = rawToChar(r$content)
  
  pattern <- "</?\\w+((\\s+\\w+(\\s*=\\s*(?:\".*?\"|'.*?'|[^'\">\\s]+))?)+\\s*|\\s*)/?>"
  txt <- gsub(pattern, "\\1", txt)
  txt <- gsub("&.?quot;", "'", txt)
  txt <- paste(txt, collapse = "\n")
  if(rsnip) {
    txt <- substr(txt, 
                  grepRaw("code-snippet-r", txt) + nchar("code-snippet-r") + 2, nchar(txt)
    )
  }
  if(type == "apidocument"){
    fromJSON(txt)
  } else {
    txt
  }
}

endpointInputDefinition <- function(ep, input = "input1"){
  help <- endpointHelp(ep, type = "apidocument")
  help$definitions$ExecutionRequest$example$Inputs[[input]]
  help$definitions$output1Item
}
