# Copyright (c) 2015 Microsoft Corporation
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
services = function(ws, service_id, name, host = ws$.management_endpoint)
{
  if(!is.Workspace(ws)) stop("ws must be an AzureML Workspace object")
  h = new_handle()
  headers = list(`User-Agent`="R",
                 `Content-Type`="application/json;charset=UTF8",
                 `Authorization`=sprintf("Bearer %s",ws$.auth),
                 `Accept`="application/json")
  handle_setheaders(h, .list=headers)
  if(missing(service_id)) service_id = ""
  else service_id = sprintf("/%s", service_id)
  r = curl(
    sprintf("%s/workspaces/%s/webservices%s", host, ws$id, service_id), 
    handle=h
    )
  on.exit(close(r))
  ans = tryCatch(fromJSON(readLines(r, warn=FALSE)), error=function(e) NULL)
  # Cache the result in the workspace
  if(service_id == "") ws$services = ans
  if(!missing(name)) return(ans[ans$Name == name,])
  if(is.null(ans)){
    warning("No service returned. The service_id may be invalid.", immediate. = TRUE)
  }
  ans
}


#' @rdname services
#' @export
getWebServices = services


#' List AzureML Web Service Endpoints
#'
#' Return a list of web services endpoints for the specified web service id.
#'
#' @inheritParams refresh
#' @param host The AzureML web services URI
#' @param service_id A web service Id, for example returned by \code{\link{services}}.
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
endpoints = function(ws, service_id, endpoint_id, host = ws$.management_endpoint)
{
  if(!is.Workspace(ws)) stop("ws must be an AzureML Workspace object")
  h = new_handle()
  headers = list(`User-Agent`="R",
                 `Content-Type`="application/json;charset=UTF8",
                 `Authorization`=sprintf("Bearer %s",ws$.auth),
                 `Accept`="application/json")
  handle_setheaders(h, .list=headers)
  if(missing(endpoint_id)) endpoint_id = ""
  else endpoint_id = sprintf("/%s", endpoint_id)
  if(is.list(service_id)) service_id = service_id$Id[1]
  r = curl(
    sprintf("%s/workspaces/%s/webservices/%s/endpoints%s", 
            host, 
            ws$id, 
            service_id, 
            endpoint_id
    ), 
    handle=h)
  on.exit(close(r))
  ans = fromJSON(readLines(r, warn=FALSE))
  # Adjust the returned API location for completeness:
  if(length(ans)>0)
  {
    ans$ApiLocation = paste(ans$ApiLocation, 
                            "/execute?api-version=2.0&details=true&format=swagger", 
                            sep="")
  }
  ans
}

#' @rdname endpoints
#' @export
getEndpoints = endpoints


#' Display AzureML Web Service Endpoint Help Screens
#'
#' Download and display help for the specified AzureML web service endpoint.
#' The result is displayed as text using the pager.
#'
#' @param e an AzureML web service endpoint from the \code{\link{endpoints}} function.
#' @param type the type of help to display.
#' 
#' @return The help text is invisibly returned.
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
#' e <- endpoints(ws, s[1,])
#' endpointHelp(e[1,])
#' 
#' }
#' @export
endpointHelp = function(e, type = c("r-snippet","score","jobs","update"))
{
  type = match.arg(type)
  rsnip = FALSE
  if(type=="r-snippet")
  {
    type = "score"
    rsnip = TRUE
  }
  pattern = "</?\\w+((\\s+\\w+(\\s*=\\s*(?:\".*?\"|'.*?'|[^'\">\\s]+))?)+\\s*|\\s*)/?>"
  text = paste(
    gsub(
      "&.?quot;", "'", 
      gsub(pattern, "\\1", 
           readLines(
             curl(paste(e$HelpLocation[1], type, sep="/")), warn = FALSE)
      )
    ), 
    collapse="\n"
  )
  if(rsnip)
  {
    text = substr(text, 
                  grepRaw("code-snippet-r",text)+nchar("code-snippet-r")+2,nchar(text)
    )
  }
  f = tempfile()
  writeLines(text, con = f)
  file.show(f)
  unlink(f)
  invisible(text)
}
