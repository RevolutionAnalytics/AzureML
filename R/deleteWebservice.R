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



#' Delete a Microsoft Azure Web Service
#'
#' Delete a Microsoft Azure Machine Learning  web service from your workspace.
#'
#' @export
#'
#' @inheritParams refresh
#' @param name Either one row from the workspace \code{services} data.frame corresponding to a service to delete, or simply a service name character string.
#' @param refresh Set to \code{FALSE} to supress automatic updating of the workspace list of services,
#' useful when deleting many services in bulk.
#' @note If more than one service matches the supplied \code{name}, the first listed service will be deleted.
#' @return The updated data.frame of workspace services is invisibly returned.
#' @seealso \code{\link{services}} \code{\link{publishWebService}} \code{\link{updateWebService}}
#' @family publishing functions
#' @example inst/examples/example_publish.R
deleteWebService <- function(ws, name, refresh = TRUE)
{
  #DELETE https://management.azureml.net/workspaces/{id}/webservices/{id}[/endpoints/{name}]
  
  stopIfNotWorkspace(ws)
  if(is.data.frame(name) || is.list(name)){
    name = name$Id[1]
  } else {
    name = ws$services[ws$services$Name == name, "Id"][1]
    if(is.na(name)) stop("service not found")
  }
  h = new_handle()
  handle_setheaders(h, `Authorization`=sprintf("Bearer %s",ws$.auth), .list=ws$.headers)
  handle_setopt(h, customrequest="DELETE")
  uri = sprintf("%s/workspaces/%s/webservices/%s", 
                ws$.management_endpoint, ws$id, name)
  s = curl_fetch_memory(uri, handle=h)$status_code
  if(s > 299) stop("HTTP error ",s)
  if(refresh) refresh(ws, "services")
  invisible(ws$services)
}
