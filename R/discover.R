#' List Available Web Services.
#'
#' Return a list of web services available to the specified Microsoft Azure Machine Learning workspace.
#'
#' @inheritParams datasets
#' @param uri The AzureML web services URI
#' @param service_id An optional web service id. If supplied, return the web service information for just
#' that service id. Leave undefined to return a data.frame of all services.
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
#' authorization_token <- ""   # Your AsureML authorization token
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
services = function(ws, service_id, uri="https://management-tm.azureml.net")
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
  r = curl(sprintf("%s/workspaces/%s/webservices%s", uri, ws$id, service_id), handle=h)
  on.exit(close(r))
  fromJSON(readLines(r, warn=FALSE))
}

#' @rdname services
#' @export
getWebServices = services

#' List AzureML Web Service Endpoints
#'
#' Return a list of web services endpoints for the specified web service id.
#'
#' @inheritParams datasets
#' @param uri The AzureML web services URI
#' @param service_id A web service id, for example returned by \code{\link{services}}.
#' @param endpoint_id An optional endpoint id. If supplied, return the endpoint information for just
#' that id. Leave undefined to return a data.frame of all end points associated with the service.
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
#' # Equivalent:
#' getEndpoints(ws, s$Id[1])
#' }
#' @export
endpoints = function(ws, service_id, endpoint_id, uri="https://management-tm.azureml.net")
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
  r = curl(sprintf("%s/workspaces/%s/webservices/%s/endpoints%s", uri, ws$id, service_id, endpoint_id), handle=h)
  on.exit(close(r))
  ans = fromJSON(readLines(r, warn=FALSE))
  # Adjust the returned API location for completeness:
  if(length(ans)>0)
  {
    ans$ApiLocation = paste(ans$ApiLocation, "/execute?api-version=2.0&details=true&format=swagger", sep="")
  }
  ans
}

#' @rdname endpoints
#' @export
getEndpoints = endpoints
