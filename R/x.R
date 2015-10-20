# experiments...
date_origin = "1970-1-1"
library(curl)
library(jsonlite)

print.Workspace =  function(x)
{
  cat("AzureML Workspace\n")
  cat("Workspace ID: ",x$id,"\n")
}

print.Datasets = function(x) print.default(x$Name)
print.Experiments = function(x) print.default(x$Description)

#' workspace Create a reference to an AzureML Studio workspace
#'
#' Create a reference to an AzureML Studio workspace. Data corresponding
#' to experiments and datasets in the workspace are cached in the result.
#' See \code{\link{refresh}} to update cached data.
#' @param id Workspace id from ML studio -> settings -> WORKSPACE ID
#' @param auth Authorization token from ML studio -> settings -> AUTHORIZATION TOKENS
#' 
#' @export
#' @seealso \code{\link{datasets}}, \code{\link{experiments}}, \code{\link{refresh}}
#' 
#' @return An R environment of class "Workspace" containing at least the following
#' objects:
#' \describe{
#'   \item{experiments}{Collection of experiments in the workspace represented as an "Experiments" data.frame}
#'   \item{datasets}{Collection of datasets in the workspace represented as a "Datasets" data.frame}
#'}
workspace = function(id, auth)
{
  e = new.env()
  class(e) = "Workspace"
  e$id = id
  e$.auth = auth
  e$.baseuri = "https://studio.azureml.net/api"
  delayedAssign("experiments", get_experiments(e), assign.env=e)
  delayedAssign("datasets", get_datasets(e), assign.env=e)
  e
}

#' refresh Refresh data in an AzureML workspace object
#'
#' Contact the AzureML web service and refresh/update data in an AzureML
#' workspace object.
#' @param w An AzureML workspace reference returned by \code{\link{workspace}}.
#' @param what Select "everything" to update all cached data, or other values
#' to selectively update those values.
#' @return NULL is invisibly returned--this function updates data in the
#' \code{w} environment.
#' @seealso \code{\link{workspace}}
refresh = function(w, what=c("everything", "datasets", "experiments"))
{
  what = match.arg(what)
  if(what %in% c("everything", "experiments")) w$experiments = get_experiments(w)
  if(what %in% c("everything", "datasets")) w$datasets    = get_datasets(w)
  invisible()
}

get_datasets = function(w)
{
  h = new_handle()
  handle_setheaders(h,
                    `User-Agent`="R",
                    `Content-Type`="application/json;charset=UTF8",
                    `x-ms-client-session-id`="DefaultSession",
                    `x-ms-metaanalytics-authorizationtoken`=w$.auth)
  r = curl(sprintf("%s/workspaces/%s/datasources", w$.baseuri, w$id), handle=h)
  x = fromJSON(readLines(r, warn=FALSE))
  close(r)
  d = x$DownloadLocation
  x$DownloadLocation = paste(d$BaseUri, d$Location, d$AccessCredential, sep="")
  d = x$VisualizeEndPoint
  x$VisualizeEndPoint = paste(d$BaseUri, d$AccessCredential, sep="")
  d = x$SchemaEndPoint
  x$SchemaEndPoint = paste(d$BaseUri, d$Location, d$AccessCredential, sep="")
  class(x) = c("Datasets", "data.frame")
  x
}

get_experiments = function(w)
{
  h = new_handle()
  handle_setheaders(h,
                    `User-Agent`="R",
                    `Content-Type`="application/json;charset=UTF8",
                    `x-ms-client-session-id`="DefaultSession",
                    `x-ms-metaanalytics-authorizationtoken`=w$.auth)
  r = curl(sprintf("%s/workspaces/%s/experiments", w$.baseuri, w$id), handle=h)
  x = fromJSON(readLines(r, warn=FALSE))
  close(r)
  x = cbind(x, x$Status)
  x$Status = c()
  x$EndTime = as.POSIXct(as.numeric(gsub("[^0-9]","",x$EndTime))/1000,origin=date_origin)
  x$StartTime = as.POSIXct(as.numeric(gsub("[^0-9]","",x$StartTime))/1000,origin=date_origin)
  x$CreationTime = as.POSIXct(as.numeric(gsub("[^0-9]","",x$CreationTime))/1000,origin=date_origin)
  class(x) = c("Experiments", "data.frame")
  x
}
