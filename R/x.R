# experiments...
date_origin = "1970-1-1"
library(curl)
library(jsonlite)
library(foreign)

print.Workspace =  function(x)
{
  cat("AzureML Workspace\n")
  cat("Workspace ID: ",x$id,"\n")
}

print.Experiments = function(x)
{
  dots = character()
  if(nrow(x) > 0) dots = "..."
  d = data.frame(Description=substr(x[, "Description"], 1, 48),
                 CreationTime=x[, "CreationTime"],
                 `...`=dots)
  print(d)
  cat("-------------------------------------------------\n")
  cat("AzureML experiments data.frame variables include:\n")
  cat(paste(capture.output(names(x)),collapse="\n"),"\n")
  d
}

print.Datasets = function(x)
{
  dots = character()
  if(nrow(x) > 0) dots = "..."
  d = data.frame(Size=x[, "Size"], Name=substr(x[, "Name"], 1, 50), `...`=dots)
  print(d)
  cat("----------------------------------------------\n")
  cat("AzureML datasets data.frame variables include:\n")
  cat(paste(capture.output(names(x)),collapse="\n"),"\n")
  d
}

# internal utility
urlconcat = function(a,b)
{
  ans = paste(gsub("/$", "", a), b, sep="/")
  ans = gsub(":/([^/])", "://\\1", ans)
  ans
}

#' workspace Create a reference to an AzureML Studio workspace
#'
#' Create a reference to an AzureML Studio workspace. Data corresponding
#' to experiments and datasets in the workspace are cached in the result.
#' See \code{\link{refresh}} to update cached data.
#' @param id Optional workspace id from ML studio -> settings -> WORKSPACE ID
#' @param auth Optional authorization token from ML studio -> settings -> AUTHORIZATION TOKENS
#' @param api_endpoint Optional AzureML API web service URI
#' @param management_endpoint Optional AzureML management web service URI
#' @param config Optional settings file containing id and authorization info. Only used if \code{id} and \code{auth} are missing.
#'
#' @note If the \code{id} and \code{auth} parameters are missing, the function
#' attempts to read values from the \code{config} file, JSON formatted as
#' shown in \url{https://github.com/RevolutionAnalytics/azureml/issues/13}.
#' 
#' @export
#' @seealso \code{\link{datasets}}, \code{\link{experiments}}, \code{\link{refresh}}
#' 
#' @return An R environment of class "Workspace" containing at least the following objects:
#' \describe{
#'   \item{experiments}{Collection of experiments in the workspace represented as an "Experiments" data.frame}
#'   \item{datasets}{Collection of datasets in the workspace represented as a "Datasets" data.frame}
#' }
#' @importFrom jsonlite fromJSON
#' @export
workspace = function(id, auth, api_endpoint="https://studio.azureml.net",
                     management_endpoint="https://management.azureml.net",
                     config="~/.azureml/settings.json")
{
  e = new.env()
  class(e) = "Workspace"
  if(missing(id) && missing(auth))
  {
    if(!file.exists(config))  stop("missing file ",config)
    s = fromJSON(file(config))
    id = s$workspace$id
    auth = s$workspace$authorization_token
  }
  e$id = id
  e$.auth = auth
  e$.api_endpoint = api_endpoint
  e$.management_endpoint = management_endpoint
  e$.baseuri = urlconcat(api_endpoint, "api")
  e$.headers = list(`User-Agent`="R",
                    `Content-Type`="application/json;charset=UTF8",
                    `x-ms-client-session-id`="DefaultSession",
                    `x-ms-metaanalytics-authorizationtoken`=auth)
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
#' @export
refresh = function(w, what=c("everything", "datasets", "experiments"))
{
  what = match.arg(what)
  if(what %in% c("everything", "experiments")) w$experiments = get_experiments(w)
  if(what %in% c("everything", "datasets")) w$datasets    = get_datasets(w)
  invisible()
}

datasets = function(w, filter=c("all", "my datasets", "samples"))
{
  filter = match.arg(filter)
  if(filter == "all") return(w$datasets)
  i = grep(paste("^", w$id, sep=""), w$datasets[,"Id"])
  w$datasets[i, ]
}

#' get_datasets internal function that retrieves datasets
#'
#' internal function that retrieves datasets
#' @param w A workspace object
#' @return a data.frame
#' @importFrom curl handle_setheaders, curl, new_handle
#' @importFrom jsonlite fromJSON
get_datasets = function(w)
{
  h = new_handle()
  handle_setheaders(h, .list=w$.headers)
  r = curl(sprintf("%s/workspaces/%s/datasources", w$.baseuri, w$id), handle=h)
  on.exit(close(r))
  x = fromJSON(readLines(r, warn=FALSE))
  # Use strict variable name matching to look up data
  d = x[,"DownloadLocation"]
  x$DownloadLocation = paste(d[,"BaseUri"], d[,"Location"],
                             d[,"AccessCredential"], sep="")
  d = x[,"VisualizeEndPoint"]
  x$VisualizeEndPoint = paste(d[,"BaseUri"], d[,"AccessCredential"], sep="")
  d = x[,"SchemaEndPoint"]
  x$SchemaEndPoint = paste(d[,"BaseUri"], d[,"Location"],
                           d[,"AccessCredential"], sep="")
  class(x) = c("Datasets", "data.frame")
  x
}

#' get_experiments internal function that retrieves experiments
#'
#' internal function that retrieves experiments
#' @param w A workspace object
#' @return a data.frame
#' @importFrom curl handle_setheaders, curl, new_handle
#' @importFrom jsonlite fromJSON
get_experiments = function(w)
{
  h = new_handle()
  handle_setheaders(h, .list=w$.headers)
  r = curl(sprintf("%s/workspaces/%s/experiments", w$.baseuri, w$id), handle=h)
  on.exit(close(r))
  x = fromJSON(readLines(r, warn=FALSE))
  # Use strict variable name matching to look up data
  x = cbind(x, x[,"Status"])
  x$Status = c()
  x$EndTime = as.POSIXct(as.numeric(gsub("[^0-9]","",x[,"EndTime"]))/1000,
                         origin=date_origin)
  x$StartTime = as.POSIXct(as.numeric(gsub("[^0-9]","",x[,"StartTime"]))/1000,
                           origin=date_origin)
  x$CreationTime = as.POSIXct(as.numeric(gsub("[^0-9]","",
                                         x[,"CreationTime"]))/1000,
                              origin=date_origin)
  class(x) = c("Experiments", "data.frame")
  x
}

# XXX make this an S3 method for the "Datasets" class?
#' download.datasets Donwload one or more datasets from an AzureML workspace
#'
#' Download one or more datasets from an AzureML workspace into local R
#' data.frame objects.
#' @param datasets One or more rows from a \code{datasets} data.frame in a workspace.
#' @param ... Optional arguments to pass to \code{read.table} for CSV or TSV DataTypeIds. For example,
#' specify \code{stringsAsFactors=TRUE} if you wish, or any other valid argument to \code{read.table}.
#' @return If one dataset is specified (that is, one row from a workspace \code{datasets} data.frame),
#' then a single data.frame is returned.
#' If more than one dataset is specified (more than one row), then a list of data.frames is returned.
#' @note TSV- and CSV-formatted datasets return data.frame results with \code{stringsAsFactors=FALSE}
#' by default (independently of the global \code{stringsAsFactors} option).
#'
#' This function can download datasets with various CSV and TSV "DataTypeIds", or "DataTypeId"
#' of "ARFF" or "PlainText". Other "DataTypeIds" return an error. See the AzureML Data Format
#' Conversion modules to convert data to a supported format.
#' @seealso \code{\link{workspace}}, \code{\link{datasets}}, \code{\link{read.table}},
#' \code{\link{download.intermediate.datasets}}
#' @export
download.datasets = function(datasets, ...)
{
# Coerce to data.frame, if for example presented as a list.
  if(is.null(dim(datasets))) datasets = as.data.frame(datasets)
  if(!all(c("DownloadLocation", "DataTypeId") %in% names(datasets)))
  {
    stop("`datasets` does not contain AzureML Datasets. See ?datasets for help.")
  }
  ans = lapply(1:nrow(datasets), function(j) get_dataset(datasets[j,], ...))
  if(length(ans)==1) return(ans[[1]])
  names(ans) = datasets$Name
  ans
}

#' download.intermediate.dataset Donwload a dataset from an AzureML experiment port
#'
#' Donwload a dataset from an AzureML experiment node/port.
#' @param w An AzureML workspace object
#' @param experiment AzureML experiment ID obtained from "Generate Data Access Code"
#' @param node_id Experiment node ID obtained from "Generate Data Access Code"
#' @param port_name Experiment port name obtained from "Generate Data Access Code"
#' @param data_type_id Experiment data type id obtained from "Generate Data Access Code"
#' @param ... Optional arguments to pass to \code{read.table} for CSV or TSV DataTypeIds. For example,
#' specify \code{stringsAsFactors=TRUE} if you wish, or any other valid argument to \code{read.table}.
#' @return A data.frame.
#' @note TSV- and CSV-formatted datasets return data.frame results with \code{stringsAsFactors=FALSE}
#' by default (independently of the global \code{stringsAsFactors} option).
#'
#' This function can download datasets with various CSV and TSV "DataTypeIds", or "DataTypeId"
#' of "ARFF" or "PlainText". Other "DataTypeIds" return an error. See the AzureML Data Format
#' Conversion modules to convert data to a supported format.
#' @seealso \code{\link{workspace}}, \code{\link{datasets}}, \code{\link{read.table}}
#' \code{\linke{download.datasets}}
#' @importFrom curl curl_escape new_handle
#' @export
download.intermediate.dataset = function(w, experiment, node_id, port_name, data_type_id, ...)
{
  url = sprintf("%s/workspaces/%s/experiments/%s/outputdata/%s/%s",
                w$.baseuri, curl_escape(w$id),
                curl_escape(experiment), curl_escape(node_id),
                curl_escape(port_name))
  h = new_handle()
  handle_setheaders(h, .list=w$.headers)
  get_dataset(list(DataTypeId="GenericTSV", DownloadLocation=url), h, ...)
}

#experiment='a2760707c7fa4245a057680427f31b17.f-id.251398cd14cc410bb5d7d04049ae2726', node_id='5f1449ad-0c32-4fd7-8e92-ac9abec11bd0-130', port_name='Results dataset', data_type_id='GenericTSV'

#' get_dataset internal function that retrieves a dataset from AzureML
#'
#' internal function that retrieves a dataset from AzureML
#' @param x a list or data.frame with \code{DownloadLocation} and \code{DataTypeId} fields
#' @param h optional curl handle
#' @param ... additional parameters to pass to \code{read.table}
#' @return a data.frame
#' @importFrom foreign read.arff
#' @importFrom curl new_handle
get_dataset = function(x, h, ...)
{
  # Set default stringsAsFactors to FALSE, but allow users to override in ...
  # Restore the option on function exit.
  opts = options(stringsAsFactors=FALSE)
  on.exit(options(opts))
  if(missing(h)) h = new_handle()
  uri = curl(x$DownloadLocation, handle=h)
  on.exit(tryCatch(close(uri), error=invisible), add=TRUE)

   # Existence of DataTypeId, DowloadLocation guaranteed by caller
   switch(tolower(x$DataTypeId),
     arff = read.arff(uri),
     plaintext = paste(readLines(uri), collapse="\n"),
     generictsvnoheader = read.table(uri, sep="\t", header=FALSE, ...),
     generictsv = read.table(uri, sep="\t", header=TRUE, ...),
     genericcsvnoheader = read.table(uri, sep=",", header=FALSE, ...),
     genericcsv = read.table(uri, sep=",", header=TRUE, ...),
     stop("unsupported data type '",x$DataTypeId,"'")
   )
}
