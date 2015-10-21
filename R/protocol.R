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

#' datasets Return datasets in an AzureML workspace
#'
#' Return datasets in an AzureML workspace, optionally filtering on sample or my
#' datasets.
#' @param w An AzureML workspace reference returned by \code{\link{workspace}}.
#' @param filter Optionally filter result, returing all, mine, or sample datasets.
#' @return A data.frame with class \code{Datasets} listing available datasets in the workspace.
#' @note \code{datasets(w)} is equivalent to \code{w$datasets}. Since \code{w$datasets} is simply
#' an R data.frame, you can alternatively filter on any variable as desired.
#' @seealso \code{\link{workspace}}, \code{\link{experiments}}
#' @export
datasets = function(w, filter=c("all", "my datasets", "samples"))
{
  filter = match.arg(filter)
  if(filter == "all") return(w$datasets)
  samples = filter == "samples"
  i = grep(paste("^", w$id, sep=""), w$datasets[,"Id"], invert=samples)
  w$datasets[i, ]
}

#' experiments Return experiments in an AzureML workspace
#'
#' Return experiments in an AzureML workspace, optionally filtering on sample or my
#' experiments.
#' @param w An AzureML workspace reference returned by \code{\link{workspace}}.
#' @param filter Optionally filter result, returing all, mine, or sample experiments.
#' @return A data.frame with class \code{Experiments} listing available experiments in the workspace.
#' @note \code{experiments(w)} is equivalent to \code{w$experiments}. Since \code{w$experiments} is
#' simply an R data.frame, you can alternatively filter on any variable as desired.
#' @seealso \code{\link{workspace}}, \code{\link{datasets}}
#' @export
experiments = function(w, filter=c("all", "my datasets", "samples"))
{
  filter = match.arg(filter)
  if(filter == "all") return(w$experiments)
  samples = filter == "samples"
  i = grep(paste("^", w$id, sep=""), w$experiments[,"ExperimentId"], invert=samples)
  w$experiments[i, ]
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
#' \code{\link{download.datasets}}
#' @importFrom curl curl_escape new_handle handle_setheaders
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
