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


#' Create a reference to an AzureML Studio workspace.
#'
#' Create a reference to an AzureML Studio workspace. Data corresponding to experiments and datasets in the workspace are cached in the result.
#' See \code{\link{refresh}} to update cached data.
#' 
#' @param id Optional workspace id from ML studio -> settings -> WORKSPACE ID
#' @param auth Optional authorization token from ML studio -> settings -> AUTHORIZATION TOKENS
#' @param api_endpoint Optional AzureML API web service URI
#' @param management_endpoint Optional AzureML management web service URI
#' @param config Optional settings file containing id and authorization info. Only used if \code{id} and \code{auth} are missing. The default config file is \code{~/.azureml/settings.json}.
#'
#' @note If the \code{id} and \code{auth} parameters are missing, the function attempts to read values from the \code{config} file, JSON formatted as shown in \url{https://github.com/RevolutionAnalytics/azureml/issues/13}.
#' 
#' @return An R environment of class "Workspace" containing at least the following objects:
#' \describe{
#'   \item{experiments}{Collection of experiments in the workspace represented as an "Experiments" data.frame}
#'   \item{datasets}{Collection of datasets in the workspace represented as a "Datasets" data.frame}
#' }
#' @importFrom jsonlite fromJSON
#' @export
#' @family dataset functions
#' @family experiment functions
#' @seealso \code{\link{datasets}}, \code{\link{experiments}}, \code{\link{refresh}}
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
    id = s$id
    auth = s$authorization_token
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
refresh = function(ws, what=c("everything", "datasets", "experiments"))
{
  what = match.arg(what)
  if(what %in% c("everything", "experiments")) ws$experiments = get_experiments(ws)
  if(what %in% c("everything", "datasets")) ws$datasets    = get_datasets(ws)
  invisible()
}

#' Return datasets in an AzureML workspace.
#'
#' Return datasets in an AzureML workspace, optionally filtering on sample or my datasets.
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
#' @example inst\examples\example_datasets.R
datasets = function(ws, filter=c("all", "my datasets", "samples"))
{
  filter = match.arg(filter)
  if(filter == "all") return(ws$datasets)
  samples = filter == "samples"
  i = grep(paste("^", ws$id, sep=""), ws$datasets[,"Id"], invert=samples)
  ws$datasets[i, ]
}

#' Return experiments in an AzureML workspace.
#'
#' Return experiments in an AzureML workspace, optionally filtering on sample or my experiments.
#' 
#' @inheritParams datasets
#' 
#' @return A data.frame with class \code{Experiments} listing available experiments in the workspace.
#' @note \code{experiments(w)} is equivalent to \code{w$experiments}. Since \code{w$experiments} is simply an R data.frame, you can alternatively filter on any variable as desired.
#' @seealso \code{\link{workspace}}, \code{\link{datasets}}, \code{\link{download.intermediate.dataset}}
#' 
#' @export
#' @family experiment functions
#' @example inst\examples\example_experiments.R
experiments = function(ws, filter=c("all", "my datasets", "samples"))
{
  filter = match.arg(filter)
  if(filter == "all") return(ws$experiments)
  samples = filter == "samples"
  i = grep(paste("^", ws$id, sep=""), ws$experiments[,"ExperimentId"], invert=samples)
  ws$experiments[i, ]
}

# XXX make this an S3 method for the "Datasets" class?
#' Download one or more datasets from an AzureML workspace.
#'
#' Download one or more datasets from an AzureML workspace into local R data.frame objects.
#' @param datasets One or more rows from a \code{datasets} data.frame in a workspace.
#' @param name Optional character vector of one or more dataset names to filter the \code{datasets}
#'   parameter list by.
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
#' \code{\link{download.intermediate.dataset}}
#' @export
download.datasets = function(datasets, name, ...)
{
  # Coerce to data.frame, if for example presented as a list.
  if(is.null(dim(datasets))) datasets = as.data.frame(datasets)
  if(!all(c("DownloadLocation", "DataTypeId", "Name") %in% names(datasets)))
  {
    stop("`datasets` does not contain AzureML Datasets. See ?datasets for help.")
  }
  # check for dataset name filter
  if(!missing(name)) datasets = datasets[datasets$Name %in% name, ]
  ans = lapply(1:nrow(datasets), function(j) get_dataset(datasets[j,], ...))
  if(length(ans)==1) return(ans[[1]])
  names(ans) = datasets$Name
  ans
}

#' Download a dataset from an AzureML experiment port.
#'
#' @inheritParams datasets
#' 
#' @param experiment AzureML experiment ID obtained from "Generate Data Access Code"
#' @param node_id Experiment node ID obtained from "Generate Data Access Code"
#' @param port_name Experiment port name obtained from "Generate Data Access Code"
#' @param data_type_id Experiment data type id obtained from "Generate Data Access Code"
#' @param ... Optional arguments to pass to \code{read.table} for CSV or TSV DataTypeIds. For example, specify \code{stringsAsFactors=TRUE} if you wish, or any other valid argument to \code{read.table}.
#' 
#' @return A data.frame.
#' @note TSV- and CSV-formatted datasets return data.frame results with \code{stringsAsFactors=FALSE}
#' by default (independently of the global \code{stringsAsFactors} option).
#'
#' This function can download datasets with various CSV and TSV "DataTypeIds", or "DataTypeId"
#' of "ARFF" or "PlainText". Other "DataTypeIds" return an error. See the AzureML Data Format
#' Conversion modules to convert data to a supported format.
#' @seealso \code{\link{workspace}}, \code{\link{datasets}}, \code{\link[utils]{read.table}}
#' \code{\link{download.datasets}}
#' @importFrom curl curl_escape new_handle handle_setheaders
#' @importFrom jsonlite toJSON
#' @export
#' @family dataset functions
#' @family experiment functions
download.intermediate.dataset = function(ws, experiment, node_id, port_name, data_type_id, ...)
{
  url = sprintf("%s/workspaces/%s/experiments/%s/outputdata/%s/%s",
                ws$.baseuri, curl_escape(ws$id),
                curl_escape(experiment), curl_escape(node_id),
                curl_escape(port_name))
  h = new_handle()
  handle_setheaders(h, .list=ws$.headers)
  get_dataset(list(DataTypeId="GenericTSV", DownloadLocation=url), h, ...)
}


#' Upload an R data.frame to an AzureML workspace.
#'
#' Upload any R data.frame to an AzureML workspace using the \code{GenericTSV} format.
#' 
#' @inheritParams datasets

#' @param x An R data.frame object
#' @param name A character name for the new AzureML dataset (may not match an existing dataset name)
#' @param description An optional character description of the dataset
#' @param family_id An optional AzureML family identifier
#' @param ... Optional additional options passed to \code{write.table}
#' @note The additional \code{\link[utils]{write.table}} options may not include \code{sep} or \code{row.names} or \code{file}, but any other options are accepted.
#' The AzureML API does not support uploads for _replacing_ datasets with new data by re-using a name. If you need to do this, first delete the dataset from the AzureML Studio interface, then upload a new version.
#' 
#' @return A single-row data.frame of "Datasets" class that corresponds to the uploaded object now available in ws$datasets.
#' @importFrom curl curl_escape new_handle handle_setheaders handle_reset handle_setopt curl_fetch_memory
#' @importFrom jsonlite fromJSON
#' @export
#' @family dataset functions
upload.dataset = function(x, ws, name, description="", family_id="", ...)
{
  # Uploading data to AzureML is a two-step process.
  # 1. Upload raw data, retrieving an ID.
  # 2. Construct a DataSource metadata JSON object describing the data and
  #    upload that.

  # Step 1
  tsv = capture.output(write.table(x, file="", sep="\t", row.names=FALSE, ...))
  url = sprintf("%s/resourceuploads/workspaces/%s/?userStorage=true&dataTypeId=GenericTSV",
                ws$.baseuri, curl_escape(ws$id))
  h = new_handle()
  hdr = ws$.headers
  hdr["Content-Type"] = "text/plain"
  handle_setheaders(h, .list=hdr)
  body = charToRaw(paste(tsv, collapse="\n"))
  handle_setopt(h, post=TRUE, postfieldsize=length(body), postfields=body)
  step1 = curl_fetch_memory(url, handle=h)
  # Check for error (see ?curl_fetch_memory)
  if(step1$status_code != 200) stop("HTTP ", step1$status_code, rawToChar(step1$content))
  # Parse the response
  step1 = fromJSON(rawToChar(step1$content))

  # Step 2
   metadata = toJSON(
     list(
       DataSource =
         list(
           Name =  name,
           DataTypeId = "GenericTSV",
           Description = description,
           FamilyId = family_id,
           Owner =  "R",
           SourceOrigin = "FromResourceUpload"),
        UploadId = step1$Id,                    # From Step 1
        UploadedFromFileName = "",
        ClientPoll =  TRUE), auto_unbox=TRUE)

  url = sprintf("%s/workspaces/%s/datasources",
                ws$.baseuri, curl_escape(ws$id))
  handle_reset(h)                               # Preserves connection, cookies
  handle_setheaders(h, .list=ws$.headers)
  body = charToRaw(paste(metadata, collapse="\n"))
  handle_setopt(h, post=TRUE, postfieldsize=length(body), postfields=body)
  step2 = curl_fetch_memory(url, handle=h)
  if(step2$status_code != 200) stop("HTTP ", step2$status_code, " ", rawToChar(step2$content))
  id = gsub("\\\"","",rawToChar(step2$content))

  # Success, refresh datasets
  refresh(ws, "datasets")

  # Return the row of ws$datasets corresponding to the uploaded data
  ws$datasets[ws$datasets$Id == id, ]
}
