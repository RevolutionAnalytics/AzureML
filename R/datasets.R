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

# XXX make this an S3 method for the "Datasets" class?
#' Download one or more datasets from an AzureML workspace.
#'
#' Download one or more datasets from an AzureML workspace into local R data frame or raw binary objects.
#' @param dataset Either one or more rows from a \code{\link{datasets}} data frame in a workspace,
#'  or just a workspace from \code{\link{workspace}}. When \code{source} is a workspace, then
#'  the \code{name} parameter must also be specified.
#' @param name Optional character vector of one or more dataset names to filter the \code{datasets}
#'   parameter list by.
#' @param ... Optional arguments to pass to \code{read.table} for CSV or TSV DataTypeIds or to
#' \code{readBin} for the ZIP DataTypeId. For example,
#' specify \code{stringsAsFactors=TRUE} if you wish, or any other valid argument to \code{read.table}.
#' @return If one dataset is specified (that is, one row from a workspace \code{datasets} data frame),
#' then a single data frame is returned.
#' If more than one dataset is specified (more than one row), then a list of data frames is returned.
#' @note TSV- and CSV-formatted datasets return data frame results with \code{stringsAsFactors=FALSE}
#' by default (independently of the global \code{stringsAsFactors} option).
#'
#' This function can download datasets with various CSV and TSV "DataTypeIds", or "DataTypeId"
#' of "ARFF", "PlainText" or "ZIP". Other "DataTypeIds" return an error. See the AzureML Data Format
#' Conversion modules to convert data to a supported format. Data with DataTypeId "ZIP" are returned
#' in a raw binary R vector, which could then be passed through \code{unzip}, for example.
#' @seealso \code{\link{workspace}}, \code{\link{datasets}}, \code{\link{read.table}},
#' \code{\link{download.intermediate.dataset}}
#' @export
#' @example inst/examples/example_download.R

download.datasets <- function(dataset, name, ...)
{
 
  # Common functionality refactored here: 
  processDatasets <- function()   # mydatasets object gets instantiated in the main 
                                  # body of the function.
  {
    if(!all(c("DownloadLocation", "DataTypeId", "Name") %in% names(mydatasets))) {
      stop("`datasets` does not contain AzureML Datasets. See ?datasets for help.")
    }
    
    ans = lapply(1:nrow(mydatasets), function(j) get_dataset(mydatasets[j,], ...) )
    if(length(ans)==1) return(ans[[1]])
    names(ans) = mydatasets$Name
    return(ans) 
  }
  
  # Case 1:  1st arg (dataset) is ws, 2nd arg (name) is character vector
  # Case 2:  1st arg is a Datasets object
  # Case 3:  arg is a Datasets object (subset of datasets(ws))
  
  # Note: name is expected to be a vector of character strings
  if(missing(dataset)) stop("Must specify at least a dataset argument: see help file for `download.datasets`")

  ## Case 3
  if(missing(name))   
  {
    mydatasets <- dataset     # same as datasets(ws) above -- this is the argument in this case
    
    processDatasets()
  } else {
  
      ## Case 1   # OK
    if(is.Workspace(dataset) && is.character(name)) {     #  && inherits(name, "character") ?
      ws <- dataset   # make it clear it is a workspace
      mydatasets = datasets(ws)
      mydatasets <- mydatasets[mydatasets$Name %in% name, ]
      
      processDatasets()
    }     
    ## Case 2    # Get the error if this case is added
    else if ((is(dataset) == "Datasets") && is.character(name)) {   # && inherits(name, "character") ?
        mydatasets <- dataset     # same as datasets(ws) above -- this is the argument in this case
        mydatasets <- mydatasets[mydatasets$Name %in% name, ]   # duplicate code -- factor out?
  
        processDatasets()
        
    }
  
    else {
        stop("Argument types incorrect: see help file for `download.datasets`")
    }
    
  }
}

#' Download a dataset from an AzureML experiment module.
#' 
#' Allows you to download the data from certain types of modules in AzureML experiments. You can generate the information required from AzureML Studio by (right) clicking on a module output port and selecting the option "Generate Data Access Code...".
#'
#' @inheritParams refresh
#' 
#' @param experiment AzureML experiment ID.
#' @param node_id Experiment node ID.
#' @param port_name Experiment port name. The default is "Results dataset".
#' @param data_type_id Experiment data type id. The default is "GenericCSV". See the note below for other types.
#' @param ... Optional arguments to pass to \code{read.table} for CSV or TSV DataTypeIds. For example, specify \code{stringsAsFactors=TRUE} if you wish, or any other valid argument to \code{read.table}.
#' 
#' @return In most cases a data frame. Exceptions are: a raw vector for \code{DataTypeId="Zip"} and character vector for \code{DataTypeId="PlainText"}
#' 
#' @note TSV- and CSV-formatted datasets return data frame results with \code{stringsAsFactors=FALSE} by default (independently of the global \code{stringsAsFactors} option).
#' 
#' \bold{Supported DataTypeId options}
#' 
#' 
#' This function can download datasets with various CSV and TSV \code{DataTypeId} (with or without headers), in addition to "ARFF", "PlainText" and "Zip". Other "DataTypeIds" return an error. See the AzureML Data Format Conversion modules to convert data to a supported format.
#' 
#' @seealso \code{\link{workspace}}, \code{\link{datasets}}, \code{\link[utils]{read.table}} and \code{\link{download.datasets}}
#' 
#' @importFrom curl curl_escape new_handle handle_setheaders
#' @importFrom jsonlite toJSON
#' 
#' @export
#' @family dataset functions
#' @family experiment functions
download.intermediate.dataset <- function(ws, experiment, node_id, 
                                          port_name = "Results dataset", 
                                          data_type_id = "GenericCSV", ...)
{
  url = sprintf("%s/workspaces/%s/experiments/%s/outputdata/%s/%s",
                ws$.studioapi, curl_escape(ws$id),
                curl_escape(experiment), curl_escape(node_id),
                curl_escape(port_name))
  h = new_handle()
  handle_setheaders(h, .list=ws$.headers)
  get_dataset(list(DataTypeId = data_type_id, DownloadLocation = url), h, ...)
}


#' Upload an R data frame to an AzureML workspace.
#'
#' Upload any R data frame to an AzureML workspace using the \code{GenericTSV} format.
#' 
#' @inheritParams refresh
#' @param x An R data frame object
#' @param name A character name for the new AzureML dataset (may not match an existing dataset name)
#' @param description An optional character description of the dataset
#' @param family_id An optional AzureML family identifier
#' @param ... Optional additional options passed to \code{write.table}
#' @note The additional \code{\link[utils]{write.table}} options may not include \code{sep} or \code{row.names} or \code{file}, but any other options are accepted.
#' The AzureML API does not support uploads for _replacing_ datasets with new data by re-using a name. If you need to do this, first delete the dataset from the AzureML Studio interface, then upload a new version.
#' 
#' @return A single-row data frame of "Datasets" class that corresponds to the uploaded object now available in ws$datasets.
#' @importFrom curl curl_escape new_handle handle_setheaders handle_reset handle_setopt curl_fetch_memory
#' @importFrom jsonlite fromJSON
#' @export
#' @family dataset functions
#' @example inst/examples/example_upload.R
upload.dataset <- function(x, ws, name, description = "", family_id="", ...)
{
  stopIfNotWorkspace(ws)
  if(name %in% datasets(ws)$Name) {
    msg <- sprintf("A dataset with the name '%s' already exists in AzureML", name)
    stop(msg)
  }
  # Uploading data to AzureML is a two-step process.
  # 1. Upload raw data, retrieving an ID.
  # 2. Construct a DataSource metadata JSON object describing the data and
  #    upload that.
  
  # Step 1
  tsv = capture.output(write.table(x, file = "", sep = "\t", row.names = FALSE, ...))
  url = sprintf("%s/resourceuploads/workspaces/%s/?userStorage=true&dataTypeId=GenericTSV",
                ws$.studioapi, curl_escape(ws$id))
  h = new_handle()
  hdr = ws$.headers
  hdr["Content-Type"] = "text/plain"
  handle_setheaders(h, .list=hdr)
  body = charToRaw(paste(tsv, collapse="\n"))
  handle_setopt(h, post=TRUE, postfieldsize=length(body), postfields=body)
  step1 = try_fetch(url, handle=h)
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
                ws$.studioapi, curl_escape(ws$id))
  handle_reset(h)                               # Preserves connection, cookies
  handle_setheaders(h, .list=ws$.headers)
  body = charToRaw(paste(metadata, collapse="\n"))
  handle_setopt(h, post=TRUE, postfieldsize=length(body), postfields=body)
  step2 = try_fetch(url, handle=h)
  if(step2$status_code != 200) stop("HTTP ", step2$status_code, " ", rawToChar(step2$content))
  id = gsub("\\\"","",rawToChar(step2$content))
  
  # Success, refresh datasets
  refresh(ws, "datasets")
  
  # Return the row of ws$datasets corresponding to the uploaded data
  ws$datasets[ws$datasets$Id == id, ]
}



#' Delete datasets from an AzureML workspace.
#'
#' @inheritParams refresh
#' @param name Either one or more \code{Dataset} objects (rows from the workspace \code{datasets} data frame), or a character vector of dataset names to delete.
#' @param host AzureML delete service endpoint
#' @return A data frame with columns Name, Deleted, status_code indicating the HTTP status code and success/failure result of the delete operation for each dataset.
#' @family dataset functions
#' @export
delete.datasets <- function(ws, name, host){
  stopIfNotWorkspace(ws)
  # https://studioapi.azureml.net/api/workspaces/<workspaceId>/datasources/family/<familyId> HTTP/1.1
  datasets <-  name
  refresh(ws, "datasets")
  if(!inherits(datasets, "Datasets")){
    datasets <-  datasets(ws)
    datasets <-  datasets[datasets$Name %in% name, ]
  }
  h <- new_handle()
  handle_setheaders(h, .list = ws$.headers)
  handle_setopt(h, customrequest = "DELETE")
  delete_one <- function(familyId){
    uri <- sprintf("%s/workspaces/%s/datasources/family/%s", 
                   ws$.studioapi,
                   curl_escape(ws$id),
                   curl_escape(familyId)
    )
    z <- try_fetch(uri, h, tries = 3, delay = 2)
    z$status_code
  }
  status_code <- vapply(datasets$FamilyId, delete_one, FUN.VALUE = numeric(1), USE.NAMES = FALSE)
  ans = data.frame(
    Name        = datasets$Name, 
    Deleted     = status_code < 300, 
    status_code = status_code,
    stringsAsFactors = FALSE
  )
  refresh(ws, "datasets")
  ans
}
