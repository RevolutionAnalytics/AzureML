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


# Used in experiment date parsing
date_origin = "1970-1-1"

# internal utility
urlconcat = function(a,b)
{
  ans = paste(gsub("/$", "", a), b, sep="/")
  ans = gsub(":/([^/])", "://\\1", ans)
  ans
}

#' Internal function that retrieves datasets.
#'
#' @param ws A workspace object
#' @return a data.frame
#' @importFrom curl handle_setheaders curl new_handle
#' @importFrom jsonlite fromJSON
#' @keywords Internal
get_datasets = function(ws)
{
  h = new_handle()
  handle_setheaders(h, .list=ws$.headers)
  r = curl(sprintf("%s/workspaces/%s/datasources", ws$.baseuri, ws$id), handle=h)
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

#' Internal function that retrieves experiments.
#'
#' @param ws A workspace object
#' @return a data.frame
#' @importFrom curl handle_setheaders curl new_handle
#' @importFrom jsonlite fromJSON
#' @keywords Internal
get_experiments = function(ws)
{
  h = new_handle()
  handle_setheaders(h, .list=ws$.headers)
  r = curl(sprintf("%s/workspaces/%s/experiments", ws$.baseuri, ws$id), handle=h)
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

#' Internal function that retrieves a dataset from AzureML.
#'
#' @param x a list or data.frame with \code{DownloadLocation} and \code{DataTypeId} fields
#' @param h optional curl handle
#' @param ... additional parameters to pass to \code{read.table}
#' @return a data.frame
#' @importFrom foreign read.arff
#' @importFrom curl new_handle curl
#' @keywords Internal
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
     stop("unsupported data type: '",x$DataTypeId,"'")
   )
}
