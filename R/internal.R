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

#' Try to fetch a uri/handle, retrying on certain returned status codes after a timeout
#' @param uri the uri to fetch
#' @param handle a curl handle
#' @param retry_on HTTP status codes that result in retry
#' @param tries number of tries before failing
#' @param delay in seconds between retries, subject to exponent
#' @param exponent increment each successive delay by delay^exponent
#' @return the result of curl_fetch_memory(uri, handle)
try_fetch = function(uri, handle, retry_on=c(503,504,509,400,401,440), tries=3, delay=10, exponent=1.2)
{
  i = 0
  while(i < tries)
  {
    r = curl_fetch_memory(uri, handle)
    if(!(r$status_code %in% retry_on)) return(r)
    if(i == 0)
      message(sprintf("Request failed with status %s. Retrying request...", r$status_code))
    Sys.sleep(delay)
    delay = delay^exponent
    i = i + 1
  }
  r
}

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
  x = tryCatch(fromJSON(readLines(r, warn=FALSE)), error=invisible)
  if(is.null(x) || is.na(x$Name[1]))
  {
    x = data.frame()
    class(x) = c("Datasets", "data.frame")
    return(x)
  }
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


convertToDate = function(x){
  x = as.numeric(gsub("[^-0-9]", "", x)) /1000
  x = ifelse(x >= 0, x, NA)
  suppressWarnings(
    as.POSIXct(x, tz = "GMT", origin = date_origin)
  )
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
  x$EndTime = convertToDate(x[["EndTime"]])
  x$StartTime = convertToDate(x[["StartTime"]])
  x$CreationTime = convertToDate(x[["CreationTime"]])
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
  conn = "r"
  if(tolower(x$DataTypeId) == "zip") conn = "rb"
  uri = curl(x$DownloadLocation, handle=h, open=conn)
  on.exit(tryCatch(close(uri), error=invisible), add=TRUE)

   # Existence of DataTypeId, DowloadLocation guaranteed by caller
   switch(tolower(x$DataTypeId),
     arff = read.arff(uri),
     plaintext = paste(readLines(uri, warn=FALSE), collapse="\n"),
     generictsvnoheader = read.table(uri, sep="\t", header=FALSE, ...),
     generictsv = read.table(uri, sep="\t", header=TRUE, ...),
     genericcsvnoheader = read.table(uri, sep=",", header=FALSE, ...),
     genericcsv = read.table(uri, sep=",", header=TRUE, ...),
     zip = readBin(uri, what="raw", n=x$Size, ...),
     stop("unsupported data type: '",x$DataTypeId,"'")
   )
}


# Checks if zip is available on system.
# Required for packageEnv()
zipAvailable = function(){
  z = unname(Sys.which("zip"))
  z != ""
}

zipNotAvailableMessage = "Requires external zip utility. Please install zip and try again."

#' Package a Function and Dependencies into an Environment
#'
#' @param exportenv R environment to package
#' @param packages a character vector of required R package dependencies
#' @param version optional R version
#' @return A base64-encoded zip file containing the saved 'exportenv' environment
#' @import codetools
#' @importFrom base64enc base64encode
#' @importFrom miniCRAN makeRepo pkgDep
#' @keywords Internal
packageEnv = function(exportenv, packages=NULL, version="3.1.0")
{
  if(!zipAvailable()) stop(zipNotAvailableMessage)
  if(!is.null(packages)) assign("..packages", packages, envir=exportenv)
  d = tempfile(pattern="dir")
  on.exit(unlink(d, recursive=TRUE))
  tryCatch(dir.create(d), warning=function(e) stop(e))
  # zip, unfortunately a zip file is apparently an AzureML requirement.
  cwd = getwd()
  on.exit(setwd(cwd), add=TRUE)
  setwd(d)
  # save export environment to an RData file
  save(exportenv, file="env.RData")

  # Package up dependencies
  if(!is.null(packages))
  {
    re = getOption("repos")
    if(is.null(re)) re = c(CRAN="http://cran.revolutionanalytics.com")
    p = paste(d,"packages",sep="/")
    tryCatch(dir.create(p), warning=function(e) stop(e))
    tryCatch(makeRepo(pkgDep(packages, repos=re, suggests=FALSE), path=p, re, type="win.binary", Rversion=version),
      error=function(e) stop(e))
  }

  z = try({
    zip(zipfile="export.zip", files=dir())
  })
  if(inherits(z, "error") || z > 0) stop("Unable to create zip file")
  setwd(cwd)
  base64encode(paste(d, "export.zip", sep="/"))
}

