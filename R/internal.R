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


urlconcat <- function(a,b)
{
  ans = paste(gsub("/$", "", a), b, sep="/")
  ans = gsub(":/([^/])", "://\\1", ans)
  ans
}

# Internal function that retrieves datasets.
#
# @param ws A workspace object
# @return a data.frame
#' @importFrom curl handle_setheaders curl new_handle
#' @importFrom jsonlite fromJSON
# @keywords Internal
get_datasets <- function(ws) {
  h = new_handle()
  handle_setheaders(h, .list = ws$.headers)
  uri <- sprintf("%s/workspaces/%s/datasources", ws$.studioapi, ws$id)
  r <- try_fetch(uri = uri, handle = h, delay = 0.25, .retry = 3)
  
  msg <- paste("No results returned from datasets(ws).", 
               "Please check your workspace credentials and api_endpoint are correct.")
  if(inherits(r, "error")){ stop(msg) }
  if(r$status_code >= 400){ stop(msg) }
  
  x <- fromJSON(rawToChar(r$content))
  if(is.null(x) || is.na(x$Name[1])){
    x = data.frame()
    class(x) = c("Datasets", "data.frame")
    return(x)
  }
  # Use strict variable name matching to look up data
  d = x[, "DownloadLocation"]
  x$DownloadLocation = paste0(d[, "BaseUri"], 
                              d[, "Location"],
                              d[, "AccessCredential"])
  d = x[,"VisualizeEndPoint"]
  x$VisualizeEndPoint = paste0(d[, "BaseUri"], 
                               d[, "AccessCredential"])
  d = x[,"SchemaEndPoint"]
  x$SchemaEndPoint = paste0(d[, "BaseUri"], 
                            d[, "Location"],
                            d[, "AccessCredential"])
  class(x) = c("Datasets", "data.frame")
  x
}


convertToDate <- function(x) {
  x = as.numeric(gsub("[^-0-9]", "", x)) /1000
  x = ifelse(x >= 0, x, NA)
  suppressWarnings(
    as.POSIXct(x, tz = "GMT", origin = date_origin)
  )
}


# Internal function that retrieves experiments.
#
# @param ws A workspace object
# @return a data.frame
#' @importFrom curl handle_setheaders curl new_handle
#' @importFrom jsonlite fromJSON
# @keywords Internal
get_experiments <- function(ws) {
  h = new_handle()
  handle_setheaders(h, .list=ws$.headers)
  uri = sprintf("%s/workspaces/%s/experiments", ws$.studioapi, ws$id)
  r <- try_fetch(uri = uri, handle = h, delay = 0.25, .retry = 3)
  
  msg <- paste("No results returned from experiments(ws).", 
               "Please check your workspace credentials and api_endpoint are correct.")
  if(inherits(r, "error")){ stop(msg) }
  if(r$status_code >= 400){ stop(msg) }
  
  # Use strict variable name matching to look up data
  x <- fromJSON(rawToChar(r$content))
  x = cbind(x, x[,"Status"])
  
  x$Status = c()
  x$EndTime = convertToDate(x[["EndTime"]])
  x$StartTime = convertToDate(x[["StartTime"]])
  x$CreationTime = convertToDate(x[["CreationTime"]])
  class(x) = c("Experiments", "data.frame")
  x
}

# Internal function that retrieves a dataset from AzureML.
#
# @param x a list or data.frame with \code{DownloadLocation} and \code{DataTypeId} fields
# @param h optional curl handle
# @param quote passed to \code{\link[utils]{read.table}}
# @param ... additional parameters to pass to \code{read.table}
# @return a data.frame
#' @importFrom foreign read.arff
#' @importFrom curl new_handle curl
# @keywords Internal
get_dataset <- function(x, h, quote = "\"", ...) {
  # Set default stringsAsFactors to FALSE, but allow users to override in ...
  # Restore the option on function exit.
  opts = options(stringsAsFactors = FALSE)
  on.exit(options(opts))
  if(missing(h)) h = new_handle()
  conn = "r"
  if(tolower(x$DataTypeId) == "zip") conn = "rb"
  uri = curl(x$DownloadLocation, handle = h, open = conn)
  on.exit(tryCatch(close(uri), error = invisible), add = TRUE)
  
  # Existence of DataTypeId, DowloadLocation guaranteed by caller
  switch(
    tolower(x$DataTypeId),
    arff               = read.arff(uri),
    plaintext          = paste(readLines(uri, warn = FALSE), collapse="\n"),
    generictsvnoheader = read.table(uri, sep = "\t", header = FALSE, quote, ...),
    generictsv         = read.table(uri, sep = "\t", header = TRUE, quote, ...),
    genericcsvnoheader = read.table(uri, sep = ",", header = FALSE, quote, ...),
    genericcsv         = read.table(uri, sep = ",", header = TRUE, quote, ...),
    zip                = readBin(uri, what = "raw", n = x$Size, ...),
    stop("unsupported data type: '",x$DataTypeId,"'")
  )
}


# Checks if zip is available on system.
# Required for packageEnv()
zipAvailable <- function(){
  z <- unname(Sys.which("zip"))
  z != ""
}

zipNotAvailableMessage = "Requires external zip utility. Please install zip, ensure it's on your path and try again."


# Package a Function and Dependencies into an Environment
#
# @param exportenv R environment to package
# @param packages a character vector of required R package dependencies
# @param version optional R version
# @return A base64-encoded zip file containing the saved 'exportenv' environment
#' @import codetools
#' @importFrom base64enc base64encode
#' @importFrom miniCRAN makeRepo pkgDep
# @keywords Internal
packageEnv <- function(exportenv = new.env(), packages=NULL, version = getOption("default_r_version")) {
  if(!zipAvailable()) stop(zipNotAvailableMessage)
  
  if(!is.null(packages)) assign("..packages", packages, envir = exportenv)
  td <- tempfile(pattern = "dir")
  on.exit(unlink(td, recursive=TRUE))
  tryCatch(dir.create(td), warning=function(e) stop(e))
  # zip, unfortunately a zip file is apparently an AzureML requirement.
  cwd = getwd()
  on.exit(setwd(cwd), add = TRUE)
  setwd(td)
  # save export environment to an RData file
  save(exportenv, file="env.RData")
  
  # Package up dependencies
  if(!is.null(packages))
  {
    re <- getOption("repos")
    if(is.null(re)){
      re <- c(CRAN = "http://cran.revolutionanalytics.com")
    }
    tp <- normalizePath(file.path(td, "packages"), winslash = "/", mustWork = FALSE)
    tryCatch(dir.create(tp), warning = function(e) stop(e))
    all_p <- pkgDep(packages, 
                    repos = re, 
                    type = "win.binary",
                    Rversion = version,
                    suggests = FALSE
    )
    tryCatch(
      z <- makeRepo(all_p,
               path = tp, 
               repos = re, 
               type = "win.binary", 
               Rversion = version
      ),
      error=function(e) stop(e)
    )
    if(!all(grepl(tp, z))) {
      warning("Packages did not copy properly in to AzureML. Please ensure you have miniCRAN v0.2.7 or above installed.")
    }
    z
  }
  
  z = try({
    zip(zipfile = "export.zip", files = dir(), flags = "-r9Xq")
  })
  if(inherits(z, "error") || z > 0) stop("Unable to create zip file")
  setwd(cwd)
  base64encode(file.path(td, "export.zip", fsep="/"))
}

