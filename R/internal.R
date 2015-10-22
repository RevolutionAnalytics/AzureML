# Used in experiment date parsing
date_origin = "1970-1-1"

# internal utility
urlconcat = function(a,b)
{
  ans = paste(gsub("/$", "", a), b, sep="/")
  ans = gsub(":/([^/])", "://\\1", ans)
  ans
}

#' get_datasets internal function that retrieves datasets
#'
#' internal function that retrieves datasets
#' @param w A workspace object
#' @return a data.frame
#' @importFrom curl handle_setheaders curl new_handle
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
#' @importFrom curl handle_setheaders curl new_handle
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

#' get_dataset internal function that retrieves a dataset from AzureML
#'
#' internal function that retrieves a dataset from AzureML
#' @param x a list or data.frame with \code{DownloadLocation} and \code{DataTypeId} fields
#' @param h optional curl handle
#' @param ... additional parameters to pass to \code{read.table}
#' @return a data.frame
#' @importFrom foreign read.arff
#' @importFrom curl new_handle curl
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
