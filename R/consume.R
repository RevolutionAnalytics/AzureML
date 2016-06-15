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



#' Use a web service to score data in list (key=value) format.
#'
#' Score data represented as lists where each list key represents a parameter of the web service.
#'
#' @export
#'
#' @inheritParams refresh
#' @param endpoint Either an AzureML web service endpoint returned by \code{\link{publishWebService}}, \code{\link{endpoints}}, or simply an AzureML web service from \code{\link{services}}; in the latter case the default endpoint for the service will be used.
#' @param ... variable number of requests entered as lists in key-value format; optionally a single data frame argument.
#' @param globalParam global parameters entered as a list, default value is an empty list
#' @param retryDelay the time in seconds to delay before retrying in case of a server error
#' @param output name of the output port to return usually 'output1' or 'output2'; set to NULL to return everything as raw results in JSON-encoded list form
#' 
#' @return data frame containing results returned from web service call
#' 
#' @note Set \code{...} to a list of key/value pairs corresponding to web service inputs. Optionally, set \code{...} to a single data frame with columns corresponding to web service variables. The data frame approach returns output from the evaluation of each row of the data frame (see the examples).
#'
#' @seealso \code{\link{publishWebService}} \code{\link{endpoints}} \code{\link{services}} \code{\link{workspace}}
#' @family consumption functions
#' @importFrom jsonlite fromJSON
#' @example inst/examples/example_publish.R
consume <- function(endpoint, ..., globalParam, retryDelay = 10, output = "output1", tries = 5)
{
  if(is.Service(endpoint))
  {
    if(nrow(endpoint) > 1) endpoint = endpoint[1, ]
    default <- endpoint$DefaultEndpointName
    endpoint <- endpoints(attr(endpoint, "workspace"), endpoint)
    endpoint <- subset(endpoint, Name = default)
  }
  
  if(!is.Endpoint(endpoint)) {
    stop("Invalid endpoint. Use publishWebservice() or endpoints() to create or obtain a service endpoint.")
  }
  
  apiKey <- endpoint$PrimaryKey
  requestUrl <- endpoint$ApiLocation
  
  if(missing(globalParam)) {
    globalParam <- setNames(list(), character(0))
  }
  # Store variable number of lists entered as a list of lists
  requestsLists <- list(...)
  if(length(requestsLists) == 1 && is.data.frame(requestsLists[[1]])) {
    requestsLists <- requestsLists[[1]]
  } else {
    if(!is.list(requestsLists[[1]])) {
      requestsLists <- list(requestsLists)
    }
  }
  # Make API call with parameters
  result <- callAPI(apiKey, requestUrl, requestsLists,  globalParam, retryDelay, tries = tries)
  if(inherits(result, "error")) stop("AzureML returned an error code")
  
  # Access output by converting from JSON into list and indexing into Results
  if(!is.null(output) && output == "output1") {
    help <- endpointHelp(endpoint)$definitions$output1Item
    ans <- data.frame(result$Results$output1)
    nums <- which("number" == unlist(help)[grepl("\\.type$", names(unlist(help)))])
    logi <- which("boolean" == unlist(help)[grepl("\\.type$", names(unlist(help)))])
    if(length(nums) > 0) for(j in nums) ans[,j] <- as.numeric(ans[,j])
    if(length(logi) > 0) for(j in logi) ans[,j] <- as.logical(ans[,j])
    return(ans)
  }
  if(!is.null(output) && output == "output2") {
    return(fromJSON(result$Results$output2[[1]]))
  }
  result$Results
}



# Framework for making an Azure ML web service API call.
#
# Helper function that constructs and send the API call to a Microsoft Azure
# Machine Learning web service, then receives and returns the response in JSON format.
#
# @param apiKey primary API key
# @param requestUrl API URL
# @param keyvalues the data to be passed to the web service
# @param globalParam the global parameters for the web service
# @param retryDelay number of seconds to wait after failing (max 3 tries) to try again
# @param tries the number of retry attempts
# @return result the response
#
# @importFrom jsonlite toJSON
# @importFrom curl handle_setheaders new_handle handle_setopt curl_fetch_memory
# @keywords internal
callAPI <- function(apiKey, requestUrl, keyvalues, globalParam, 
                    retryDelay=10, tries = 5) {
  # Set number of tries and HTTP status to 0
  result <- NULL
  # Construct request payload
  req <- list(
    Inputs = list(input1 = keyvalues), 
    GlobalParameters = globalParam
  )
  # message(toJSON(req, auto_unbox = TRUE, digits = 16, pretty = TRUE))
  body <- charToRaw(paste(
    toJSON(req, auto_unbox = TRUE, digits = 16),
    collapse = "\n")
  )
  h <- new_handle()
  headers <- list(`User-Agent` = "R",
                  `Content-Type` = "application/json",
                  `Authorization` = paste0("Bearer ", apiKey))
  handle_setheaders(h, .list = headers)
  handle_setopt(h, 
                .list = list(
                  post = TRUE, 
                  postfieldsize = length(body), 
                  postfields = body
                )
  )
  r <- try_fetch(requestUrl, h, delay = retryDelay, tries = tries)
  result <- fromJSON(rawToChar(r$content))
  if(r$status_code >= 400)  {
    stop(paste(capture.output(result), collapse="\n"))
  }
  return(result)
}
