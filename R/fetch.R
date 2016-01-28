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


# Used in experiment date parsing
date_origin = "1970-1-1"


validate_response <- function(r){
  if(r$status_code >= 400){
    # Some functions return response in JSON format, others not
    body <- tryCatch(fromJSON(rawToChar(r$content)), error = function(e)e)
    response_is_json <- !inherits(body, "error")
    
    if(response_is_json){
      # If response is JSON, then we have a list wiwh status code and error message
      msg <- paste(
        "AzureML returns error code:",
        sprintf("HTTP status code : %s", r$status_code),
        sprintf("AzureML error code  : %s", body$error$code),
        "", 
        body$error$message,
        sep = "\n"
      )
    } else {
      # Response is plain text, with not list structure
      body <- rawToChar(r$content)
      msg <- switch(
        as.character(r$status_code),
        "400" = "400 (Bad request). Please check your workspace ID, auth and api_endpoint.",
        "401" = "401 (Unauthorised).  Please check your workspace ID and auth codes.",
        "403" = "403 (Forbidden).",
        paste(
          "AzureML returns error code:",
          sprintf("HTTP status code : %s", r$status_code),
          sep = "\n"
        )
      )
      msg <- paste(msg, body, sep = "\n")
    }
    stop(msg, call. = FALSE)
  }
}

#' Try to fetch a uri/handle, retrying on certain returned status codes after a timeout.
#' 
#' @param uri the uri to fetch
#' @param handle a curl handle
#' @param retry_on HTTP status codes that result in retry
#' @param tries number of tries before failing
#' @param delay in seconds between retries, subject to exponent
#' @param exponent increment each successive delay by delay^exponent
#' @param no_message_threshold Only show messages if delay is greater than this limit
#' 
#' @keywords Internal
#' @return the result of curl_fetch_memory(uri, handle)
#' 
try_fetch <- function(uri, handle, 
                      retry_on = c(400, 401, 440, 503, 504, 509), 
                      tries = 6, 
                      delay = 1, exponent = 2,
                      no_message_threshold = 1)
{
  collisions = 1
  while(collisions < tries) {
    r = curl_fetch_memory(uri, handle)
    if(!(r$status_code %in% retry_on)) {
      validate_response(r)
      return(r)
    }
    wait_time = delay * (2 ^ collisions - 1)
    wait_time <- runif(1, min = 0.001, max = wait_time)
    printed_message <- FALSE
    if(wait_time > no_message_threshold){
      message(sprintf("Request failed with status %s. Waiting %3.1f seconds before retry", 
                      r$status_code,
                      wait_time))
      printed_message <- TRUE
      wait_time <- ceiling(wait_time)
      for(i in 1:wait_time){
        message(".", appendLF = FALSE)
        Sys.sleep(1)
      }
    } else {
      Sys.sleep(wait_time)
    }
    collisions = collisions + 1
  }
  if(printed_message) message("\n")
  validate_response(r)
  r
}

# urlAPIinsert <- function(x, text = "api"){
#   gsub("(http.*?)(\\..*)", sprintf("\\1%s\\2", text), x)
# }


