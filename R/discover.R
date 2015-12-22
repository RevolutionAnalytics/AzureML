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


#' Helper function to extract information from a help page URL
#'
#' Given a Microsoft Azure Machine Learning web service endpoint, extracts the endpoint ID and the workspace ID
#'
#' @param url the URL of a help page
#' @return a vector containing the workspace ID, webservices ID and endpoint ID
#'
#' @keywords internal
getDetailsFromUrl <- function(url) {
  ptn = ".*?/workspaces/([[:alnum:]]*)/webservices/([[:alnum:]]*)/endpoints/([[:alnum:]]*)/*.*$"
  if(!grepl(ptn, url)) stop("Invalid url")
  c(
    gsub(ptn, "\\1", url),
    gsub(ptn, "\\2", url),
    gsub(ptn, "\\3", url)
    
  )
}


#' Discover web service schema.
#'
#' Discover the expected input to a web service specified by a web service ID ng the workspace ID and web service ID, information specific to the consumption functions
#'
#' @param helpURL URL of the help page of the web service
#' @param scheme the URI scheme
#' @param host optional parameter that defaults to ussouthcentral.services.azureml.net
#' @param api_version AzureML API version
#' 
#' @return List containing the request URL of the webservice, column names of the data, sample input as well as the input schema
#'
#' @seealso \code{\link{publishWebService}} \code{\link{consume}} \code{\link{workspace}} \code{link{services}} \code{\link{endpoints}} \code{\link{endpointHelp}}
#' 
#' @family discovery functions
#' @export
discoverSchema <- function(helpURL, scheme = "https", 
                           host = "ussouthcentral.services.azureml.net", 
                           api_version = "2.0")
{
  workspaceId = getDetailsFromUrl(helpURL)[1]
  endpointId = getDetailsFromUrl(helpURL)[3]
  # Construct swagger document URL using parameters
  # Use paste method without separator
  uri = paste0(scheme,"://", host, 
               "/workspaces/", workspaceId, 
               "/services/", endpointId,
               "/swagger.json")
  
  # parses the content and gets the swagger document
  r <- try_fetch(uri, handle = new_handle())
  swagger <- fromJSON(rawToChar(r$content))
  
  # Accesses the input schema in the swagger document
  inputSchema <- swagger$definition$input1Item
  
  # Accesses the example in the swagger document and converts it to JSON
  exampleJson <- toJSON(swagger$definitions$ExecutionRequest$example)
  
  # Accesses a single specific JSON object and formats it to be a request inputted as a list in R
  inputExample <- as.list((fromJSON((exampleJson)))$Inputs$input1)
  idx <- sapply(inputExample, class, USE.NAMES = FALSE) == "character"
  inputExample[idx] <- "Please input valid String"
  
  # Accesses the names of the columns in the example
  # and stores it in a list of column names
#   columnNames <- vector("list", length = length(inputExample))
#   columnNames <- list()
#   for(i in seq_along(inputExample)) {
#     columnNames[[i]] = names(inputExample)[i]
#   }
  columnNames <- lapply(seq_along(inputExample), function(i)names(inputExample[i]))

    # Uses multiple nested loops to access the various paths in the 
  # swagger document and find the execution path
  foundExecPath = FALSE
  pathNo = 0
  execPathNo = -1
  for(execPath in swagger$paths) {
    pathNo = pathNo + 1
    for(operationpath in execPath) {
      for(operation in operationpath) {
        # Goes through the characteristcs in every operation e.g. operationId
        for(charac in operation) {
          # Finds the path in which the 
          # operationId (characteristic of the path) == execute 
          # and sets the execution path number
          if(charac[1] == "execute")
          {
            #Sets found execution path to true
            foundExecPath = TRUE
            execPathNo = pathNo
            break
          }
        }
      }
    }
  }
  
  # Stores the execution path
  executePath <- if(foundExecPath) names(swagger$paths)[[execPathNo]] 
  else "Path not found"
  
  # Constructs the request URL with the parameters as well as execution path found. 
  # The separator is set to an empty string
  requestUrl <- paste0(scheme,"://", host, 
                       "/workspaces/", workspaceId, 
                       "/services/", endpointId, 
                       executePath)
  
  # Access the HTTP method type e.g. GET/ POST and constructs an example request
  httpMethod <- toupper(names(swagger$paths[[2]]))
  httpRequest <- paste(httpMethod,requestUrl)
  
  # Warns user of characters and urges them to enter valid strings for them
  firstWarning = TRUE
  for(i in 1:length(inputExample)) {
    if(is.character(inputExample[[i]])) {
      if(firstWarning) {
        msg <- paste("The sample input does not contain sample values for characters.",
                     "Please input valid strings for these fields:", 
                     sep = "\n")
        message(msg)
      }
      message(" - ", names(inputExample)[[i]])
      firstWarning = FALSE
    }
  }
  
  #Returns what was discovered in the form of a list
  z <- list(requestUrl = requestUrl, 
            columnNames = columnNames, 
            sampleInput = inputExample, 
            inputSchema = inputSchema
  )
  class(z) <- "discoverSchema"
  z
}

#' @export
print.discoverSchema <- function(x, ...){
  str(x, ...)
  invisible()
}
