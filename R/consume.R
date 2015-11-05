#' Use a web service to score data in list (key=value) format
#'
#' Score data represented as lists where each list key represents
#' a parameter of the web service.
#'
#' @export
#'
#' @inheritParams refresh
#' @param endpoint AzureML Web Service endpoint returned by \code{\link{endpoints}}
#' @param ... variable number of requests entered as lists in key-value format;
#' optionally a single data frame argument.
#' @param globalParam global parameters entered as a list, default value is an empty list
#' @param retryDelay the time in seconds to delay before retrying in case of a server error
#' @param output name of the output port to return usually 'output1' or 'output2';
#'  set to NULL to return everything as raw results in JSON-encoded list form
#' @return data frame containing results returned from web service call
#' @note Set \code{...} to a list of key/value pairs corresponding to web service
#' inputs. Optionally, set \code{...} to a single data frame with columns corresponding
#' to web service variables. The data frame approach returns output from the evaluation
#' of each row of the data frame (see the examples).
#' @seealso \code{\link{publishWebService}} \code{\link{endpoints}} \code{\link{services}} \code{\link{workspace}}
#' @family consumption functions
#' @importFrom jsonlite fromJSON
#' @example inst/examples/example_publish.R
consume = function(endpoint, ..., globalParam, retryDelay=10, output="output1")
{
  apiKey = endpoint$PrimaryKey
  requestUrl = endpoint$ApiLocation
  if(missing(globalParam)) {
    globalParam = setNames(list(), character(0))
  }
  # Store variable number of lists entered as a list of lists
  requestsLists = list(...)
  if(length(requestsLists)==1 && is.data.frame(requestsLists[[1]]))
  {
    requestsLists = requestsLists[[1]]
  }
  # Make API call with parameters
  result = fromJSON(callAPI(apiKey, requestUrl, requestsLists,  globalParam, retryDelay))
  # Access output by converting from JSON into list and indexing into Results
  if(output=="output1") return(data.frame(result$Results$output1))
  if(output=="output2") return(fromJSON(result$Results$output2[[1]]))
  result$Results
}



#' Framework for making an Azure ML web service API call.
#'
#' Helper function that constructs and send the API call to a Microsoft Azure
#' Machine Learning web service, then receives and returns the response in JSON format.
#'
#' @param apiKey primary API key
#' @param requestUrl API URL
#' @param keyvalues the data to be passed to the web service
#' @param globalParam the global parameters for the web service
#' @param retryDelay number of seconds to wait after failing (max 3 tries) to try again
#' @return result the response
#'
#' @importFrom jsonlite toJSON
#' @importFrom curl handle_setheaders new_handle handle_setopt curl_fetch_memory
#' @keywords internal
callAPI = function(apiKey, requestUrl, keyvalues,  globalParam, retryDelay=10)
{
  # Set number of tries and HTTP status to 0
  result = NULL
  tries = 0
  while(tries < 3) # Limit number of API calls to 3
  {
    # Construct request payload
    req = list(Inputs=list(input1=keyvalues), GlobalParameters=globalParam)
    body = charToRaw(paste(toJSON(req, auto_unbox=TRUE),collapse="\n"))
    h = new_handle()
    headers = list(`User-Agent`="R",
		   `Content-Type`="application/json",
		   `Authorization`=sprintf("Bearer %s", apiKey))
    handle_setheaders(h, .list=headers)
    handle_setopt(h, .list=list(post=TRUE, postfieldsize=length(body), postfields=body))
    r = curl_fetch_memory(requestUrl, handle=h)
    # Get HTTP status to decide whether to throw bad request or retry, or return etc.
    httpStatus = r$status_code
    result = rawToChar(r$content)
    if(httpStatus == 200) break
    if(tries==0)
      warning(sprintf("Request failed with status %s. Retrying request...", httpStatus), immediate.=TRUE)
    Sys.sleep(retryDelay)
    tries = tries + 1
  }
  if(httpStatus >= 400) stop(result)
  result
}



#' Discover web service schema
#'
#' Discover the expected input to a web service specified by a web service ID ng the workspace ID and web service ID, information specific to the consumption functions
#'
#' @export
#'
#' @param helpURL URL of the help page of the web service
#' @param scheme the URI scheme
#' @param host optional parameter that defaults to ussouthcentral.services.azureml.net
#' @param api_version AzureML API version
#' @return List containing the request URL of the webservice, column names of the data, sample input as well as the input schema
#'
#' @seealso \code{\link{publishWebService}} \code{\link{consume}} \code{\link{workspace}} \code{link{services}} \code{\link{endpoints}} \code{\link{endpointHelp}}
#' @family discovery functions
discoverSchema = function(helpURL, scheme = "https", host = "ussouthcentral.services.azureml.net", api_version = "2.0")
{
  endpointId = getDetailsFromUrl(helpURL)[[1]]
  workspaceId = getDetailsFromUrl(helpURL)[[2]]
  # Construct swagger document URL using parameters
  # Use paste method without separator
  swaggerURL = paste(scheme,"://", host, "/workspaces/", workspaceId, "/services/", endpointId,"/swagger.json", sep = "")
  print(swaggerURL)

  # parses the content and gets the swagger document
  r = curl(swaggerURL)
  swagger = fromJSON(readLines(r,warn=FALSE))
  close(r)

  # Accesses the input schema in the swagger document
  inputSchema = swagger$definition$input1Item
  #Accesses the example in the swagger document and converts it to JSON
  exampleJson = toJSON(swagger$definitions$ExecutionRequest$example)

  #Accesses a single specific JSON object and formats it to be a request inputted as a list in R
  inputExample = as.list((fromJSON((exampleJson)))$Inputs$input1)

  for(i in 1:length(inputExample)) {
    if(typeof(inputExample[[i]]) == "character") {
      inputExample[i] = "Please input valid String"
    }
  }
  #Accesses the names of the columns in the example and stores it in a list of column names
  columnNames = list()
  for(i in 1:length(inputExample)) {
    columnNames[[i]] = names(inputExample)[[i]]
  }
  # Uses multiple nested loops to access the various paths in the swagger document and find the execution path
  foundExecPath = FALSE
  pathNo = 0
  execPathNo= -1
  for(execPath in swagger$paths) {
    pathNo = pathNo + 1
    for(operationpath in execPath) {
      for(operation in operationpath) {
        #Goes through the characteristcs in every operation e.g. operationId
        for(charac in operation) {
          # Finds the path in which the operationId (characteristic of the path) = execute and sets the execution path number
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

  #Stores the execution path
  if(foundExecPath) {
    executePath = names(swagger$paths)[[execPathNo]]
  } else{
    executePath = "Path not found"
  }
  # Constructs the request URL with the parameters as well as execution path found. The separator is set to an empty string
  requestUrl = paste(scheme,"://", host, "/workspaces/", workspaceId, "/services/", endpointId, executePath, sep = "")

  # Access the HTTP method type e.g. GET/ POST and constructs an example request
  httpMethod = toupper(names(swagger$paths[[2]]))
  httpRequest = paste(httpMethod,requestUrl)
  # Warns user of characters and urges them to enter valid strings for them
  firstWarning = TRUE
  for(i in 1:length(inputExample)) {
    if(typeof(inputExample[[i]]) == "character") {
      if(firstWarning) {
        cat("Warning! The sample input does not contain sample values for characters. Please input valid Strings for these fields", "\n")
      }
      cat("   ", names(inputExample)[[i]],"\n")
      firstWarning = FALSE
    }
  }

  #Returns what was discovered in the form of a list
  return (list("requestUrl" = requestUrl, "columnNames" = columnNames, "sampleInput" = inputExample, "inputSchema" = inputSchema))
}



#' Helper function to extract information from a help page URL
#'
#' Given a Microsoft Azure Machine Learning web service endpoint, extracts the endpoint ID and the workspace ID
#'
#' @param helpURL the URL of a help page
#' @return a list containing the endpoint ID and the workspace ID
#'
#' @keywords internal
getDetailsFromUrl = function(helpURL) {
  #Uses a strong split to extract the endpoint ID and the workspace ID
  return (list((strsplit(((strsplit(helpURL,"endpoints/"))[[1]][2]),"/")[[1]][[1]]),(strsplit(((strsplit(helpURL,"/workspaces/"))[[1]][2]),"/")[[1]][[1]])))

}
