#' Discover web service schema
#'
#' Discover the expected input to a web service specified by a web service ID ng the workspace ID and web service ID, information specific to the consumption functions
#'
#' @export
#'
#' @param helpURL URL of the help page of the web service
#' @param scheme the URI scheme
#' @param host optional parameter that defaults to ussouthcentral.services.azureml.net
#' @param api_version defaults to 2.0
#' @return List containing the request URL of the webservice, column names of the data, sample input as well as the input schema
#'
#' @seealso \code{\link{publishWebService}} \code{\link{consumeLists}}
#' @family discovery functions
#'
#' @examples
#' \dontrun{
#' endpoints <- getEndpoints("wsID", "wsAuth", "webserviceID")
#' wsSchema <- discoverSchema(endpoints[[1]]$HelpLocation)
#' }
discoverSchema <- function(helpURL, scheme = "https", host = "ussouthcentral.services.azureml.net", api_version = "2.0") {
  endpointId = getDetailsFromUrl(helpURL)[[1]]
  workspaceId = getDetailsFromUrl(helpURL)[[2]]
  # Construct swagger document URL using parameters
  # Use paste method without separator
  swaggerURL = paste(scheme,"://", host, "/workspaces/", workspaceId, "/services/", endpointId,"/swagger.json", sep = "")
  print(swaggerURL)

  # Automatically parses the content and gets the swagger document

  response <- RCurl::getURLContent(swaggerURL)
  swagger = rjson::fromJSON(response)

  # Accesses the input schema in the swagger document
  inputSchema = swagger$definition$input1Item
  #Accesses the example in the swagger document and converts it to JSON
  exampleJson <- rjson::toJSON(swagger$definitions$ExecutionRequest$example)

  #Accesses a single specific JSON object and formats it to be a request inputted as a list in R
  inputExample = as.list((jsonlite::fromJSON((exampleJson)))$Inputs$input1)

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
  # Tell user what functions they can use and prints to the console
  if(foundExecPath) {
    consumeFile = paste("To score a file: consumeFile(apiKey, requestUrl, dataframe)")
    consumeDataFrame = paste("To score a dataframe: consumeDataframe(apiKey, requestUrl, scoreDataFrame)")
    consumeLists = paste("To score requests as lists in the key-value format: consumeLists(apiKey, requestUrl, ...)")
    cat("Sample functions to execute the web service and get a response synchronously:","\n", consumeFile,"\n", consumeDataFrame,"\n", consumeLists,"\n","\n")

  } else {
    cat("Warning! There was no execution path found for this web service, hence a request URL cannot be constructed!","\n","\n")
  }
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



#' Use a web service to score a file
#'
#' Read in a csv and score it in batches using a Microsoft Azure Machine Learning Web Service. The results are stored in a new csv, default named "results.csv"
#'
#' @export
#'
#' @param apiKey primary access key as a string
#' @param requestUrl API URL
#' @param inFileName the name of the file to be scored as a string
#' @param globalParam global parameters entered as a list, default value is an empty list
#' @param outputFileName the name of the file to output results to, entered as a string, default value is "results.csv"
#' @param batchSize batch size of each batch, default value is 300
#' @param retryDelay the time in seconds to delay before retrying in case of a server error, default value is 0.3 seconds
#' @return returnDataFrame data frame containing results returned from web service call
#'
#' @seealso \code{\link{discoverSchema}} \code{\link{publishWebService}}
#' @family consumption functions
#'
#' @import stats
#'
#' @examples
#' \dontrun{
#' add <- function(x,y) { return(x+y) }
#' newService <- publishWebService("add", "add",
#'  list("x"="int","y"="int"), list("z"="int"), wsID, authToken)
#' webserviceDetails <- newService[[1]]
#' endpoints <- newService[[2]]
#'
#' response <- consumeFile(endpoints[[1]]$PrimaryKey, endpoints[[1]]$ApiLocation, "test.csv")
#' }
consumeFile <- function(apiKey, requestUrl, inFileName, globalParam = setNames(list(), character(0)), outputFileName = "results.csv", batchSize = 300, retryDelay = 0.3) {
  #Stops users if they miss out mandatory fields
  if (missing(apiKey)) {
    stop("Need to specify API key")
  }
  if (missing(inFileName)) {
    stop("Need to specify file to be scored")
  }
  if (missing(requestUrl)) {
    stop("Need to specify request URL")
  }
  #read file and store as a data frame to be scored
  scoreDataFrame = read.csv(inFileName,check.names=FALSE)
  # create empty data frame that stores results to be returned
  returnDataFrame <- data.frame(stringsAsFactors=FALSE)
  # create data frame that stores requests in each batch
  requestBatch = data.frame(stringsAsFactors=FALSE)
  counter = 1
  lastProc = 0

  # Loop that iterates through the rows of the entire data frame that is to be scored
  for(i in 1:(nrow(scoreDataFrame))) {
    # If we have reached the batch size provided or the end of the data frame
    if(counter == batchSize || i == (nrow(scoreDataFrame))) {
      # Create empty data frame that stores results for that batch
      batchResults = data.frame(stringsAsFactors=FALSE)
      # Store a single batch of requests in a data frame
      requestBatch = scoreDataFrame[(lastProc+1):i,]
      # Convert them into key-value lists using rjson and df2json packages
      keyvalues = rjson::fromJSON((df2json::df2json(requestBatch)))
      # Store results returned from call in temp variable
      temp <- callAPI(apiKey, requestUrl, keyvalues, globalParam, retryDelay)
      # Set last processed to current row
      lastProc = i
      # Access output by converting from JSON into list and indexing into Results
      resultStored <- jsonlite::fromJSON(temp)
      resultList = resultStored$Results$output1
      batchResults <- data.frame(resultList)
      # Force returnDataFrame to have the same column names to avoid errors
      if(length(returnDataFrame) != 0 && length(batchResults) != 0) {
        names(returnDataFrame) <- names(resultList)
      }
      #Add batch results to the dataframe to be returned
      returnDataFrame <- rbind(returnDataFrame,batchResults)
      #Print how many rows in total have been processed
      print(sprintf("%i %s %i %s", i,"out of",nrow(scoreDataFrame),"processed"))
      #Reset the requests in the batch to empty data frame
      requestBatch = data.frame(stringsAsFactors=FALSE)
      counter = 0
    }
    counter = counter + 1
  }

  # Write results to a csv file
  resultsFile <-file(outputFileName,"w")
  write.csv(returnDataFrame, resultsFile)
  close(resultsFile)
  return (returnDataFrame)
}

#' Use a web service to score data in list format
#'
#' Score data represented as lists, where each list represents one parameter of the web service
#'
#' @export
#'
#' @param apiKey primary access key as a string
#' @param requestUrl API URL
#' @param ... variable number of requests entered as lists in key-value format
#' @param globalParam global parameters entered as a list, default value is an empty list
#' @param retryDelay the time in seconds to delay before retrying in case of a server error, default value is 0.3 seconds
#' @return returnDataFrame data frame containing results returned from web service call
#'
#' @seealso \code{\link{discoverSchema}} \code{\link{publishWebService}}
#' @family consumption functions
#'
#' @examples
#' \dontrun{
#' add <- function(x,y) { return(x+y) }
#' newService <- publishWebService("add", "add",
#'  list("x"="int","y"="int"), list("z"="int"), wsID, authToken)
#' webserviceDetails <- newService[[1]]
#' endpoints <- newService[[2]]
#'
#' response <- consumeLists(endpoints[[1]]$PrimaryKey, endpoints[[1]]$ApiLocation,
#'  list("x"=1, "y"=2), list("x"=3, "y"=4))
#' }
consumeLists <- function(apiKey, requestUrl, ..., globalParam = setNames(list(), character(0)), retryDelay = 0.3) {
  #Stops users if they miss out mandatory fields

  if (missing(apiKey)) {
    stop("Need to specify API key")
  }

  if (missing(requestUrl)) {
    stop("Need to specify request URL")
  }
  if(missing(globalParam)) {
    globalParam = setNames(list(), character(0))
  }
  # Store variable number of lists entered as a list of lists
  requestsLists <- list(...)
  # Make API call with parameters
  result <- callAPI(apiKey, requestUrl, requestsLists,  globalParam, retryDelay)
  # Access output by converting from JSON into list and indexing into Results
  resultStored <- jsonlite::fromJSON(result)
  resultList = resultStored$Results$output1

  # Store results in a data frame
  resultDataFrame <- data.frame(resultList)

  return(resultDataFrame)
}

#' Use a web service to score a data frame
#'
#' Score a data frame, where each row is the input to the scoring function, using a Microsoft Azure Machine Learning web service
#'
#' @export
#'
#' @param apiKey primary access key of the web service as a string
#' @param requestUrl API URL
#' @param scoreDataFrame the data frame to be scored
#' @param globalParam global parameters entered as a list, default value is an empty list
#' @param batchSize batch size of each batch, default value is 300
#' @param retryDelay the time in seconds to delay before retrying in case of a server error, default value is 0.3 seconds
#' @return returnDataFrame data frame containing results returned from web service call
#'
#' @seealso \code{\link{discoverSchema}} \code{\link{publishWebService}}
#' @family consumption functions
#'
#' @examples
#' \dontrun{
#' add <- function(x,y) { return(x+y) }
#' newService <- publishWebService("add", "add",
#'  list("x"="int","y"="int"), list("z"="int"), wsID, authToken)
#' webserviceDetails <- newService[[1]]
#' endpoints <- newService[[2]]
#'
#' df <- data.frame("x"=c(1,2), "y"=c(3,4))
#' response <- consumeDataframe(endpoints[[1]]$PrimaryKey, endpoints[[1]]$ApiLocation, df)
#' }
consumeDataframe <- function(apiKey, requestUrl, scoreDataFrame, globalParam=setNames(list(), character(0)), batchSize = 300, retryDelay = 0.3) {
  #Stops users if they miss out mandatory fields

  if (missing(apiKey)) {
    stop("Need to specify API key")
  }

  if (missing(requestUrl)) {
    stop("Need to specify request URL")
  }
  if (missing(scoreDataFrame)) {
    stop("Need to specify dataframe to be scored")
  }

  # create empty data frame that stores results to be returned
  returnDataFrame <- data.frame(stringsAsFactors=FALSE)
  # create data frame that stores requests in each batch
  requestBatch = data.frame(stringsAsFactors=FALSE)
  counter = 1
  lastProc = 0

  # Loop that iterates through the rows of the entire data frame that is to be scored
  for(i in 1:(nrow(scoreDataFrame))) {
    # If we have reached the batch size provided or the end of the data frame
    if(counter == batchSize || i == (nrow(scoreDataFrame))) {
      # Create empty data frame that stores results for that batch
      batchResults = data.frame(stringsAsFactors=FALSE)
      # Store a single batch of requests in a data frame
      requestBatch = scoreDataFrame[(lastProc+1):i,]
      # Convert them into key-value lists using rjson and df2json packages
      keyvalues = rjson::fromJSON((df2json::df2json(requestBatch)))
      # Store results returned from call in temp variable
      temp <- callAPI(apiKey, requestUrl, keyvalues, globalParam, retryDelay)
      # Set last processed to current row
      lastProc = i
      # Access output by converting from JSON into list and indexing into Results
      resultStored <- jsonlite::fromJSON(temp)
      resultList = resultStored$Results$output1
      batchResults <- data.frame(resultList)
      # Force returnDataFrame to have the same column names to avoid errors
      if(length(returnDataFrame) != 0 && length(batchResults) != 0) {
        names(returnDataFrame) <- names(resultList)
      }
      #Add batch results to the dataframe to be returned
      returnDataFrame <- rbind(returnDataFrame,batchResults)
      #Print how many rows in total have been processed
      print(sprintf("%i %s %i %s", i,"out of",nrow(scoreDataFrame),"processed"))
      #Reset the requests in the batch to empty data frame
      requestBatch = data.frame(stringsAsFactors=FALSE)
      counter = 0
    }
    counter = counter + 1
  }
  return(returnDataFrame)
}


#' Framework for making an Azure ML web service API call.
#'
#' Helper function that constructs and send the API call to a Microsoft Azure Machine Learning web service, then receives and returns the response in JSON format.
#'
#' @param apiKey primary API key
#' @param requestUrl API URL
#' @param keyvalues the data to be passed to the web service
#' @param globalParam the global parameters for the web service
#' @param retryDelay number of seconds to wait after failing (max 3 tries) to try again
#' @return result the response
#'
#' @keywords internal
callAPI <- function(apiKey, requestUrl, keyvalues,  globalParam, retryDelay) {
  # Set number of tries and HTTP status to 0
  httpStatus = 0
  tries = 0
  # Limit number of API calls to 3
  for(i in 1:3) {
    # In case of server error or if first try,
    if(tries == 0 || httpStatus >= 500) {
      if(httpStatus >= 500) {
        # Print headers and let user know you are retrying
        print(paste("The request failed with status code:", httpStatus, sep=" "))
        print("headers:")
        print(headers)
        print(sprintf("%s %f %s", "Retrying in ",retryDelay," seconds"))
        # Delay by specified time in case of server error
        Sys.sleep(retryDelay)
      }
      tries = tries + 1
      # Load RCurl package functions
      options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
      h = RCurl::basicTextGatherer()
      hdr = RCurl::basicHeaderGatherer()
      # Construct request payload
      req = list(
        Inputs = list(
          input1 = keyvalues
        )
        ,GlobalParameters = globalParam
      )
      # Convert request payload to JSON
      body = enc2utf8((rjson::toJSON(req)))
      # Create authorization header
      authz_hdr = paste('Bearer', apiKey, sep=' ')

      # Make call to API with necessary components
      h$reset()
      RCurl::curlPerform(url = requestUrl,
                         httpheader=c('Content-Type' = "application/json", 'Authorization' = authz_hdr),
                         postfields=body,
                         writefunction = h$update,
                         headerfunction = hdr$update,
                         verbose = FALSE
     )
      # Gather headers
      headers = hdr$value()
      # Get HTTP status to decide whether to throw bad request or retry, or return etc.
      httpStatus = headers["status"]
      result = h$value()
    }
    # Return result if successful
    if(httpStatus == 200) {
      return(result)
    }
    #if user error, print and return error details
    else if ((httpStatus>= 400) && (500 > httpStatus))
    {
      print(paste("The request failed with status code:", httpStatus, sep=" "))
      print("headers:")
      print(headers)
      print(h$value())
      return (result)
    }
  }
  return(result)
}

#' Helper function to extract information from a help page URL
#'
#' Given a Microsoft Azure Machine Learning web service endpoint, extracts the endpoint ID and the workspace ID
#'
#' @param helpURL the URL of a help page
#' @return a list containing the endpoint ID and the workspace ID
#'
#' @keywords internal
getDetailsFromUrl <- function(helpURL) {
  #Uses a strong split to extract the endpoint ID and the workspace ID
  return (list((strsplit(((strsplit(helpURL,"endpoints/"))[[1]][2]),"/")[[1]][[1]]),(strsplit(((strsplit(helpURL,"/workspaces/"))[[1]][2]),"/")[[1]][[1]])))

}
