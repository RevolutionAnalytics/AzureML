# String constants --------------------------------------------------------

publishURL <- "https://management.azureml.net/workspaces/%s/webservices/%s"
wrapper <- "inputDF <- maml.mapInputPort(1)\r\noutputDF <- matrix(ncol = %s, nrow = nrow(inputDF))\r\ncolnames(outputDF) <- list(%s)\r\noutputDF <- data.frame(outputDF)\r\nfor (file in list.files(\"src\")) {\r\n  if (file == \"%s\") {\r\n    load(\"src/%s\")\r\n    for (item in names(dependencies)) {\r\n      assign(item, dependencies[[item]])\r\n    }\r\n  }\r\n  else {\r\n    if (!(file %%in%% installed.packages()[,\"Package\"])) {\r\n      install.packages(paste(\"src\", file, sep=\"/\"), lib=\".\", repos=NULL, verbose=TRUE)\r\n    }\r\n    library(strsplit(file, \"\\\\.\")[[1]][[1]], character.only=TRUE)\r\n  }\r\n}\r\naction <- %s\r\nfor (i in 1:nrow(inputDF)) {\r\n  outputDF[i,] <- do.call(\"action\", as.list(inputDF[i,]))\r\n}\r\nmaml.mapOutputPort(\"outputDF\")"

# Functions ---------------------------------------------------------------

#' Get function source code
#'
#' Returns the source code of a function as a string
#'
#' @export
#'
#' @param x name of the function to convert to a string
#' @return source code of the function as a string
#'
#' @keywords internal
getFunctionString <- function (x)
{
  if (tryCatch(!is.character(x), error = function(e) TRUE))
    x <- as.character(substitute(x))
  objs <- list()
  where <- character()
  visible <- logical()
  if (length(pos <- find(x, numeric = TRUE))) {
    objs <- lapply(pos, function(pos, x) get(x, pos = pos),
                   x = x)
    where <- names(pos)
    visible <- rep.int(TRUE, length(pos))
  }
  if (length(grep(".", x, fixed = TRUE))) {
    np <- length(parts <- strsplit(x, ".", fixed = TRUE)[[1L]])
    for (i in 2:np) {
      gen <- paste(parts[1L:(i - 1)], collapse = ".")
      cl <- paste(parts[i:np], collapse = ".")
      if (gen == "" || cl == "")
        next
      Call <- substitute(getS3method(gen, cl, TRUE), list(gen = gen,
                                                          cl = cl))
      f <- eval.parent(Call)
      if (!is.null(f) && !is.null(environment(f))) {
        ev <- topenv(environment(f), baseenv())
        nmev <- if (isNamespace(ev))
          getNamespaceName(ev)
        else NULL
        objs <- c(objs, f)
        msg <- paste("registered S3 method for", gen)
        if (!is.null(nmev))
          msg <- paste(msg, "from namespace", nmev)
        where <- c(where, msg)
        visible <- c(visible, FALSE)
      }
    }
  }
  for (i in loadedNamespaces()) {
    ns <- asNamespace(i)
    if (exists(x, envir = ns, inherits = FALSE)) {
      f <- get(x, envir = ns, inherits = FALSE)
      objs <- c(objs, f)
      where <- c(where, paste("namespace", i, sep = ":"))
      visible <- c(visible, FALSE)
    }
  }
  ln <- length(objs)
  dups <- rep.int(FALSE, ln)
  if (ln > 1L)
    for (i in 2L:ln) for (j in 1L:(i - 1L)) if (identical(objs[[i]],
                                                          objs[[j]], ignore.environment = TRUE)) {
      dups[i] <- TRUE
      break
    }
  res <- list(name = x, objs = objs, where = where, visible = visible,
              dups = dups)
  class(res) <- "getAnywhere"

  #don't show the full response!
  #res
  # Might return multiple objects in a list if multiple functions with the same name
  return(gsub("\n", "\r\n", gsub("\"", "\\\"", objs[1])))
}



#' Package a function's dependencies into a base64 encoded string
#'
#' Find a function's in-memory and package dependencies, and turn them into a base-64 encoded zip file. This string is used in the publish API call to upload dependencies to the server.
#'
#' @param functionName function to package dependencies from
#' @return list containing the guid for the rdta file and the encoded zip
#'
#' @import utils
#'
#' @keywords internal
packDependencies <- function(functionName) {

  # Recursive step for package packaging
  recurPkg <- function(pkgName, pkgList) {
    # if the package isn't already in the list
    if (!(pkgName %in% pkgList)) {
      # add it
      pkgList <- c(pkgName, pkgList)

      # if the package is available on a repo
      if (pkgName %in% row.names(utils::available.packages())) {
        # iterate through the dependencies and check if need to add them
        for (pkg in strsplit(utils::available.packages()[pkgName, "Depends"], split=", ")[[1]]) {
          # filter out duplicates, R version dependencies, and base packages
          if (!(pkg %in% pkgList) && !(grepl("R \\((.*)\\)", pkg)) && (pkg %in% row.names(available.packages()))) {
            # recursively call recurPkg
            pkgList <- recurPkg(pkg, pkgList)
          }
        }
        # iterate through imports
        for (pkg in strsplit(available.packages()[pkgName, "Imports"], split=", ")[[1]]) {
          # filter out duplicates, R version dependencies, and base packages
          if (!(pkg %in% pkgList) && !(grepl("R \\((.*)\\)", pkg)) && (pkg %in% row.names(available.packages()))) {
            # recursively call recurPkg
            pkgList <- recurPkg(pkg, pkgList)
          }
        }
      }
    }
    # return updated list of packages
    return(pkgList)
  }

  # Recursive step for object packaging
  # NOTE: will not work if the user function specifies the names directly, e.g. won't find rjson::toJSON
  # from findGlobals man page: "R semantics only allow variables that might be local to be identified"
  recurDep <- function(objName, depList, pkgList) {
    # findGlobals() gets all external dependencies
    # Iterate over them
    for (obj in codetools::findGlobals(get(objName))) {
      name = get(obj)

      # filter out primitives and duplicates
      if (is.primitive(name) || (obj %in% names(depList))) {
        next
      }
      # non-function object dependencies
      else if (!is.function(name)) {
        depList[[obj]] <- name

        # Use the object's class to find package dependencies
        objClass <- class(name)

        # iterate through the class vector looking for packages
        for (class in objClass) {
          tryCatch({
            # get the name of the package the class belongs to
            nameEnv <- environment(get(class))
            # filter out basic environment
            if (!(identical(nameEnv, NULL)) && !(identical(nameEnv, .BaseNamespaceEnv))) {
              pkgList <- recurPkg(paste(getNamespaceName(nameEnv)), pkgList)
            }
          # if unable to find package, continue
          }, error = function(e) {
            sprintf("%s not found", obj)
          })
        }
      }
      # user defined functions
      else if (identical(environment(name), globalenv())) {
        depList[[obj]] <- name
        results <- recurDep(obj, depList, pkgList)
        depList <- results$dependencies
        pkgList <- results$packages
      }
      # functions from packages
      else if (paste(getNamespaceName(environment(name))) != "base") {
        pkgList <- recurPkg(paste(getNamespaceName(environment(name))), pkgList)
      }
    }
    return(list("dependencies"=depList, "packages"=pkgList))
  }

  # call recurDep on the desired function and with empty lists
  results <- recurDep(functionName, list(), list())
  dependencies <- results$dependencies
  packages <- results$packages

  # save current path to restore later
  start = getwd()
  # go to package library, doing this to prevent zipping entire path to package
  toPack <- packages
  toZip = vector()
  for (i in 1:length(.libPaths())) {
    setwd(.libPaths()[i])
    # try to find the package in the directory and zip it
    for (pkg in toPack) {
      if (file.exists(pkg)) {
        # save it to original directory
        zip(paste(start, paste(pkg, "zip", sep="."), sep="/"), pkg)
        toZip <- c(toZip, paste(pkg, "zip", sep="."))
        # remove the package from the list of packages to pack
        toPack <- toPack[toPack != pkg]
      }
    }

    # if done packing, break
    if (length(toPack) == 0) {
      break
    }
  }

  # go back to where the user started
  setwd(start)

  # make sure that all packages were found
  if (length(toPack) > 0) {
    stop("Error: unable to locate one or more packages. Please make sure the packages used are in at least one of the library paths.")
  }

  # generate a GUID to act as a file name to store packages, R data
  guid = gsub("-", "", uuid::UUIDgenerate(use.time=TRUE))
  # dump objects, functions, etc. into .rdta file
  if (length(dependencies) > 0) {
    # maybe can save directly as a .zip and skip the zip() call?
    save(dependencies, file=guid)
    toZip <- c(toZip, guid)
  }

  # zip up everything
  if (length(toZip) > 0) {
    zip(zipfile=guid, files=toZip)
    zipEnc <- base64enc::base64encode(paste(guid, ".zip", sep=""))

    # delete the packages
    for (pkg in packages) {
      file.remove(paste(pkg, "zip", sep="."))
    }

    # delete the dependency rdta file
    if (length(dependencies) > 0) {
      file.remove(guid)
      file.remove(paste(guid,"zip",sep="."))
    }

    # return the encoded zip as a string
    return(list(guid, zipEnc))
    #return(list(guid))
  }

  # if nothing was zipped, return empty string to indicate
  # returning two things because unable to return variable amounts
  else {
    return(list(guid, ""))
  }
}



#' Convert input schema to API expected format.
#'
#' Helper function to convert the user-friendly input and output schema parameters to the publishWebService() function to the format expected by the API.
#'
#' @param argList list of expected input parameters in the format expected by \code{\link{publishWebService}}
#' @return list of the format expected by the API
#'
#' @keywords internal
publishPreprocess <- function(argList) {
  form <- list()
  for (arg in names(argList)) {
    type = argList[[arg]]

    if (type == "float" || type == "double") {
      form[[ arg ]] <- list("type"="number", "format"=type)
    }
    else if (type == "date-time" || type == "string" || type == "time-span") {
      form[[arg]] <- list("type"="string", "format"=type)
    }
    else if (type == "uint16" || type == "int16" || type == "uint32" || type == "int32" || type == "uint64" || type == "int64") {
      form[[arg]] <- list("type"="integer", "format"=type)
    }
    else if (type == "int") {
      form[[arg]] <- list("type"="integer", "format"="int32")
    }
    else if (type == "bool" || type == "boolean") {
      form[[arg]] <- list("type"="boolean")
    }
    else {
      stop(sprintf("Error: data type \"%s\" not supported", type), call. = TRUE)
    }
  }
  return(form)
}



#' Publish a function to Microsoft Azure
#'
#' Publish a function to Microsoft Azure Machine Learning as a web service. The web service created is a standard Azure ML web service, and can be utilized from any web or mobile platform. as long as the user has the API key and URL. The function to be published can use arbitrary objects and packages. Currently, the function to be published can only take in primitive data types as input, i.e. no data frames or lists, but support for those functions will be added in the future.
#'
#' @export
#'
#' @param functionName function name as a string to be published
#' @param serviceName name of the new web service
#' @param inputSchema list of input parameters of format \code{list("arg1"="type", "arg2"="type", ...)}
#' @param outputSchema list of outputs of format \code{list("output1"="type", "output2"="type", ...)}
#' @param wkID the workspace ID
#' @param authToken primary authorization token
#' @return nested list, the first element is a list containing information about the new web service, the second element is a list of its endpoints
#'
#' @seealso \code{\link{getWSDetails}} \code{\link{getEndpoints}} \code{\link{discoverSchema}} \code{\link{consumeLists}}
#' @family publishing functions
#'
#' @examples
#' \dontrun{
#' add <- function(x,y) { return(x+y) }
#' newService <- publishWebService("add", "add",
#'  list("x"="int","y"="int"), list("z"="int"), wsID, authToken)
#' webserviceDetails <- newService[[1]]
#' endpoints <- newService[[2]]
#' helpURL <- endpoints[[1]]$HelpLocation
#' pKey <- endpoints[[1]]$PrimaryKey
#' apiURL <- endpoints[[1]]$ApiLocation
#' }
publishWebService <- function(functionName, serviceName, inputSchema, outputSchema, wkID, authToken) {

  # Make sure schema inputted matches function signature
  if (length(formals(functionName)) != length(inputSchema)) {
    stop(sprintf("Input schema does not contain the proper input. You provided %s inputs and %s were expected",length(inputSchema),length(formals(functionName))), call. = TRUE)
  }
  inputSchema <- publishPreprocess(inputSchema)
  outputSchema <- publishPreprocess(outputSchema)

  # Accept SSL certificates issued by public Certificate Authorities
  options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

  # Get and encode the dependencies
  zipString = packDependencies(functionName)

  # Build the body of the request, differing on whether or not there is a zip to upload
  if (zipString[[2]] == "") {
    req = list(
      "Name" = serviceName,
      "Type" = "Code",
      "CodeBundle" = list(
        "InputSchema" = inputSchema,
        "OutputSchema" = outputSchema,
        "Language" = "r-3.1-64",
        "SourceCode" = sprintf(wrapper, length(outputSchema), paste(sprintf("\"%s\"", names(outputSchema)), collapse=","), zipString[[1]], zipString[[1]], paste(getFunctionString(functionName)))
      )
    )
  }
  else {
    req = list(
      "Name" = serviceName,
      "Type" = "Code",
      "CodeBundle" = list(
        "InputSchema" = inputSchema,
        "OutputSchema" = outputSchema,
        "Language" = "r-3.1-64",
        "SourceCode" = sprintf(wrapper, length(outputSchema), paste(sprintf("\"%s\"", names(outputSchema)), collapse=","), zipString[[1]], zipString[[1]], paste(getFunctionString(functionName))),
        "ZipContents" = zipString[[2]]
      )
    )
  }


  # convert the payload to JSON as expected by API
  body = rjson::toJSON(req)

  # Response gatherer
  h = RCurl::basicTextGatherer()
  h$reset()

  # Generate unique guid to serve as the web service ID
  guid = gsub("-", "", uuid::UUIDgenerate(use.time=TRUE))

  # API call
  RCurl::httpPUT(url = sprintf(publishURL, wkID, guid), # defined above
                 httpheader=c('Authorization' = paste('Bearer', authToken, sep=' '),
                              'Content-Type' = 'application/json',
                              'Accept' = 'application/json'),
                 content = body,
                 writefunction = h$update)

  # Format output
  newService <- rjson::fromJSON(h$value())
  print(newService)

  # Use discovery functions to get endpoints for immediate use
  endpoints <- getEndpoints(wkID, authToken, newService["Id"])

  # currently returning list of webservice details (as a list) and endpoint details (as a list) in that order
  return(list("serviceDetails"=newService, "endpoints"=endpoints))
}



#' Update a Published Web Service
#'
#' Update a web service, i.e. change the underlying R code that the service will run when called. The same restrictions that apply to publishWebService() also apply to updateWebService()
#'
#' @export
#'
#' @inheritParams publishWebService
#' @param wsID ID of the web service to be updated
#' @return List of webservice details, default endpoint details, and the consumption function
#'
#' @seealso \code{\link{getWSDetails}} \code{\link{getEndpoints}} \code{\link{discoverSchema}} \code{\link{consumeLists}}
#' @family publishing functions
#'
#' @examples
#' \dontrun{
#' add1 <- function(x) { return(x+1) }
#' addService <- publishWebService("add1", "add1",
#'  list("x"="int"), list("z"="int"), wsID, wsAuth)
#'
#' add2 <- function(x) { return(x+2) }
#' addService <- updateWebService("add2", "add2", addService[[1]]$Id,
#'  list("x"="int"), list("z"="int"), wsID, wsAuth)
#' }
updateWebService <- function(functionName, serviceName, wsID, inputSchema, outputSchema, wkID, authToken) {

  # Make sure schema inputted matches function signature
  if (length(formals(functionName)) != length(inputSchema)) {
    stop(sprintf("Input schema does not contain the proper input. You provided %s inputs and %s were expected",length(inputSchema),length(formals(functionName))), call. = TRUE)
  }
  inputSchema <- publishPreprocess(inputSchema)
  outputSchema <- publishPreprocess(outputSchema)

  # Accept SSL certificates issued by public Certificate Authorities
  options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

  # Get and encode the dependencies
  zipString = packDependencies(functionName)

  # Build the body of the request, differing on whether or not there is a zip to upload
  if (zipString[[2]] == "") {
    req = list(
      "Name" = serviceName,
      "Type" = "Code",
      "CodeBundle" = list(
        "InputSchema" = inputSchema,
        "OutputSchema" = outputSchema,
        "Language" = "r-3.1-64",
        "SourceCode" = sprintf(wrapper, length(outputSchema), paste(sprintf("\"%s\"", names(outputSchema)), collapse=","), zipString[[1]], zipString[[1]], paste(getFunctionString(functionName)))
      )
    )
  }
  else {
    req = list(
      "Name" = serviceName,
      "Type" = "Code",
      "CodeBundle" = list(
        "InputSchema" = inputSchema,
        "OutputSchema" = outputSchema,
        "Language" = "r-3.1-64",
        "SourceCode" = sprintf(wrapper, length(outputSchema), paste(sprintf("\"%s\"", names(outputSchema)), collapse=","), zipString[[1]], zipString[[1]], paste(getFunctionString(functionName))),
        "ZipContents" = zipString[[2]]
      )
    )
  }

  # convert the payload to JSON as expected by API
  # TODO: consolidate json packages, i.e. use only one if possible
  body = rjson::toJSON(req)

  # Response gatherer
  h = RCurl::basicTextGatherer()
  h$reset()

  # API call
  RCurl::httpPUT(url = sprintf(publishURL, wkID, wsID),
                 httpheader=c('Authorization' = paste('Bearer', authToken, sep=' '),
                              'Content-Type' = 'application/json',
                              'Accept' = 'application/json'),
                 content = body,
                 writefunction = h$update)

  # Format output
  updatedService <- rjson::fromJSON(h$value())

  # Use discovery functions to get default endpoint for immediate use
  endpoints <- getEndpoints(wkID, authToken, updatedService["Id"])

  return(list("serviceDetails"=updatedService, "endpoints"=endpoints))
}
