# `wrapper` is the expression executed in the AzureML R environment.  The
# publishWebService function sets up the environment "exportenv" from which
# this expression follows.

wrapper = "inputDF <- maml.mapInputPort(1)\nload('src/env.RData')\n if(!is.null(exportenv$..packages))\n {\n install.packages(exportenv$..packages, repos=paste('file:///',getwd(),'/src/packages',sep=''), lib=getwd());.libPaths(new=getwd())\n lapply(exportenv$..packages, require, quietly=TRUE, character.only=TRUE)}\nparent.env(exportenv) = globalenv()\n\nattach(exportenv, warn.conflicts=FALSE)\nif(..data.frame){outputDF <- data.frame(..fun(inputDF)); colnames(outputDF) <- ..output_names} else{outputDF <- matrix(nrow=nrow(inputDF), ncol=length(..output_names)); colnames(outputDF) <- ..output_names; outputDF <- data.frame(outputDF); for(j in 1:nrow(inputDF)){outputDF[j, ] <- do.call('..fun', as.list(inputDF[j,]))}}\nmaml.mapOutputPort(\"outputDF\")"

#' Test the AzureML wrapper locally
#' @param inputDF data frame
#' @param wrapper the AzureML R wrapper code
#' @param fun a function to test
#' @param output_names character vector of function output names
#' @param data.frame i/o format
test_wrapper = function(inputDF, wrapper, fun, output_names, `data.frame`)
{
  exportenv = new.env()
  maml.mapInputPort = function(x) inputDF
  maml.mapOutputPort = function(x) get(x)
  load = function(x) invisible()
  exportenv$..fun = fun
  exportenv$..output_names = output_names
  exportenv$..data.frame = `data.frame`
  eval(parse(text=wrapper), envir=environment())
}


#' Convert input schema to API expected format.
#'
#' Helper function to convert the user-friendly input and output schema parameters to the publishWebService() function to the format expected by the API.
#'
#' @param argList list of expected input parameters in the format expected by \code{\link{publishWebService}}
#' @return list of the format expected by the API
#'
#' @keywords internal
azureSchema = function(argList) {
  form = list()
  for (arg in names(argList)) {
    type = argList[[arg]]
    
    if ("numeric" %in% type || "double" %in% type) {
      form[[ arg ]] = list("type"="number", "format"="double")
    }
    else if ("date-time" %in% type || "time-span" %in% type) {
      form[[arg]] = list("type"="string", "format"=type)
    }
    else if (any(c("uint16", "int16", "uint32", "int32", "uint64", "int64") %in% type)) {
      form[[arg]] = list("type"="integer", "format"=type)
    }
    else if ("integer" %in% type) {
      form[[arg]] = list("type"="integer", "format"="int32")
    }
    else if (any(c("logical", "bool", "boolean") %in% type)) {
      form[[arg]] = list("type"="boolean")
    }
    else if (any(c("character", "string", "factor", "ordered") %in% type)) {
      form[[arg]] = list("type"="string", "format"="string")
    }
    else {
      stop(sprintf("Error: data type \"%s\" not supported", type), call. = TRUE)
    }
  }
  return(form)
}


#' Publish a function as a Microsoft Azure Web Service.
#'
#' Publish a function to Microsoft Azure Machine Learning as a web service. The web service created is a standard Azure ML web service, and can be used from any web or mobile platform as long as the user knows the API key and URL. The function to be published is limited to inputs/outputs consisting of lists of scalar values or single data frames (see the notes below and examples). Requires a zip program to be installed (see note below).
#'
#' @export
#'
#' @inheritParams refresh
#' @param fun a function to publish; the function must have at least one argument.
#' @param name name of the new web service; ignored when \code{serviceId} is specified (when updating an existing web service).
#' 
#' @param inputSchema either a list of \code{fun} input parameters and their AzureML types formatted as \code{list("arg1"="type", "arg2"="type", ...)}, or an example input data frame when \code{fun} takes a single data frame argument; see the note below for details.
#' 
#' @param outputSchema list of \code{fun} outputs and AzureML types, formatted as \code{list("output1"="type", "output2"="type", ...)}, optional when \code{inputSchema} is an example input data frame.
#' 
#' @param export optional character vector of variable names to explicitly export in the web service for use by the function. See the note below.
#' @param noexport optional character vector of variable names to prevent from exporting in the web service.
#' @param packages optional character vector of R packages to bundle in the web service, including their dependencies.
#' @param version optional R version string for required packages (the version of R running in the AzureML Web Service).
#' @param serviceID optional Azure web service ID; use to update an existing service (see Note below).
#' @param host optional Azure regional host, defaulting to the global \code{management_endpoint} set in \code{\link{workspace}}
#' @param data.frame \code{TRUE} indicates that the function \code{fun} accepts a data frame as input and returns a data frame output; automatically set to \code{TRUE} when \code{inputSchema} is a data frame.
#' 
#' @return A data.frame describing the new service endpoints, cf. \code{\link{endpoints}}. The output can be directly used by the \code{\link{consume}} function.
#'  
#' @note 
#' 
#' \bold{Data Types}
#' 
#' AzureML data types are different from, but related to, R types. You may specify the R types \code{numeric, logical, integer,} and \code{character} and those will be specified as AzureML types \code{double, boolean, int32, string}, respectively.
#'
#' \bold{Input and output schemas}
#'
#' Function input must be:
#' \enumerate{
#' \item named scalar arguments with names and types specified in \code{inputSchema}
#' \item one or more lists of named scalar values
#' \item a single data frame when \code{data.frame=TRUE} is specified; either explicitly specify the column names and types in \code{inputSchema} or provide an example input data frame as \code{inputSchema}
#' }
#' Function output is always returned as a data frame with column names and types specified in \code{outputSchema}. See the examples for example use of all three I/O options.
#'
#' \bold{Updating a web service}
#'
#' Leave the \code{serviceId} parameter undefined to create a new AzureML web service, or specify the ID of an existing web service to update it, replacing the function, \code{inputSchema}, \code{outputSchema}, and required R pacakges with new values. The \code{name} parameter is ignored \code{serviceId} is specified to update an existing web service.
#' 
#' The \code{\link{updateWebService}} function is nearly an alias for \code{\link{publishWebService}}, differing only in that the \code{serviceId} parameter is required by \code{\link{updateWebService}}.
#'
#' The \code{publishWebService} function automatically exports objects required by the function to a working environment in the AzureML machine, including objects accessed within the function using lexical scoping rules. Use the \code{exports} parameter to explicitly include other objects that are needed. Use \code{noexport} to explicitly prevent objects from being exported.
#' 
#' Note that it takes some time to update the AzureML service on the server.  After updating the service, you may have to wait several seconds for the service to update.  The time it takes will depend on a number of factors, including the complexity of your web service function.
#' 
#' \bold{External zip program required}
#' 
#' The function uses \code(\link[utils]{zip}) to compress information before transmission to AzureML. To use this, you need to have a zip program installed on your machine, and this program should be available in the path. The program should be called \code{zip} otherwise R may not find it. On windows, it is sufficient to install RTools (see \url{https://cran.r-project.org/bin/windows/Rtools/})
#' 
#' @seealso \code{\link{endpoints}}, \code{\link{discoverSchema}}, \code{\link{consume}} and \code{\link{services}}.
#' @family publishing functions
#'
#' @example inst/examples/example_publish.R
#' @importFrom jsonlite toJSON
#' @importFrom uuid UUIDgenerate
#' @importFrom curl new_handle handle_setheaders handle_setopt
publishWebService = function(ws, fun, name,
                             inputSchema, outputSchema, `data.frame`=FALSE,
                             export=character(0), noexport=character(0), packages,
                             version="3.1.0", serviceId, host = ws$.management_endpoint)
{
  # Perform validation on inputs
  if(!is.Workspace(ws)) stop("ws must be a workspace object")
  if(!zipAvailable()) stop(zipNotAvailableMessage)
  if(is.character(fun)) stop("You must specify 'fun' as a function, not a character")
  if(!is.function(fun)) stop("The argument 'fun' must be a function.")
  if(!is.list(inputSchema)) stop("You must specify inputSchema as either a list or a data.frame")
  
  if(missing(serviceId) && as.character(match.call()[1]) == "updateWebService")
    stop("updateWebService requires that the serviceId parameter is specified")
  if(missing(name) && !missing(serviceId)) name = "" # unused in this case
  if(missing(serviceId)) serviceId = gsub("-", "", UUIDgenerate(use.time=TRUE))
  publishURL = sprintf("%s/workspaces/%s/webservices/%s",
                       host, ws$id, serviceId)
  # Make sure schema matches function signature
  if(is.data.frame(inputSchema))
  {
    `data.frame` = TRUE
    test = match.fun(fun)(head(inputSchema))
    inputSchema = azureSchema(lapply(inputSchema, class))
    if(missing(outputSchema))
    {
      if(is.data.frame(test) || is.list(test)) outputSchema = azureSchema(lapply(test, class))
      else outputSchema = list(ans=class(test))
    }
  } else 
  {
    inputSchema = azureSchema(inputSchema)
  }
  outputSchema = azureSchema(outputSchema)
  if(`data.frame`)
  {
    if(length(formals(fun)) != 1) stop("when data.frame=TRUE fun must only take one data.frame argument")
  } else 
  {
    if(length(formals(fun)) != length(inputSchema)) stop("length(inputSchema) does not match the number of function arguments")
  }
  
  # Get and encode the dependencies
  if(missing(packages)) packages=NULL
  exportenv = new.env()
  .getexports(substitute(fun), exportenv, parent.frame(), good=export, bad=noexport)
  # Assign required objects in the export environment
  assign("..fun", fun, envir=exportenv)
  assign("..output_names", names(outputSchema), envir=exportenv)
  assign("..data.frame", `data.frame`, envir=exportenv)
  
  zipString = packageEnv(exportenv, packages=packages, version=version)
  
  # Build the body of the request
  req = list(
    Name = name,
    Type = "Code",
    CodeBundle = list(
      InputSchema = inputSchema,
      OutputSchema = outputSchema,
      Language = "R-3.1-64",
      SourceCode = wrapper,
      ZipContents = zipString
    )
  )
  body = charToRaw(
    paste(toJSON(req, auto_unbox=TRUE), collapse="\n")
  )
  h = new_handle()
  httpheader = list(
    Authorization = paste("Bearer", ws$.auth, sep=' '),
    `Content-Type` = "application/json",
    Accept = "application/json"
  )
  opts = list(
    post = TRUE,
    postfieldsize = length(body),
    postfields = body,
    customrequest = "PUT"
  )
  handle_setheaders(h, .list = httpheader)
  handle_setopt(h, .list = opts)
  r = try_fetch(publishURL, handle = h)
  result = rawToChar(r$content)
  if(r$status_code >= 400) stop(result)
  newService = fromJSON(result)
  # refresh the workspace cache
  refresh(ws, "services")
  
  # Use discovery functions to get endpoints for immediate use
  endpoints(ws, newService["Id"])
}


#' @rdname publishWebService
#' @export
updateWebService = publishWebService


#' Delete a Microsoft Azure Web Service
#'
#' Delete a Microsoft Azure Machine Learning  web service from your workspace.
#'
#' @export
#'
#' @inheritParams refresh
#' @param name Either one row from the workspace \code{services} data.frame corresponding to a service to delete, or simply a service name character string.
#' @param refresh Set to \code{FALSE} to supress automatic updating of the workspace list of services,
#' useful when deleting many services in bulk.
#' @note If more than one service matches the supplied \code{name}, the first listed service will be deleted.
#' @return The updated data.frame of workspace services is invisibly returned.
#' @seealso \code{\link{services}} \code{\link{publishWebService}} \code{\link{updateWebService}}
#' @family publishing functions
#' @example inst/examples/example_publish.R
deleteWebService = function(ws, name, refresh = TRUE)
{
  #DELETE https://management.azureml.net/workspaces/{id}/webservices/{id}[/endpoints/{name}]
  
  if(!is.Workspace(ws)) stop("Invalid ws. Please provide a workspace object")
  if(is.data.frame(name) || is.list(name)){
    name = name$Id[1]
  } else {
    name = ws$services[ws$services$Name == name, "Id"][1]
    if(is.na(name)) stop("service not found")
  }
  h = new_handle()
  handle_setheaders(h, `Authorization`=sprintf("Bearer %s",ws$.auth), .list=ws$.headers)
  handle_setopt(h, customrequest="DELETE")
  uri = sprintf("%s/workspaces/%s/webservices/%s", 
                ws$.management_endpoint, ws$id, name)
  s = curl_fetch_memory(uri, handle=h)$status_code
  if(s > 299) stop("HTTP error ",s)
  if(refresh) refresh(ws, "services")
  invisible(ws$services)
}
