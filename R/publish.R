# `wrapper` is the expression executed in the AzureML R environment.  The
# publishWebService function sets up the environment "exportenv" from which
# this expression follows.

wrapper = "inputDF <- maml.mapInputPort(1)\r\nload('src/env.RData')\r\n outputDF <- data.frame(matrix(NA, ncol=length(exportenv$..output_names), nrow=nrow(inputDF)))\r\n names(outputDF) <- exportenv$..output_names\r\n if(!is.null(exportenv$..packages))\r\n {\r\n install.packages(exportenv$..packages, repos=paste('file:///',getwd(),'/src/packages',sep=''), lib=getwd());.libPaths(new=getwd())\r\n}\r\nparent.env(exportenv) = globalenv()\n\nattach(exportenv, warn.conflicts=FALSE)\n\nfor (i in 1:nrow(inputDF)){\r\n  outputDF[i,] <- do.call('..fun', as.list(inputDF[i,]))\r\n}\r\nmaml.mapOutputPort(\"outputDF\")"


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
    
    if (type == "numeric" || type == "double") {
      form[[ arg ]] = list("type"="number", "format"="double")
    }
    else if (type == "date-time" || type == "time-span") {
      form[[arg]] = list("type"="string", "format"=type)
    }
    else if (type == "uint16" || type == "int16" || type == "uint32" || type == "int32" || type == "uint64" || type == "int64") {
      form[[arg]] = list("type"="integer", "format"=type)
    }
    else if (type == "integer") {
      form[[arg]] = list("type"="integer", "format"="int32")
    }
    else if (type == "logical" || type == "bool" || type == "boolean") {
      form[[arg]] = list("type"="boolean")
    }
    else if (type == "character" || type == "string") {
      form[[arg]] = list("type"="string", "format"="string")
    }
    else {
      stop(sprintf("Error: data type \"%s\" not supported", type), call. = TRUE)
    }
  }
  return(form)
}



#' Publish a function as a Microsoft Azure Web Service
#'
#' Publish a function to Microsoft Azure Machine Learning as a web service. The
#' web service created is a standard Azure ML web service, and can be used
#' from any web or mobile platform as long as the user knows the API key and URL.
#' The function to be published is limited to inputs/outputs consisting of
#' lists of scalar values.
#'
#' @export
#'
#' @inheritParams refresh
#' @param fun a function to publish; the function must have at least one argument
#' @param name name of the new web service
#' @param inputSchema a list of \code{fun} input parameters and their AzureML types
#'   formatted as \code{list("arg1"="type", "arg2"="type", ...)}; see the note below
#'   for details
#' @param outputSchema list of \code{fun} outputs and AzureML types,
#'   formmated as \code{list("output1"="type", "output2"="type", ...)}
#' @param export optional character vector of variable names to explicitly export
#'   in the web service for use by the function. See the note below.
#' @param noexport optional character vector of variable names to prevent from exporting
#'  in the web service
#' @param packages optional character vector of R packages required by the function
#' @param version optional R version string for required packages (the version of R running in the AzureML Web Service)
#' @param wsid optional Azure web service ID; use to update an existing service (see Note below)
#' @param host optional Azure regional host, defaulting to the global \code{management_endpoint} set in
#' \code{\link{workspace}}.
#' 
#' @return A data.frame describing the new service endpoints, cf. \code{\link{endpoints}}. The output can be directly used by the \code{\link{consume}} function.
#'  
#' @note AzureML data types are different than, but related to, R types. You may specify the R types \code{numeric, logical, integer,} and \code{character} and those will be specified as AzureML types \code{double, boolean, int32, string}, respectively.
#'
#' Leave the \code{wsid} parameter undefined to create a new AzureML web service, or specify the ID of an existing web service to update it, replacing the function and required R pacakges with new values. Although the API allows that the name, input and output schema to also be specified when updating it's not possible to change those values.
#' 
#' The \code{\link{updateWebService}} function is nearly an alias for \code{\link{publishWebService}}, differing only in that the \code{wsid} parameter is required by \code{\link{updateWebService}}.
#'
#' The \code{publishWebService} function automatically exports objects required by the function to a working environment in the AzureML machine, including objects accessed within the function using lexical scoping rules. Use the \code{exports} parameter to explicitly include other objects that are needed. Use \code{noexport} to explicitlt prevent objects from being exported.
#' 
#' @seealso \code{\link{endpoints}} \code{\link{discoverSchema}} \code{\link{consume}} \code{\link{services}}
#' @family publishing functions
#'
#' @example inst/examples/example_publish.R
#' @importFrom jsonlite toJSON
#' @importFrom uuid UUIDgenerate
#' @importFrom curl new_handle handle_setheaders handle_setopt
publishWebService = function(ws, fun, name,
                             inputSchema, outputSchema,
                             export=character(0), noexport=character(0), packages,
                             version="3.1.0", wsid, host = ws$.management_endpoint)
{
  if(!is.Workspace(ws)) stop("ws must be a workspace object")
  if(!zipAvailable()) stop(zipNotAvailableMessage)
  if(missing(wsid) && as.character(match.call()[1]) == "updateWebService")
    stop("updateWebService requires that the wsid parameter is specified")
  if(missing(wsid)) wsid = gsub("-", "", UUIDgenerate(use.time=TRUE))
  publishURL = sprintf("%s/workspaces/%s/webservices/%s",
                       host, ws$id, wsid)
  # Make sure schema inputted matches function signature
  if (length(formals(fun)) != length(inputSchema))
  {
    stop(sprintf("Input schema does not contain the proper input. You provided %s inputs and %s were expected",length(inputSchema),length(formals(fun))), call. = TRUE)
  }
  inputSchema = azureSchema(inputSchema)
  outputSchema = azureSchema(outputSchema)
  
  # Get and encode the dependencies
  if(missing(packages)) packages=NULL
  exportenv = new.env()
  .getexports(substitute(fun), exportenv, parent.frame(), good=export, bad=noexport)
  # Assign required objects in the export environment
  assign("..fun", fun, envir=exportenv)
  assign("..output_names", names(outputSchema), envir=exportenv)
  
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
  r = curl_fetch_memory(publishURL, handle = h)
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
#' @note If more than one service matches the supplied \code{name}, the first listed service will be deleted.
#' @return The updated data.frame of workspace services is invisibly returned.
#' @seealso \code{\link{services}} \code{\link{publishWebService}} \code{\link{updateWebService}}
#' @family publishing functions
#' @example inst/examples/example_publish.R
deleteWebService = function(ws, name)
{
#DELETE https://management.azureml.net/workspaces/{id}/webservices/{id}[/endpoints/{name}]
  if(is.data.frame(name) || is.list(name))
  {
    name = name$Id[1]
  } else
  {
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
  refresh(ws, "services")
  invisible(ws$services)
}
