# Convert input schema to API expected format.
#
# Helper function to convert the user-friendly input and output schema parameters to the publishWebService() function to the format expected by the API.
#
# @param argList list of expected input parameters in the format expected by \code{\link{publishWebService}}
# @return list of the format expected by the API
#
# @keywords internal
convertArgsToAMLschema <- function(argList) {
  form = list()
  for (arg in names(argList)) {
    type = argList[[arg]]
    
    if ("numeric" %in% type || "double" %in% type) {
      form[[ arg ]] = list("type"="number", 
                           "format"="double")
    }
    else if ("date-time" %in% type || "time-span" %in% type) {
      form[[arg]] = list("type"="string", 
                         "format"=type)
    }
    else if (any(c("uint16", "int16", "uint32", "int32", "uint64", "int64") %in% type)) {
      form[[arg]] = list("type"="integer", 
                         "format"=type)
    }
    else if ("integer" %in% type) {
      form[[arg]] = list("type"="integer", 
                         "format"="int32")
    }
    else if (any(c("logical", "bool", 
                   "boolean") %in% type)) {
      form[[arg]] = list("type"="boolean")
    }
    else if (any(c("character", "string", "factor", "ordered") %in% type)) {
      form[[arg]] = list("type"="string", 
                         "format"="string")
    }
    else {
      stop(sprintf("Error: data type \"%s\" not supported", type), call. = TRUE)
    }
  }
  return(form)
}


testAllowedTypes <- function(x){
  allowedTypes <- c("numeric", "double", 
                    "date-time", "time-span", 
                    "uint16", "int16", "uint32", "int32", "uint64", "int64", "integer", 
                    "logical", "bool", 
                    "character", "string", "factor", "ordered")
  all(sapply(x, function(x)x %in% allowedTypes))
}

inputSchemaIsDataframe <- function(x){
  inherits(x, "data.frame") || "data.frame" %in% attr(x, "original.class")
}

# Convert input schema to API expected format.
#
# Helper function to convert the user-friendly input and output schema parameters to the publishWebService() function to the format expected by the API.
#
# @param argList list of expected input parameters in the format expected by \code{\link{publishWebService}}
# @return list of the format expected by the API
#
# @keywords internal
# @examples
# azureSchema(list(x=3))
# azureSchema(iris)
# azureSchema(list(input1 = iris, input2 = cars))
azureSchema <- function(object){
  if(inherits(object, "azureSchema")) return(object)
  if(all(sapply(object, typeof) != "list") && (testAllowedTypes(object))){
    z <- convertArgsToAMLschema(object)
    class(z) <- c("azureSchema", class(z))
    attr(z, "original.class") <- class(object)
    return(z)
  }
  if(!is.list(object)) stop("object must be a list")
  if(is.data.frame(object)) {
    z <- convertArgsToAMLschema(lapply(object, class))
    class(z) <- c("azureSchema", class(z))
    attr(z, "original.class") <- class(object)
    return(z)
  }
  z <- rapply(object, class, how = "replace")
  idx <- sapply(object, is.list)
  if(length(idx) > 0){
    z[idx] <- lapply(object[idx], function(x)convertArgsToAMLschema(lapply(x, class)))
  }
  if(length(!idx) > 0){
    z[!idx] <- convertArgsToAMLschema(lapply(object[!idx], class))
  }
  class(z) <- c("azureSchema", class(z))
  attr(z, "original.class") <- class(object)
  z
}

print.azureSchema <- function(x, ...){
  str(x)
}