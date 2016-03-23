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


# Convert input schema to API expected format.
#
# Helper function to convert the user-friendly input and output schema parameters to the publishWebService() function to the format expected by the API.
#
# @param argList list of expected input parameters in the format expected by \code{\link{publishWebService}}
# @return list of the format expected by the API
#
# @keywords internal
convertArgsToAMLschema <- function(argList) {
  form <- list()
  for (arg in names(argList)) {
    type <- argList[[arg]]
    
    form[[ arg ]] <- if (type %in% c("numeric", "double")) {
      list("type"="number", "format"="double")
    } else if (type %in% c("date-time", "time-span")){
      list("type"="string", "format"=type)
    } else if (type %in% c("uint16", "int16", "uint32", "int32", "uint64", "int64")) {
      list("type"="integer", "format"=type)
    } else if (type %in% c("integer")) {
      list("type"="integer", "format"="int32")
    } else if (type %in% c("logical", "bool", "boolean")) {
      list("type"="boolean")
    } else if (type %in% c("character", "string", "factor", "ordered")) {
      list("type"="string", "format"="string")
    } else {
      stop(sprintf("Error: data type \"%s\" not supported", type), call. = TRUE)
    }
  }
  return(form)
}

testAllowedTypes <- function(x){
  allowedTypes <- c("numeric", "double", 
                    "date-time", "time-span", 
                    "uint16", "int16", "uint32", "int32", "uint64", "int64", "integer", 
                    "logical", "bool", "boolean",
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