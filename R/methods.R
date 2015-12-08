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


#' @title Test if an object is an Azure ML workspace.
#' @param x an R object
#' @return logical value, TRUE if \code{x} represents an Azure ML workspace.
#' @export
is.Workspace = function(x) "Workspace" %in% class(x)

#' @title Test if an object is an Azure ML Service.
#' @param x an R object
#' @return logical value, TRUE if \code{x} represents an Azure ML web service
#' @export
is.Service = function(x){
  inherits(x, "Service")
}

#' @title Test if an object is an Azure ML Endpoint
#' @param x an R object
#' @return logical value, TRUE if \code{x} represents an Azure ML web service endpoint
#' @export
is.Endpoint = function(x){
  inherits(x, "Endpoint")
}

#' @export
print.Workspace =  function(x, ...)
{
  cat("AzureML Workspace\n")
  cat("Workspace ID: ", x$id, "\n")
  cat("API endpoint:", x$.api_endpoint, "\n")
}

#' @export
print.Experiments = function(x, ...)
{
  dots = character()
  if(nrow(x) > 0) dots = "..."
  d = data.frame(
    Description = substr(x[, "Description"], 1, 48),
    CreationTime = x[, "CreationTime"],
    `...` = dots
  )
  print(d)
  cat("-------------------------------------------------\n")
  cat("AzureML experiments data.frame variables include:\n")
  cat(paste(capture.output(names(x)),collapse="\n"),"\n")
  d
}

#' @export
print.Datasets = function(x, ...)
{
  dots = character()
  if(nrow(x) > 0) dots = "..."
  d = data.frame(
    Name = substr(x[, "Name"], 1, 50), 
    DataTypeId = x[, "DataTypeId"],
    Size = x[, "Size"],
    `...` = dots
  )
  print(d)
  cat("----------------------------------------------\n")
  cat("AzureML datasets data.frame variables include:\n")
  cat(paste(capture.output(names(x)),collapse="\n"),"\n")
  d
}
