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


# `wrapper` is the expression executed in the AzureML R environment.  The publishWebService function sets up the environment "exportenv" from which this expression follows.

# Note that exposing wrapperFunction() and wrapper will cause R CMD BUILD failures
# The workaround is to comment out the wrapper function, and replace it with the text.
# To update the function, uncomment the following function.


### --- Do not remove this uncommented code ------------------------------------

# wrapperFunction <- function(){
#   inputDF <- maml.mapInputPort(1)
#   load('src/env.RData')
#   if(!is.null(exportenv$..packages))
#   {
#     lapply(exportenv$..packages, function(pkg){
#       if(!require(pkg, character.only = TRUE, quietly = TRUE))
#         install.packages(pkg, 
#                        repos = paste0('file:///', getwd(), '/src/packages'), 
#                        lib = getwd()
#       )
#     })
#     .libPaths(new = getwd())
#     lapply(exportenv$..packages, require, 
#            quietly = TRUE, character.only=TRUE)
#   }
#   parent.env(exportenv) = globalenv()
#   
#   attach(exportenv, warn.conflicts = FALSE)
#   if(..data.frame){
#     outputDF <- as.data.frame(..fun(inputDF))
#     colnames(outputDF) <- ..output_names
#   } else {
#     outputDF <- matrix(nrow = nrow(inputDF),
#                        ncol = length(..output_names)
#     )
#     outputDF <- as.data.frame(outputDF)
#     names(outputDF) <- ..output_names
#     for(i in 1:nrow(inputDF)){
#       outputDF[i, ] <- do.call('..fun', inputDF[i, ])
#     }
#   }
#   maml.mapOutputPort("outputDF")
# }
# 
# wrapper <- paste(as.character(body(wrapperFunction)[-1]), 
#                  collapse = "\n")

### --- End of Do not remove ---------------------------------------------------

wrapper <- "inputDF <- maml.mapInputPort(1)\nload(\"src/env.RData\")\nif (!is.null(exportenv$..packages)) {\n    lapply(exportenv$..packages, function(pkg) {\n        if (!require(pkg, character.only = TRUE, quietly = TRUE)) \n            install.packages(pkg, repos = paste0(\"file:///\", getwd(), \"/src/packages\"), lib = getwd())\n    })\n    .libPaths(new = getwd())\n    lapply(exportenv$..packages, require, quietly = TRUE, character.only = TRUE)\n}\nparent.env(exportenv) = globalenv()\nattach(exportenv, warn.conflicts = FALSE)\nif (..data.frame) {\n    outputDF <- as.data.frame(..fun(inputDF))\n    colnames(outputDF) <- ..output_names\n} else {\n    outputDF <- matrix(nrow = nrow(inputDF), ncol = length(..output_names))\n    outputDF <- as.data.frame(outputDF)\n    names(outputDF) <- ..output_names\n    for (i in 1:nrow(inputDF)) {\n        outputDF[i, ] <- do.call(\"..fun\", inputDF[i, ])\n    }\n}\nmaml.mapOutputPort(\"outputDF\")"



# Test the AzureML wrapper locally
# @param inputDF data frame
# @param wrapper the AzureML R wrapper code
# @param fun a function to test
# @param output_names character vector of function output names
# @param data.frame i/o format
# @examples
# foo <- function(dat)head(dat, 10)
# test_wrapper(foo, iris)
test_wrapper <- function(fun = function(x)head(x, 3), inputDF, `data.frame` = TRUE)
{
  if(missing(inputDF) || is.null(inputDF)){
    # replicate first 5 lines of iris
    # this is a workaround to pass R CMD check
    iris <- data.frame(
      Sepal.Length = c(5.1, 4.9, 4.7, 4.6, 5, 5.4), 
      Sepal.Width = c(3.5, 3, 3.2, 3.1, 3.6, 3.9), 
      Petal.Length = c(1.4, 1.4, 1.3, 1.5, 1.4, 1.7), 
      Petal.Width = c(0.2, 0.2, 0.2, 0.2, 0.2, 0.4), 
      Species = factor(rep(1, 6), levels = 1:3, labels = c("setosa", "versicolor", "virginica"))
    )
    inputDF <- iris
  }
  exportenv = new.env()
  maml.mapInputPort <- function(x) as.data.frame(inputDF)
  maml.mapOutputPort <- function(x) get(x)
  load <- function(x) invisible()
  exportenv$..fun = fun
  exportenv$..output_names = if(`data.frame`) {
    names(match.fun(fun)(inputDF))
  } else {
    do.call(match.fun(fun), inputDF)
  }
  exportenv$..data.frame = `data.frame`
  eval(parse(text = wrapper))
}

