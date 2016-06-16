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


# `wrapper` is the expression executed in the AzureML R environment.  The
# publishWebService function sets up the environment "exportenv" from which
# this expression follows.

wrapperFunction <- function(){
  inputDF <- maml.mapInputPort(1)
  load('src/env.RData')
  if(!is.null(exportenv$..packages))
  {
    lapply(exportenv$..packages, function(pkg){
      if(!require(pkg, character.only = TRUE, quietly = TRUE))
        install.packages(pkg, 
                       repos = paste0('file:///', getwd(), '/src/packages'), 
                       lib = getwd()
      )
    })
    .libPaths(new = getwd())
    lapply(exportenv$..packages, require, 
           quietly = TRUE, character.only=TRUE)
  }
  parent.env(exportenv) = globalenv()
  
  attach(exportenv, warn.conflicts = FALSE)
  if(..data.frame){
    outputDF <- as.data.frame(..fun(inputDF))
    colnames(outputDF) <- ..output_names
  } else {
    outputDF <- matrix(nrow = nrow(inputDF),
                       ncol = length(..output_names)
    )
    outputDF <- as.data.frame(outputDF)
    names(outputDF) <- ..output_names
    for(i in 1:nrow(inputDF)){
      outputDF[i, ] <- do.call('..fun', inputDF[i, ])
    }
  }
  maml.mapOutputPort("outputDF")
}

wrapper <- paste(as.character(body(wrapperFunction)[-1]), 
                 collapse = "\n")



# Test the AzureML wrapper locally
# @param inputDF data frame
# @param wrapper the AzureML R wrapper code
# @param fun a function to test
# @param output_names character vector of function output names
# @param data.frame i/o format
# @examples
# foo <- function(dat)head(dat, 10)
# test_wrapper(foo, iris)
test_wrapper <- function(fun = function(x)head(x, 3), inputDF = iris, `data.frame` = TRUE)
{
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

