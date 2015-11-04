# Support functions adapted from the foreach package:
#
# Copyright (c) 2008-2010 Revolution Analytics
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

# These are all internal functions.

#' Package a Function and Dependencies into an Environment
#'
#' @param exportenv R environment to package
#' @param packages a character vector of required R package dependencies
#' @param version optional R version
#' @return A base64-encoded zip file containing the saved 'exportenv' environment
#' @import codetools
#' @importFrom base64enc base64encode
#' @importFrom miniCRAN makeRepo pkgDep
packageEnv = function(exportenv, packages=NULL, version="3.1.0")
{
  if(!is.null(packages)) assign("..packages", packages, envir=exportenv)
  d = tempfile(pattern="dir")
  on.exit(unlink(d, recursive=TRUE))
  tryCatch(dir.create(d), warning=function(e) stop(e))
  # zip, unfortunately a zip file is apparently an AzureML requirement.
  cwd = getwd()
  on.exit(setwd(cwd), add=TRUE)
  setwd(d)
  # save export environment to an RData file
  save(exportenv, file="env.RData")

  # Package up dependencies
  if(!is.null(packages))
  {
    re = getOption("repos")
    if(is.null(re)) re = c(CRAN="http://cran.revolutionanalytics.com")
    p = paste(d,"packages",sep="/")
    tryCatch(dir.create(p), warning=function(e) stop(e))
    tryCatch(makeRepo(pkgDep(packages, repos=re), path=p, re, type="win.binary", Rversion=version),
      error=function(e) stop(e))
  }

  zip(zipfile="export.zip", files=dir())
  setwd(cwd)
  base64encode(paste(d, "export.zip", sep="/"))
}

.getsyms = function(ex) {
  fun = function(x) {
    if (is.symbol(x))
      as.character(x)
    else if (is.call(x))
      .getsyms(x)
    else
      NULL
  }
  unlist(lapply(ex, fun))
}

.gather = function(x) {
  fun = function(a, b) unique(c(a, b))
  accum = list(good=character(0), bad=character(0))
  for (e in x) {
    accum = mapply(fun, e, accum, SIMPLIFY=FALSE)
  }
  accum
}

.expandsyms = function(syms, env, good, bad) {
  fun = function(sym, good, bad) {
    if (sym %in% c(good, bad)) {
      # we already saw this symbol
      list(good=good, bad=bad)
    } else if (!nzchar(sym)) {
      # apparently a symbol can be converted into an empty string,
      # but it's an error to call "exists" with an empty string,
      # so we just declare it to be bad here
      list(good=good, bad=c(sym, bad))
    } else if (exists(sym, env, mode='function', inherits=FALSE)) {
      # this is a function defined in this environment
      good = c(sym, good)
      f = get(sym, env, mode='function', inherits=FALSE)
      if (identical(environment(f), env)) {
        # it's a local function
        globs = findGlobals(f)
        if (length(globs) > 0) {
          # it's got free variables, so let's check them out
          .gather(lapply(globs, fun, good, bad))
        } else {
          # it doesn't have free variables, so we're done
          list(good=good, bad=bad)
        }
      } else {
        # it's not a local function, so we're done
        list(good=good, bad=bad)
      }
    } else if (exists(sym, env, inherits=FALSE)) {
      # it's not a function, but it's defined in this environment
      list(good=c(sym, good), bad=bad)
    } else {
      # it's not defined in this environment
      list(good=good, bad=c(sym, bad))
    }
  }
  .gather(lapply(syms, fun, good, bad))$good
}

.getexports = function(ex, e, env, good=character(0), bad=character(0)) {
  syms = .getsyms(ex)
  syms = .expandsyms(syms, env, good, bad)
  for (s in syms) {
    if (s != '...') {
      val = get(s, env, inherits=FALSE)

      # if this is a function, check if we should change the
      # enclosing environment to be this new environment
      fenv = environment(val)
      if (is.function(val) &&
          (identical(fenv, env) || identical(fenv, .GlobalEnv)))
        environment(val) = e

      assign(s, val, e)
    }
  }
  invisible(NULL)
}
