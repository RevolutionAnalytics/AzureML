\dontrun{
  # Use a default configuration in ~/.azureml, alternatively
  # see help for `workspace`.
  ws <- workspace()
  
  # Really simple example:
  add <- function(x,y) x + y
  endpoint <- publishWebService(ws, 
                                fun = add, 
                                name = "addme", 
                                inputSchema = list(x="numeric", 
                                                   y="numeric"), 
                                outputSchema = list(ans="numeric"))
  consume(endpoint, list(x=pi, y=2))

  # Now remove the web service named "addme" that we just published
  deleteWebService(ws, "addme")

  # The publishWebService uses `miniCRAN` to include dependencies on
  # packages required by your function. The next example uses the
  # `digest` function from the digest package:
  ep <- publishWebService(ws, fun=function(x) digest::digest(x), name="digest",
                          inputSchema=list(x="character"),
                          outputSchema=list(ans="character"),
                          packages="digest")

  md5 <- consume(ep, list(x="test"))

  # Compare, and then remove the web service:
  cat(md5$ans, "\t", digest::digest("test"), "\n")
  deleteWebService(ws, "digest")
  
  
  # A neat trick to evaluate any expression in the Azure ML virtual
  # machine R session and view its output:
  ep <- publishWebService(ws, 
                          fun = function(expr) {
                            paste(capture.output(
                              eval(parse(text=expr))), collapse="\n")
                          },
                          name="commander", 
                          inputSchema = list(x = "character"),
                          outputSchema = list(ans = "character"))
  cat(consume(ep, list(x = "getwd()"))$ans)
  cat(consume(ep, list(x = ".packages(all=TRUE)"))$ans)
  cat(consume(ep, list(x = "R.Version()"))$ans)

  # Remove the service we just published
  deleteWebService(ws, "commander")
  
  # The following example illustrates scoping rules. Note that the function
  # refers to the variable y defined outside the function body. That value
  # will be exported with the service.
  y <- pi
  ep <- publishWebService(ws, 
                          fun = function(x) x + y, 
                          name = "lexical scope",
                          inputSchema = list(x = "numeric"), 
                          outputSchema = list(ans = "numeric"))
  cat(consume(ep, list(x=2))$ans)
  
  # Remove the service we just published
  deleteWebService(ws, "lexical scope")
  
  # Example showing the use of consume to score all the rows of a data frame
  # at once, and other invocations for evaluating multiple sets of input
  # values. The columns of the data frame correspond to the input parameters
  # of the web service in this example:
  f <- function(a,b,c,d) list(sum = a+b+c+d, prod = a*b*c*d)
  ep <-  publishWebService(ws, 
                           f, 
                           name = "rowSums",
                           inputSchema = list(
                             a="numeric", 
                             b="numeric", 
                             c="numeric", 
                             d="numeric"
                           ),
                           outputSchema = list(
                             sum ="numeric", 
                             prod = "numeric")
  )
  x <- head(iris[,1:4])  # First four columns of iris
  
  # Note the following will FAIL because of a name mismatch in the arguments
  # (with an informative error):
  consume(ep, x, retryDelay=1)
  # We need the columns of the data frame to match the inputSchema:
  names(x) <- letters[1:4]
  # Now we can evaluate all the rows of the data frame in one call:
  consume(ep, x)
  # output should look like:
  #    sum    prod
  # 1 10.2   4.998
  # 2  9.5   4.116
  # 3  9.4  3.9104
  # 4  9.4   4.278
  # 5 10.2    5.04
  # 6 11.4 14.3208

  # You can use consume to evaluate just a single set of input values with this
  # form:
  consume(ep, a=1, b=2, c=3, d=4)

  # or, equivalently,
  consume(ep, list(a=1, b=2, c=3, d=4))

  # You can evaluate multiple sets of input values with a data frame input:
  consume(ep, data.frame(a=1:2, b=3:4, c=5:6, d=7:8))

  # or, equivalently, with multiple lists:
  consume(ep, list(a=1, b=3, c=5, d=7), list(a=2, b=4, c=6, d=8))
  
  # Remove the service we just published
  deleteWebService(ws, "rowSums")
}
