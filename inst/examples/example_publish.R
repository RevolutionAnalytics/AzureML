\dontrun{
# Use a default configuration in ~/.azureml, alternatively
# see help for `workspace`.
ws <- workspace()

# Really simple example:
add <- function(x,y) x + y
endpoint <- publishWebService(ws, add, "add_service",
              list(x="numeric", y="numeric"), list(ans="numeric"))
result <- consumeLists(endpoint, list(x=pi, y=2))
result$ans

# A neat trick to evaluate any expression  and view its output:
ep <- publishWebService(ws, fun=function(expr) {
       paste(capture.output(eval(parse(text=expr))), collapse="\n")},
       name="commander", inputSchema=list(x="character"),
       outputSchema=list(ans="character"))
cat(consumeLists(ep, list=(expr="getwd()"))$ans)
cat(consumeLists(ep, list=(expr=".packages(all=TRUE)"))$ans)

# The following example illustrates scoping rules. Note that the function
# refers to the variable y defined outside the function body. That value
# will be exported with the service.
y <- pi
ep <- publishWebService(ws, fun=function(x) x + y, name="lexi",
        inputSchema=list(x="numeric"), outputSchema=list(ans="numeric"))
cat(consumeLists(ep, list(x=2))$ans)
}
