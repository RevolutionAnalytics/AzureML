#' @export
print.Workspace =  function(x, ...)
{
  cat("AzureML Workspace\n")
  cat("Workspace ID: ",x$id,"\n")
}

#' @export
print.Experiments = function(x, ...)
{
  dots = character()
  if(nrow(x) > 0) dots = "..."
  d = data.frame(Description=substr(x[, "Description"], 1, 48),
                 CreationTime=x[, "CreationTime"],
                 `...`=dots)
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
  d = data.frame(Size=x[, "Size"], Name=substr(x[, "Name"], 1, 50), `...`=dots)
  print(d)
  cat("----------------------------------------------\n")
  cat("AzureML datasets data.frame variables include:\n")
  cat(paste(capture.output(names(x)),collapse="\n"),"\n")
  d
}
