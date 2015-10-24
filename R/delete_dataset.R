#' Delete datasets from an AzureML workspace.
#'
#' @param name Either one or more \code{Dataset} objects (rows from the workspace
#'        \code{datasets} data.frame), or a character vector of dataset names to delete.
#' @return A data.frame with columns Name, Deleted, status_code indicating the HTTP
#' status code and success/failure result of the delete operation for each dataset.
#' @family dataset functions
#' @export
delete.datasets = function(ws, name)
{
  # https://studioapi.azureml.net/api/workspaces/<workspaceId>/datasources/family/<familyId> HTTP/1.1
  datasets = name
  if(!inherits(datasets, "Datasets"))
  {
    datasets = datasets(ws)
    datasets = datasets[datasets$Name %in% name, ]
  }
  h = new_handle()
  handle_setheaders(h, .list = ws$.headers)
  handle_setopt(h, customrequest = "DELETE")
  status_code = vapply(datasets$FamilyId, function(familyId)
    {
      uri = sprintf("https://studioapi.azureml.net/api/workspaces/%s/datasources/family/%s",
                    curl_escape(ws$id), curl_escape(familyId))
      curl_fetch_memory(uri, handle=h)$status_code
    }, 1, USE.NAMES=FALSE)
  ans = data.frame(Name = datasets$Name, Deleted=status_code < 300, status_code=status_code)
  if(any(ans$Deleted)) refresh(ws, "datasets")
  ans
}
