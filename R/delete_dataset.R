
#' Delete one or more datasets from an AzureML workspace.
#'
#' @export
delete.dataset = function(ws, name, ...)
{
  
  # https://studioapi.azureml.net/api/workspaces/<workspaceId>/datasources/family/<familyId> HTTP/1.1
  
  datasets <- datasets(ws)
  datasets <- datasets[datasets$Name %in% name, ]
  familyId <- datasets$FamilyId
  message(familyId)
  baseuri <- "https://studioapi.azureml.net/api"
  uri = sprintf("%s/workspaces/%s/datasources/family/%s",
                curl_escape(baseuri), curl_escape(ws$id), curl_escape(familyId)
  )
  h = new_handle()
  handle_setheaders(h, .list = ws$.headers)
  handle_setopt(h, customrequest = "DELETE")
  
  on.exit(close(r))
  r <- curl(uri, handle=h)
}