readConfig <- function(config = getOption("AzureML.config")){
  fromJSON(config)
}

makeConfig <- function(id = NULL, authorization_token = NULL, 
                       api_endpoint = NULL, management_endpoint = NULL, file){
  x <- list(
    id = id, 
    authorization_token = authorization_token, 
    api_endpoint = api_endpoint, 
    management_endpoint = management_endpoint
  )
  conf <- list(
    workspace = x[!sapply(x, is.null)]
  )
  js <- jsonlite::toJSON(conf, pretty = TRUE)
  if(!missing(file) && !is.null(file)) {
    writeLines(js, con = file)
  } else {
    js
  }
}
