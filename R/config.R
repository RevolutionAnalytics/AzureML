validate.AzureML.config <- function(config = getOption("AzureML.config"), stopOnError = FALSE){
  # Stop if the config file is missing
  if(!file.exists(config)) {
    msg <- sprintf("config file is missing: '%s'", config)
    if(stopOnError)
      stop(msg, call. = FALSE)
    else 
      return(simpleError(msg))
  }
  
  # Stop if the config is a directory, not a file
  if(file.info(config)$isdir){
    msg <- paste(
      "The config argument should point to a file.",
      sprintf(" You provided a directory (%s)", 
              normalizePath(config, winslash = "/", mustWork = FALSE)
      ), sep = "\n"
    )
    if(stopOnError)
      stop(msg, call. = FALSE)
    else 
      return(simpleError(msg))
  }
  TRUE
}

#' Reads settings from configuration file in JSON format.
#' 
#' @inheritParams workspace
#' 
#' @export
#' @seealso write.AzureML.config
#' @seealso workspace
read.AzureML.config <- function(config = getOption("AzureML.config")){
  z <- tryCatch(fromJSON(file(config)), 
                       error = function(e)e
  )
  # Error check the settings file for invalid JSON
  if(inherits(z, "error")) {
    msg <- sprintf("Your config file contains invalid json", config)
    msg <- paste(msg, z$message, sep = "\n\n")
    stop(msg, call. = FALSE)
  }
  z
}

#' Writes settings to configuration file.
#' 
#' @inheritParams workspace
#' @param file either a character string naming a file or a connection open for writing. "" indicates output to the console.
#' 
#' @rdname read.AzureML.config
#' 
#' @export
#' @seealso write.AzureML.config
#' @seealso workspace
write.AzureML.config <- function(id = NULL, auth = NULL, 
                           api_endpoint = NULL, 
                           management_endpoint = NULL, 
                           file = ""){
  # Construct list
  x <- list(
    id = id, 
    authorization_token = auth, 
    api_endpoint = api_endpoint, 
    management_endpoint = management_endpoint
  )
  # Remove null values
  conf <- list(
    workspace = x[!sapply(x, is.null)]
  )
  # Convert to JSON
  js <- jsonlite::toJSON(conf, pretty = TRUE)
  if(!missing(file) && !is.null(file)) {
    writeLines(js, con = file)
  } else {
    js
  }
}
