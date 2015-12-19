# This function is used in unit testing to skip tests if the config file is missing
#
skip_if_missing_config <- function(f){
  if(!file.exists(f)) {
    msg <- paste("To run tests, add a file ~/.azureml/settings.json containing AzureML keys.",
                 "See ?workspace for help",
                 sep = "\n")
    message(msg)
    skip("settings.json file is missing")
  }
}
