AzureML.config.default <- "~/.azureml/settings.json" 

.onAttach <- function(libname, pkgname){
  options(AzureML.config = AzureML.config.default)
}
