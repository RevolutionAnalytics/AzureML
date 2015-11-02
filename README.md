# azureml
Interface to Azure ML

# Installation instructions

The `azureml` package is not yet available on CRAN.

To install the package and all its dependencies, try:

```r
# Install devtools
if(!require("devtools")) install.packages("devtools")

# Install webapi dependencies ---------------------------------------------

if(!require("jsonlite")) install.packages("jsonlite")
if(!require("curl")) install.packages("curl")
devtools::install_github("RevolutionAnalytics/azureml")
```


# Working with AzureML workspaces, datasets and experiments

# Working with the AzureML services: discover, publish and consume a service

This package provides an interface to publish web services on Microsoft Azure Machine Learning (Azure ML) from your local R environment. There are three main functionalities:
- Publish: define a custom function or train a model and publish it to Azure
- Discover: browse the web services in your workspace
- Consume: use available web service from R in a variety of convenient formats

This is a technology preview. The APIs used by the package are still subject to change. Please send any bugs or comments you have to the maintainers listed.

## Installing the package

Currently the package lives only on this GitHub repo. You can install the package and the dependencies via the following lines in R:

```
install.packages(c("RCurl", "RJSONIO", "uuid", "jsonlite", "codetools", "base64enc", "httr", "data.table", "df2json", "rjson", "devtools"), repos = "http://cran.r-project.org")
devtools::install_github("Azure-MachineLearning-ClientLibrary-R", "Azure", subdir="AzureML")
```

Also, you will need to install [R tools](https://cran.r-project.org/bin/windows/Rtools/) and 
make sure that a zipper is included in your PATH variable (instructions [here](http://stackoverflow.com/questions/29129681/create-zip-file-error-running-command-had-status-127))



## Using the package

To get started, please go [here](https://studio.azureml.net) and create a free account (not guest) or use your existing Azure ML account. 

After logging in, under the "Settings" tab, copy and paste your Workspace ID from the "Name" sub-tab into your R console. From the "Authorization Tokens" sub-tab, copy your Primary Authorization Token into your R console. You will need this information to access all package functionality.


## Publishing a web service

The primary functionality implemented by this package is the capability to publish a function or a model from R to Azure ML as a web service. Creating a predictive web service is now a simple one line function call.

```
newService<-publishWebService(functionName, serviceName, inputSchema, outputSchema, wkID, authToken)
```

The publish function takes in the name of the function to be published as a string, the name to be displayed on Azure ML, your Azure ML Workspace ID, and your Azure ML Workspace Authorization token. The function also requires the input and output schemas of the function to be published, which is a list of the format

```
list("arg1"=<type>, "arg2"=<type>, ...)
```

The publish function can take in a function that uses arbitrary packages. However, the function can only take in as input primitive data types, meaning that functions that take in dataframes, lists, or other complex objects are currently not supported. 
The R datatypes supported are as follows:
- float, double
- date-time
- string
- time-span
- uint16, int16, uint32, int32, uint64, int64
- int
- bool

If using a factor variable, it is recommended you use strings instead, e.g. "male" and "female".
We are currently working to extend functionality to be able to handle complex data types, as well as infer the signature of user functions, so users won't need to manually enter the schemas.

The publish function will return a lists of lists. The first list contains the details of the web service. The second list contains a list of the endpoints of the web service, including the webservice URL, API key, and help page URL. Using hte URL and API key, you can call the web service from web and mobile clients to make real time predictions:

```
endpoints <- newService[[2]]
webServiceURL <- endpoints[[1]]$ApiLocation
webServiceKey <- endpoints[[1]]$PrimaryKey
webPageHelpURL <- endpoints[[1]]$HelpLocation
```
For an example, please refer to the end of this README or the vignette included in the package.

You are also able to update your function with one line. Note that this also requires passing the input and output schemas of the function.

```
updateWebService(functionName, webServiceID, inputSchema, outputScema, wkID, authToken)
```

The return value is the same as that of publishWebService().


## Discovering web services

This package allows you to start with a workspace ID and discover all web service available, or start with a web service ID and discover all service endpoints and any information needed to access the endpoint. Service can then be consumed directly in R or used on another platform, e.g. Visual Studio or Excel.

```
getWebServices(workspaceID, authToken)
getWSDetails(wkID, authToken, webserviceID)
getEndpoints(wID, authToken, wsID)
getEPDetails(wkID, authToken, wsID, endpointName)
```


## Consuming a web service

The package includes a number of convenience functions to consume a web service via a number of different input formats.

```
consumeFile(api_key, requestURL, infileName, globalParam = "", outfileName = "results.txt", batchSize = 250, retryDelay = 0.3)
consumeLists(api_key, requestURL, columnNames, ..., globalParam="", retryDelay = 0.3)
consumeDataframe(api_key, requestURL, valuesDF, globalParam="", batchSize = 250, retryDelay = 0.3)
```


## Example

The below example demonstrates how to use the various functions included in this package. The script uses the datasets contained in /demo

```
# End to end demo using Titanic survival classifier
# Store your workspace ID and authorization token
wsID <- "abcdefghijklmnop"
wsAuth <- "abcdefghijklmnop"

# Import data
test <- read.csv(file="test.csv")
train <- read.csv(file="train.csv")

# Data wrangling
survived <- train$Survived
passengerId <- test$PassengerId
train = train[,-2]
end_trn = nrow(train)
train <- rbind(train, test)
train$Age[is.na(train$Age)] <- 30
end = nrow(train)
train = train[,c(-1,-3,-8,-10,-11)]

# Train a gbm model
library(gbm)
set.seed(123)
pr=0
tr=0
n.models = 5
ntrees=2000
for(i in 1:n.models){
  GBM.model = gbm.fit(
    x=train[1:end_trn,], y = survived,
    distribution= "gaussian",
    n.trees = ntrees,
    shrinkage = 0.01,
    interaction.depth = 25,
    n.minobsinnode = 5,
    verbose = TRUE)
}

# define a function to make predictions using the trained model
predictTitanic <- function (Pclass, Sex, Age, SibSp, Parch, Fare) {
  return(round(predict.gbm(object=GBM.model, newdata=data.frame("Pclass"=Pclass, "Sex"=Sex, "Age"=Age, "SibSp"=SibSp, "Parch"=Parch, "Fare"=Fare), 2000)))
}

# Sample local call
predictTitanic(1, "male", 20, 2, 0, 8.50)

# Publish the function
TitanicService <- publishWebService("predictTitanic", "TitanicDemoR", list("Pclass"="string", "Sex"="string", "Age"="int", "SibSp"="int", "Parch"="int", "Fare"="float"), list("survProb"="float"), wsID, wsAuth)

# Discover the endpoints
endpoints <- getEndpoints(wsID, wsAuth, TitanicService[[1]]["Id"], internalURL)
# Alternatively,
endpoints <- TitanicService[[2]]


# Consume the new web service
# First, consume with inputs as a list
# Notice that we can use access the API key and location from the results of publishWebService(). If need additional help, we can also access the help page URL via endpoints[[1]]$HelpLocation
response <- consumeDataTable(endpoints[[1]]$PrimaryKey, endpoints[[1]]$ApiLocation, list("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare"), list(1, "male", 20, 2, 0, 8.50), list(1, "female", 20, 1, 0, 8.50))
# Next, consume with inputs as dataframe
demoDF <- data.frame("Pclass"=c(1,2,1), "Sex"=c("male","female","male"), "Age"=c(8,20, 30), "Parch"=c(1,1,1), "SibSp"=c(1,3,1), "Fare"=c(10,7.5, 9))
responseDF <- consumeDataframe(TitanicService[[2]][[1]]$PrimaryKey, TitanicService[[2]][[1]]$ApiLocation, demoDF)
```

Further examples can be found in the /examples folder of the repository. Each example includes a script and an accompanying dataset. To run the script, load the package and store your Workspace ID and Authorization token.
Then run the script in order line by line.