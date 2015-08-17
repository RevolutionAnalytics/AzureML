#' Create an object representing an ML studio workspace
#' 
#' Create an object that mediates access to a ML studio workspace. This does
#' not actually create an ML studio workspace, it has to exist already.
#' 
#' @param id Workspace id from ML studio -> settings -> workspace id
#' @param authorization.token Authorization token from ML studio -> settings -> authorization.tokens
#' 
#' @export
#' @seealso \code{\link{datasets}}, \code{\link{experiments}}
#' 
#' @return A Workspace object with the following user-accessible elements:
#' \describe{
#'   \item{experiments, example.experiments, user.experiments}{Collection of experiments in workspace, optionally filtered for example or user experiments}
#'   \item{datasets, example.datasets, user.datasets}{Collection of datasets in workspace, optionally filtered for example or user datasets}
#'}
#' @examples
#' 
#' \dontrun{
#' # provide id and auth token from ML studio -> settings
#' workspace(id, authorization.token)
#' }
workspace =
  function(id, authorization.token)
    Workspace$new(id, authorization.token)



#' Extract datasets from experiments
#' 
#' Extract datasets from experiments
#' 
#' @param workspace workspace object, as returned by \code{\link{workspace}}
#' 
#' @param x Experiment from which to extract a dataset
#' @param node Node id
#' @param port Port name
#' @param data.type Format of intermediate data set
#' 
#' @return A remote dataset
#' @export
#' 
#' @family dataset functions

datasets = function(workspace) private(workspace)$datasets




#' Extract experiments from Azure ML studio
#' 
#' @inheritParams datasets
#' 
#' @export
#' @family experiment functions
experiments = function(workspace) private(workspace)$experiments



#' Convert Dataset to data.frame
#' 
#' @param x Dataset to convert
#' @param row.names Unused for now, needed for consistency with generic
#' @param optional Unused for now, needed for consistency with generic
#' @param \dots Unused for now, needed for consistency with generic
#' 
#' @export
#' @family dataset functions
#' 
#' @return A data frame with the same contents as the dataset
#' 
#' @examples
#' 
#' \dontrun{
#' ws = workspace(id, authorization.token)
#' as.data.frame(ws$datasets[[1]])
#' }

as.data.frame.Dataset = function(x, row.names = NULL, optional = FALSE, ...) private(x)$as.data.frame()




#'  Access experiment or dataset by index or name/id
#'  
#'  Access individual experiments or datasets in a workspace by index or by name for datasets, id for experiments
#'  
#'  Both names and ids can be obtained with the function names applied to the experiments or datasets, which are elements of the a workspace object.

#' @param x The experiments or datasets slots of a Workspace object
#' @param i The index of the dataset or experiment
#' @param name The name of the dataset
#' @param id The id of the experiment

#' @export
#' @family dataset functions

#' @return A dataset or experiment object

#' @examples
#'   \dontrun{
#'     # provide id and auth token from ML studio -> settings
#'     ws = workspace(id, authorization.token)
#'     experiments(ws)[[1]]
#'     datasets(ws)[[1]]
#'     # name from ML studio ->datasets -> my datasets or samples -> name or
#'     # names(ws$datsets)
#'     datasets(ws)[[name]]
#'     # id from ML studio -> experiments -> my datasets or samples -> name
#'     experiments(ws)[[id]]
#'   }

`[[.Datasets` = function(x, i) private(x)$get.item(i)


#' Convert Datsets object to list
#' 
#' @export
#' @family dataset functions
#' 
#' @return A list of datasets
as.list.Datasets =
  function(x, ...) {
    y = private(x)$get.datasets()
    setNames(y, unlist(map(as.list(y), "Name")))}


#' Return the names of available experiments or datasets.
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' In the case of Experiments, the names are actually experiment ids.
#' 
#' @param x Collection of experiments or datasets to extract names from
#' 
#' @export
#' @family dataset functions
#' @return A character vector.
#' 
#' @examples
#' 
#' \dontrun{
#' ws = workspace(id, authorization.token)
#' names(experiments(ws))
#' names(datasets(ws))
#' }

names.Datasets = function(x) names(as.list(x))



#' @export
#' @family dataset functions
`[[.Experiments` = function(x, i) private(x)$get.item(i)



#' Extract datasets from experiments
#' 
#' Extract datasets from experiments
#' 
#' 
#' @param x Experiment from which to extract a dataset
#' @param node Node id
#' @param port Port name
#' @param data.type Format of intermediate data set
#' @return A remote dataset
#' 
#' @export dataset
#' @family experiment functions

dataset =
  function(x, node, port, data.type) {
    private(x)$get.intermediate.dataset(node, port, data.type)}


#' Convert Collection of Datasets and Experiments to lists
#' 
#' 
#' 
#' @param x The object to convert to a list
#' @param \dots Unused, just for compatibility with generic
#' 
#' @export
#' @return A Dataset or Experiment object
#' @family experiment functions

#' @examples
#' 
#' \dontrun{
#' ws = workspace(id, authorization.token)
#' as.list(experiments(ws))
#' as.list(datasets(ws))
#' }

as.list.Experiments =
  function(x, ...) {
    y = private(x)$get.experiments()
    setNames(y, unlist(map(as.list(y), "ExperimentId")))}

#' @export
#' @family experiment functions
names.Experiments = function(x) names(as.list(x))













#' Create new remote dataset
#' 
#' Create a new remote dataset from a dataframe or serialized data/
#' 
#' 
#' @param data Either a data frame or serialized data.
#' @param workspace The workspace where to create the remote dataset
#' @param data.type The format of \code{data} or to be used to serialize it.
#' @param name A unique name for the data set within a workspace
#' @param description An extended description for the dataset
#' @return A remote dataset object. Can be transformed into a data frame with
#' \code{as.data.frame} or updated with \code{update}
#' @examples
#' 
#' \dontrun{
#' ws = workspace(workspace, authorization.token)
#' create(iris3, ws, "GenericCSV", "iris3", "Iris three class data set")
#' }
#' @export create
create =
  function(data, workspace, data.type, name, description)
    workspace$datasets$add(data, data.type, name, description)





#' Update and existing remote data set
#' 
#' 
#' Arguments set to NULL are construed as "leave unchanged".
#' 
#' @param x A remote dataset object as returned by \code{create} or extracted
#' from a workspace experiments field with the bracket operator/
#' @param data A data frame or serialized data
#' @param data.type The format of \code{data} or to be used to serialize it.
#' @param name A unique name for the dataset
#' @param description An extended description for the dataset
#' @examples
#' 
#' \dontrun{
#' ws = workspace(workspace, authorization.token)
#' update(ws$user.datasets[[1]], iris3)
#' }
update.Dataset =
  function(
    x,
    data,
    data.type = NULL,
    name = NULL,
    description = NULL){
      private(x)$update(data, data.type, name, description)}

length.Datasets =
length.Experiments =
  function(x) length(as.list(x))
