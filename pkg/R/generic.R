
workspace =
  function(id, authorization.token)
    Workspace$new(id, authorization.token)

datasets = function(workspace) private(workspace)$datasets
experiments = function(workspace) private(workspace)$experiments

as.data.frame.Dataset = function(x, row.names = NULL, optional = FALSE, ...) private(x)$as.data.frame()

`[[.Datasets` = function(x, i) private(x)$get.item(i)

as.list.Datasets =
  function(x, ...) {
    y = private(x)$get.datasets()
    setNames(y, unlist(map(as.list(y), "Name")))}

names.Datasets = function(x) names(as.list(x))

`[[.Experiments` = function(x, i) private(x)$get.item(i)

dataset =
  function(x, node, port, data.type) {
    private(x)$get.intermediate.dataset(node, port, data.type)}

as.list.Experiments =
  function(x, ...) {
    y = private(x)$get.experiments()
    setNames(y, unlist(map(as.list(y), "ExperimentId")))}

names.Experiments = function(x) names(as.list(x))

create =
  function(data, workspace, data.type, name, description)
    workspace$datasets$add(data, data.type, name, description)

update.Dataset =
  function(
    x,
    data,
    data.type = NULL,
    name = NULL,
    description = NULL){
      private(x)$update(data, data.type, name, description)}

length.Datasets =
length.Exoeriments =
  function(x) length(as.list(x))
