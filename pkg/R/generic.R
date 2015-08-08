

as.data.frame.Dataset = function(x, row.names = NULL, optional = FALSE, ...) x$as.data.frame()

`[[.Datasets` = function(x, i) x$get.item(i)

as.list.Datasets =
  function(x, ...) {
    y = x$get.datasets()
    setNames(y, unlist(map(as.list(y), "Name")))}

names.Datasets = function(x) names(as.list(x))

`[[.Experiments` = function(x, i) x$get.item(i)

as.list.Experiments =
  function(x, ...) {
    y = x$get.experiments()
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
      x$update(data, data.type, name, description)}

length.Datasets =
length.Exoeriments =
  function(x) length(as.list(x))
