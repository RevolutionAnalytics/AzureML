

as.data.frame.Dataset = function(x, row.names, optional, ...) x$as.data.frame()

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

`[[.Workspace` = function(x, slot) x$get(slot)