

as.data.frame.Dataset = function(x) x$as.data.frame()
`[[.Datasets` = function(xx, i) xx$get.item(i)

as.list.Datasets =
  function(xx) {
    yy = xx$get.datasets()
    setNames(yy, unlist(map(as.list(yy), "Name")))}

names.Datasets = function(xx) names(as.list(xx))

`[[.Experiments` = function(xx, i) xx$get.item(i)

as.list.Experiments =
  function(xx) {
    yy = xx$get.experiments()
    setNames(yy, unlist(map(as.list(yy), "ExperimentId")))}

names.Experiments = function(xx) names(as.list(xx))