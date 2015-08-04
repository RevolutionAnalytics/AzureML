

as.data.frame.Dataset = function(x) x$as.data.frame()
`[[.Datasets` = function(xx, i) xx$get.item(i)
as.list.Datasets = function(xx) xx$get.datasets()
`[[.Experiments` = function(xx, i) xx$get.item(i)
as.list.Experiments = function(xx) xx$get.experiments()