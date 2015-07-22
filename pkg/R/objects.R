Dataset =
  R6Class(
    "Dataset" )


SourceDataset =
  R6Class(
    "SourceDataset",
    inherit = Dataset,
    public =
      list(
        workspace = NA,
        metadata = NA,
        initialize =
          function(workspace, metadata) {
            self$workspace = workspace
            self$metadata = metadata},
        get =
          function(mode = c("open", "text", "binary")) {
            get.dataset.contents(self$contents.url, mode = mode)},
        as.data.frame =
          function() {
            deserialize.data.frame(self$get(), self$data.type.id)}))

read.SourceDataset =
  function(x, format)
    read.dataset.content(content.url(x), format)

as.dataframe.SourceDataset =
  function(x) {
    tc = textConnection(read.dataset.content(content.url(x), "raw"))
    deserialize.SourceDataset(textConnectionValue(tc))}

#update.SourceDataset.data.frame
#update.SourceDataset.raw
#upload.and.refresh
#

Datasets =
  R6Class(
    "Datasets",
    public =
      list(
        workspace = NA,
        example.filter = NA,
        initialize =
          function(workspace, example.filter = NA) {
            self$workspace = workspace
            self$example.filter = example.filter},
        get.item =
          function(index) {
            datasets =
              get.datasets(
                self$workspace$id,
                self$workspace$authorization.token)
            if(is.numeric(index))
              self$create.dataset(datasets[[index]])
            else
              keep(datasets, ~.["Name"] == index)[[1]]},
        get.datasets =
          function() {
            datasets =
              get.datasets(
                self$workspace$id,
                self$workspace$authorization.token)
            if(is.na(self$example.filter))
              datasets
            else
              keep(
                datasets,
                ~stri_startswith(
                  .$id, fixed = global.workspace.id) ==
                  self$example.filter)},
        create.dataset =
          function(metadata) {
            SourceDataset$new(self$workspace, metadata)}))

IntermediateDataset =
  R6Class(
    "IntermediateDataset",
    inherit = Dataset,
    public =
      list(
        workspace = NA,
        experiment = NA,
        node.id = NA,
        port.name = NA,
        data.type.id = NA,
        initialize =
          function(workspace, experiment, node.id, port.name, data.type.id = NA) {
            self$workspace = workspace
            self$experiment = experiment
            self$node.id = node.id
            self$port.name = port.name
            self$data.type.id = data.type.id},
        get =
          function(mode = c("open", "text", "binary")) {
            mode = match.arg(mode)
            get.intermediate.dataset.contents(
              workspace = self$workspace$id,
              experiment = self$experiment$id,
              node = self$node.id,
              port = self$port.name,
              mode = mode)},
        as.data.frame =
          function(x) {
            tc = textConnection(get.dataset.content(content.url(x), "raw"))
            deserialize.IntermediateDataset(textConnectionValue(tc))}))

Experiment =
  R6Class(
    "Experiment",
    public =
      list(
        workspace = NA,
        metadata = NA,
        initialize =
          function(workspace, metadata) {
            self$workspace = workspace
            self$metadata = metadata},
        experiment.id =
          function()
            self$metadata['ExperimentId'],
        is.example =
          function()
            stri_startswith(self$id, fixed = global.workspace.id),
        get.intermediate.dataset =
          function(node.id, port.name, data.type.id)
            IntermediateDataset(self$workspace, self, node.id, port.name, data.type.id)))

Experiments =
  R6Class(
    "Experiments",
    public =
      list(
        workspace = NA,
        example.filter = NA,
        initialize =
          function(workspace, example.filter = NA) {
            self$workspace = workspace
            self$example.filter = example.filter},
        get.item =
          function(index) {
          experiments = self$get.experiments()
            if(is.numeric(index)) {
              self$create.experiment(experiments[[index]])}
            else
              self$create.experiment(keep(experiments, ~.$id == index)[[1]])},
        get.experiments =
          function() {
            experiments = get.experiments(self$workspace$id)
            if(is.na(self$example.filter))
              experiments
            else {
              keep(
                experiments,
                ~stri_startswith(
                  .$id,
                  fixed = global.workspace.id) ==
                  self$example.filter)}},
        create.experiment =
          function(metadata)
            Experiment$new(self$workspace, metadata)))

global.workspace.id =  '506153734175476c4f62416c57734963'

Workspace =
  R6Class(
    "Workspace",
    public =
      list(
        id = NA,
        authorization.token = NA,
        datasets = NA,
        user.datasets = NA,
        example.datasets = NA,
        experiments = NA,
        user.experiments = NA,
        example.experiments = NA,
        initialize =
          function(id, authorization.token) {
            self$id = id
            self$authorization.token = authorization.token
            self$datasets = Datasets$new(self)
            self$user.datasets = Datasets$new(self, FALSE)
            self$example.datasets = Datasets$new(self, TRUE)
            self$experiments = Experiments$new(self)
            self$user.experiments = Experiments$new(self, FALSE)
            self$example.experiments = Experiments$new(self, TRUE)}))
