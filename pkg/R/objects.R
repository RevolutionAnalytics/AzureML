assert = function(x, f) {stopifnot(all(f(x))); x}

Dataset = R6Class("Dataset" )

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
            get.dataset.contents(self$contents.url(), mode = mode)},
        as.data.frame =
          function() {
            to.data.frame(self$get(mode = "text"), self$metadata$DataTypeId)},
        contents.url =
          function() {
            loc = self$download.location()
            paste0(loc$BaseUri, loc$Location, loc$AccessCredential)},
        download.location =
          function(){
            self$metadata$DownloadLocation}))

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
            self$create.dataset(
              if(is.numeric(index))
                datasets[[index]]
              else
                assert(keep(datasets, ~.["Name"] == index), function(x) length(x) > 0)[[1]])},
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
          function() {
            get.intermediate.dataset.contents(
              workspace = self$workspace$id,
              experiment = self$experiment$id(),
              node = self$node.id,
              port = self$port.name,
              authorization.token = self$workspace$authorization.token)},
        as.data.frame =
          function() {
            to.data.frame(self$get(), self$data.type.id)}))

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
        id =
          function()
            self$metadata['ExperimentId'],
        is.example =
          function()
            stri_startswith(self$id, fixed = global.workspace.id),
        get.intermediate.dataset =
          function(node.id, port.name, data.type.id)
            IntermediateDataset$new(self$workspace, self, node.id, port.name, data.type.id)))

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
              self$create.experiment(keep(experiments, ~.$ExperimentId == index)[[1]])},
        get.experiments =
          function() {
            experiments =
              get.experiments(
                self$workspace$id,
                self$workspace$authorization.token)
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


serialize.dataframe =
  function(df, type)
    stop()

to.data.frame =
  function(data, format) {
    textcon = textConnection(data, "r")
    read.csv.character =
      function(...)
        read.csv(file = textcon, ...)
    switch(
      format,
      ARFF = read.arff(textcon),
      PlainText = dataframe.to.txt(data),
      GenericCSV = read.csv.character(),
      GenericTSV = read.csv.character(sep = "\t"),
      GenericCSVNoHeader = read.csv.character(header = FALSE),
      GenericTSVNoHeader = read.csv.character(sep = "\t", header = FALSE),
      stop("Unknown Format"))}

