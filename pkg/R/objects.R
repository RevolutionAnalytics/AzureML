assert = function(x, f) {stopifnot(all(f(x))); x}

Dataset = R6Class("Dataset" )

SourceDataset =
  R6Class(
    "SourceDataset",
    inherit = Dataset,
    public =
      list(
        initialize =
          function(workspace, metadata) {
            private$workspace = workspace
            private$metadata = metadata},
        as.data.frame =
          function() {
            to.data.frame(private$get(mode = "text"), private$metadata$DataTypeId)}),
    private =
      list(
        workspace = NA,
        metadata = NA,
        get =
          function(mode = c("open", "text", "binary")) {
            get.dataset.contents(private$contents.url(), mode = mode)},
        contents.url =
          function() {
            loc = private$download.location()
            paste0(loc$BaseUri, loc$Location, loc$AccessCredential)},
        download.location =
          function(){
            private$metadata$DownloadLocation}))

Datasets =
  R6Class(
    "Datasets",
    public =
      list(
        initialize =
          function(workspace, example.filter = NA) {
            private$workspace = workspace
            private$example.filter = example.filter},
        get.item =
          function(index) {
            datasets =
              get.datasets(
                private$workspace[["id"]],
                private$workspace[["authorization.token"]])
            private$create.dataset(
              if(is.numeric(index))
                datasets[[index]]
              else
                assert(keep(datasets, ~.["Name"] == index), function(x) length(x) > 0)[[1]])},
        get.datasets =
          function() {
            datasets =
              get.datasets(
                private$workspace[["id"]],
                private$workspace[["authorization.token"]])
            if(is.na(private$example.filter))
              datasets
            else
              keep(
                datasets,
                ~stri_startswith(
                  .$id, fixed = global.workspace.id) ==
                  private$example.filter)}),
    private =
      list(
        workspace = NA,
        example.filter = NA,
        create.dataset =
          function(metadata) {
            SourceDataset$new(private$workspace, metadata)}))

IntermediateDataset =
  R6Class(
    "IntermediateDataset",
    inherit = Dataset,
    public =
      list(
        initialize =
          function(workspace, experiment, node.id, port.name, data.type.id = NA) {
            private$workspace = workspace
            private$experiment = experiment
            private$node.id = node.id
            private$port.name = port.name
            private$data.type.id = data.type.id},
        as.data.frame =
          function() {
            to.data.frame(private$get(), private$data.type.id)}),
    private =
      list(
        workspace = NA,
        experiment = NA,
        node.id = NA,
        port.name = NA,
        data.type.id = NA,
        get =
          function() {
            get.intermediate.dataset.contents(
              workspace = private$workspace[["id"]],
              experiment = private$experiment$id(),
              node = private$node.id,
              port = private$port.name,
              authorization.token = private$workspace[["authorization.token"]])}))

Experiment =
  R6Class(
    "Experiment",
    public =
      list(
        initialize =
          function(workspace, metadata) {
            private$workspace = workspace
            private$metadata = metadata},
        get.intermediate.dataset =
          function(node.id, port.name, data.type.id)
            IntermediateDataset$new(private$workspace, self, node.id, port.name, data.type.id),
        id =
          function()
            private$metadata['ExperimentId']),
    private =
      list(
        workspace = NA,
        metadata = NA,
        is.example =
          function()
            stri_startswith(private$id, fixed = global.workspace.id)))

Experiments =
  R6Class(
    "Experiments",
    public =
      list(
        initialize =
          function(workspace, example.filter = NA) {
            private$workspace = workspace
            private$example.filter = example.filter},
        get.item =
          function(index) {
            experiments = self$get.experiments()
            if(is.numeric(index)) {
              private$create.experiment(experiments[[index]])}
            else
              private$create.experiment(keep(experiments, ~.$ExperimentId == index)[[1]])},
        get.experiments =
          function() {
            experiments =
              get.experiments(
                private$workspace[["id"]],
                private$workspace[["authorization.token"]])
            if(is.na(private$example.filter))
              experiments
            else {
              keep(
                experiments,
                ~stri_startswith(
                  .$id,
                  fixed = global.workspace.id) ==
                  private$example.filter)}}),
    private =
      list(
        workspace = NA,
        example.filter = NA,
        create.experiment =
          function(metadata)
            Experiment$new(private$workspace, metadata)))

global.workspace.id =  '506153734175476c4f62416c57734963'

Workspace =
  R6Class(
    "Workspace",
    public =
      list(
        initialize =
          function(id, authorization.token) {
            private$id = id
            private$authorization.token = authorization.token
            private$datasets = Datasets$new(self)
            private$user.datasets = Datasets$new(self, FALSE)
            private$example.datasets = Datasets$new(self, TRUE)
            private$experiments = Experiments$new(self)
            private$user.experiments = Experiments$new(self, FALSE)
            private$example.experiments = Experiments$new(self, TRUE)},
        get =
          function(slot)
            private[[slot]]),
    private =
      list(
        id = NA,
        authorization.token = NA,
        datasets = NA,
        user.datasets = NA,
        example.datasets = NA,
        experiments = NA,
        user.experiments = NA,
        example.experiments = NA))


serialize.dataframe =
  function(df, format) {
    capture.write =
      function(...)
        capture.output(write.csv(df, file = stdout()))
    switch(
      format,
      ARFF = capture.output(write.arff(df, "")),
      PlainText = dataframe.to.txt(df),
      GenericCSV = capture.write(),
      GenericTSV = capture.write(sep = "\t"),
      GenericCSVNoHeader = capture.write(header = FALSE),
      GenericTSVNoHeader = capture.write(sep = "\t", header = FALSE))}

to.data.frame =
  function(data, format) {
    textcon = textConnection(data, "r")
    read.csv.character =
      function(...)
        read.csv(file = textcon, ...)
    switch(
      format,
      ARFF = read.arff(textcon),
      PlainText = txt.to.data.frame(data),
      GenericCSV = read.csv.character(),
      GenericTSV = read.csv.character(sep = "\t"),
      GenericCSVNoHeader = read.csv.character(header = FALSE),
      GenericTSVNoHeader = read.csv.character(sep = "\t", header = FALSE),
      stop("Unknown Format"))}

