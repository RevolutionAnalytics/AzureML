# The MIT License (MIT)
# 
# Copyright (c) 2015 Microsoft Corporation
#   
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#   
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.


assert = function(x, f) {stopifnot(all(f(x))); x}

private = function(x) x$.__enclos_env__$private

Dataset = R6Class("Dataset")

SourceDataset =
  R6Class(
    "SourceDataset",
    inherit = Dataset,
    public =
      list(
        initialize =
          function(workspace, metadata) {
            private$workspace = workspace
            private$metadata = metadata}),
    private =
      list(
        workspace = NA,
        metadata = NA,
        as.data.frame =
          function() {
            to.data.frame(private$get(mode = "text"), private$metadata$DataTypeId)},
        update =
          function(
            data,
            data.type = private$metadata$DataTypeId,
            name = private$metadata$Name,
            description = private$metadata$Description){
            if(is.null(data.type)) data.type = private$metadata$DataTypeId
            if(is.null(name)) name = private$metadata$Name
            if(is.null(description)) description = private$metadata$Description
            if(is.example(private$metadata$Id))
              stop("Can't update Azure ML example")
            raw.data = {
              if(is.data.frame(data))
                serialize.dataframe(data, data.type)
              else
                data}
            private$upload.and.refresh(raw.data, data.type, name, description)},
        get =
          function(mode = c("open", "text", "binary")) {
            get.dataset.contents(private$contents.url(), mode = mode)},
        contents.url =
          function() {
            loc = private$download.location()
            paste0(loc$BaseUri, loc$Location, loc$AccessCredential)},
        download.location =
          function(){
            private$metadata$DownloadLocation},
        upload.and.refresh =
          function(raw.data, data.type, name, description) {
            dataset.id =
              upload.dataset(
                private(private$workspace)$id,
                name,
                description,
                data.type,
                raw.data,
                private$metadata$FamilyId,
                private(private$workspace)$authorization.token)
            private$metadata =
              get.dataset(
                private(private$workspace)$id,
                dataset.id,
                private(private$workspace)$authorization.token)}))


is.example =
  function(id)
    stri_startswith(id, fixed = global.workspace.id)

Datasets =
  R6Class(
    "Datasets",
    public =
      list(
        initialize =
          function(workspace, example.filter = NA) {
            private$workspace = workspace
            private$example.filter = example.filter}),
    private =
      list(
        workspace = NA,
        example.filter = NA,
        get.item =
          function(index) {
            datasets =
              get.datasets(
                private(private$workspace)$id,
                private(private$workspace)$authorization.token)
            private$create.dataset(
              if(is.numeric(index))
                datasets[[index]]
              else
                assert(keep(datasets, ~.["Name"] == index), function(x) length(x) > 0)[[1]])},
        get.datasets =
          function() {
            datasets =
              get.datasets(
                private(private$workspace)$id,
                private(private$workspace)$authorization.token)
            if(is.na(private$example.filter))
              datasets
            else
              keep(
                datasets,
                ~is.example(.$Id) ==
                  private$example.filter)},
        add =
          function(data, data.type, name, description) {
            if(is.data.frame(data))
              data = serialize.dataframe(data, data.type)
            private$upload(data, data.type, name, description)},
        create.dataset =
          function(metadata) {
            SourceDataset$new(private$workspace, metadata)},
        upload =
          function(raw.data, data.type, name, description) {
            dataset.id =
              upload.dataset(
                private(private$workspace)$id,
                name,
                description,
                data.type,
                raw.data,
                family.id = UUIDgenerate(),
                authorization.token)
            metadata = get.dataset(private(private$workspace)$id, dataset.id, private(private$workspace)$authorization.token)
            private$create.dataset(metadata)}))

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
            private$data.type.id = data.type.id}),
    private =
      list(
        workspace = NA,
        experiment = NA,
        node.id = NA,
        port.name = NA,
        data.type.id = NA,
        as.data.frame =
          function() {
            to.data.frame(private$get(), private$data.type.id)},
        get =
          function() {
            get.intermediate.dataset.contents(
              workspace = private(private$workspace)$id,
              experiment = private(private$experiment)$id(),
              node = private$node.id,
              port = private$port.name,
              authorization.token = private(private$workspace)$authorization.token)}))

Experiment =
  R6Class(
    "Experiment",
    public =
      list(
        initialize =
          function(workspace, metadata) {
            private$workspace = workspace
            private$metadata = metadata}),
    private =
      list(
        workspace = NA,
        metadata = NA,
        is.example =
          function()
            is.example(private$id),
        get.intermediate.dataset =
          function(node.id, port.name, data.type.id)
            IntermediateDataset$new(private$workspace, self, node.id, port.name, data.type.id),
        id =
          function()
            private$metadata['ExperimentId']))

Experiments =
  R6Class(
    "Experiments",
    public =
      list(
        initialize =
          function(workspace, example.filter = NA) {
            private$workspace = workspace
            private$example.filter = example.filter}),
    private =
      list(
        workspace = NA,
        example.filter = NA,
        get.item =
          function(index) {
            experiments = private$get.experiments()
            if(is.numeric(index)) {
              private$create.experiment(experiments[[index]])}
            else
              private$create.experiment(keep(experiments, ~.$ExperimentId == index)[[1]])},
        get.experiments =
          function() {
            experiments =
              get.experiments(
                private(private$workspace)$id,
                private(private$workspace)$authorization.token)
            if(is.na(private$example.filter))
              experiments
            else {
              keep(
                experiments,
                ~is.example(.$Id) == private$example.filter)}},
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
            private$example.experiments = Experiments$new(self, TRUE)}),
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

dataframe.to.txt =
  function(df) {
    paste(df[, 1,drop = TRUE], collapse = "\n")}

txt.to.data.frame =
  function(data) {
    data.frame(txt = strsplit(data, "\n"))[[1]]}
