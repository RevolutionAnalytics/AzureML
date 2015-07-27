
pasteslash = partial(paste, sep = "/", collapse = "/")

make.web.call.headers =
  partial(
    make.web.call,
    .url = "https://studio.azureml.net/api",
    .headers =
      list(
        `User-Agent` = a(default = "R-azureml", export = NULL),
        `Content-Type` = a(default  = 'application/json;charset=UTF8', export = NULL),
        `x-ms-metaanalytics-authorizationtoken` = a(mandatory = TRUE, export = "authorization.token"),
        `x-ms-client-session-id` = a(default  = "DefaultSession", export = NULL)))

#neeeds custom content type for uploads

# def get_experiments(self, workspace_id):
#   """Runs HTTP GET request to retrieve the list of experiments."""
# api_path = self.EXPERIMENTS_URI_FMT.format(workspace_id)
# return self._send_get_req(api_path)
#

get.experiments =
  make.web.call.headers(
    .method = "get",
    .parameters = list(workspace = a()),
    .param.encoding = interpylate("workspaces/{workspace}/experiments"))

# DATASOURCES_URI_FMT = SERVICE_ROOT + 'workspaces/{0}/datasources'
# def get_datasets(self, workspace_id):
#   """Runs HTTP GET request to retrieve the list of datasets."""
# api_path = self.DATASOURCES_URI_FMT.format(workspace_id)
# return self._send_get_req(api_path)

get.datasets =
  make.web.call.headers(
    .method = "get",
    .parameters = list(workspace = a()),
    .param.encoding = interpylate("workspaces/{workspace}/datasources"))

#
# DATASOURCE_URI_FMT = SERVICE_ROOT + 'workspaces/{0}/datasources/{1}'
# def get_dataset(self, workspace_id, dataset_id):
#   """Runs HTTP GET request to retrieve a single dataset."""
# api_path = self.DATASOURCE_URI_FMT.format(workspace_id, dataset_id)
# return self._send_get_req(api_path)
#
#

get.dataset =
  make.web.call.headers(
    .method = "get",
    .parameters = list(workspace = a(), dataset = a()),
    .param.encoding = interpylate("workspaces/{workspace}/datasources/{dataset}"))


# INTERMEDIATE_DATASET_URI_FMT = SERVICE_ROOT + 'workspaces/{0}/experiments/{1}/outputdata/{2}/{3}'
# def _get_intermediate_dataset_contents(self, workspace_id, experiment_id,
#                                        node_id, port_name, stream):
#   api_path = self.INTERMEDIATE_DATASET_URI_FMT.format(
#     workspace_id, experiment_id, node_id, port_name)
# response = requests.get(
#   url=urljoin(self._service_endpoint, api_path),
#   headers=self._get_headers(),
#   stream=stream,
# )
# return response
#

get.intermediate.dataset.contents =
  make.web.call.headers(
    .method = "get",
    .parameters =
      list(
        workspace = a(mandatory = TRUE),
        experiment = a(mandatory = TRUE),
        node = a(mandatory = TRUE),
        port = a(mandatory = TRUE)),
    .param.encoding = interpylate("workspaces/{workspace}/experiments/{experiment}/outputdata/{node}/{port}"),
    .response.encoding = "text")


get.dataset.contents =
  function(url, mode) {
    web.call =
      make.web.call(
        .method = "get",
        .url = url,
        .response.encoding = mode)
    web.call()}

#
# def upload_dataset(self, workspace_id, name, description, data_type_id,
#                    raw_data, family_id):
#   # uploading data is a two step process. First we upload the raw data
#   api_path = self.UPLOAD_URI_FMI.format(workspace_id, data_type_id)
# upload_result = self._send_post_req(api_path, raw_data)
#

# api_path = self.DATASOURCES_URI_FMT.format(workspace_id)
# datasource_id = self._send_post_req(
#   api_path, json.dumps(metadata), self.CONTENT_TYPE_HEADER_VALUE_JSON)
# return datasource_id
#

upload.dataset =
  function(workspace, name, description, data.type.id, raw.data, family.id, authorization.token) {
    upload.result =
      make.web.call.headers(
        .method = "post",
        .parameters =
          list(
            workspace = a(),
            data.type.id = a()),
        .param.encoding = interpylate("resourceuploads/workspaces/{workspace}/?userStorage=true&dataTypeId={data.type.id}"),
        .body = list(raw.data = a()),
        .body.encoding = "text")(workspace, data.type.id, authorization.token, raw.data)
    upload_id = upload_result["Id"]
    metadata =
      list(
      DataSource =
        list(
          Name =  name,
          DataTypeId = data.type.id,
          Description = description,
          FamilyId = family.id,
          Owner =  "R SDK",
          SourceOrigin = "FromResourceUpload"),
      UploadId =  upload.id,
      UploadedFromFileName = "",
      ClientPoll =  TRUE)
    make.web.call.headers(
      .method = "post",
      .parameters = list(
        workspace = a()),
      .param.encoding = interpylate('workspaces/{workspace}/datasources'),
      .body = list(metadata = a()),
      .body.encoding = "json")(workspace, metadata)}

    #
    # api_path = self.DATASOURCES_URI_FMT.format(workspace_id)
    # datasource_id = self._send_post_req(
    #   api_path, json.dumps(metadata), self.CONTENT_TYPE_HEADER_VALUE_JSON)
    # return datasource_id}
