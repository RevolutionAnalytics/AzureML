
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

get.experiments =
  make.web.call.headers(
    .method = "get",
    .parameters = list(workspace = a()),
    .param.encoding = interpylate("workspaces/{workspace}/experiments"))

get.datasets =
  make.web.call.headers(
    .method = "get",
    .parameters = list(workspace = a()),
    .param.encoding = interpylate("workspaces/{workspace}/datasources"))

get.dataset =
  make.web.call.headers(
    .method = "get",
    .parameters = list(workspace = a(), dataset = a()),
    .param.encoding = interpylate("workspaces/{workspace}/datasources/{dataset}"))

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
        .param.encoding = "none",
        .url = url,
        .response.encoding = mode)
    web.call()}

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
        .body.encoding = "multipart")(
          workspace = workspace,
          data.type.id = data.type.id,
          authorization.token = authorization.token,
          raw.data = raw.data)
    upload.id = upload.result[["Id"]]
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
      .body = list(metadata = a(conversion = identity)),
      .body.encoding = "json")(
        workspace = workspace,
        metadata = metadata,
        authorization.token = authorization.token)}
