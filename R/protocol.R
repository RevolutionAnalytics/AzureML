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
  function(
    workspace,
    name,
    description,
    data.type.id,
    raw.data,
    family.id,
    authorization.token) {
    upload.result =
      make.web.call.headers(
        .method = "post",
        .parameters =
          list(
            workspace = a(),
            data.type.id = a()),
        .param.encoding =
          interpylate(
            "resourceuploads/workspaces/{workspace}/?userStorage=true&dataTypeId={data.type.id}"),
        .body = list(raw.data = a()),
        .body.encoding = function(x) unname(unlist(x)))(
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
      .body.encoding = function(x) jsonlite::toJSON(x$metadata, auto_unbox = TRUE))(
        workspace = workspace,
        metadata = metadata,
        authorization.token = authorization.token)}
