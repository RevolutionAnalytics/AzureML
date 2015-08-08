\name{update.Dataset}
\alias{update.Dataset}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Update and existing remote data set}
\usage{
\method{update}{Dataser}(x, data, data.type = NULL, name = NULL, description = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A remote dataset object as returned by \code{create} or extracted from a workspace experiments field with the bracket operator/
}
  \item{data}{
A data frame or serialized data}
  \item{data.type}{
The format of \code{data} or to be used to serialize it.
}
  \item{name}{
A unique name for the dataset}
  \item{description}{
An extended description for the dataset}
}
\details{
Arguments set to NULL are construed as "leave unchanged".}
\examples{
\dontrun{
ws = workspace(workspace, authorization.token)
update(ws$user.datasets[[1]], iris3)
}}
