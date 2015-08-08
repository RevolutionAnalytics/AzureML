\name{create}
\alias{create}%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create new remote dataset}
\description{
Create a new remote dataset from a dataframe or serialized data/
}
\usage{
create(data, workspace, data.type, name, description)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  Either a data frame or serialized data.
}
  \item{workspace}{
The workspace where to create the remote dataset
}
  \item{data.type}{
The format of \code{data} or to be used to serialize it.
}
  \item{name}{
A unique name for the data set within a workspace}
  \item{description}{
An extended description for the dataset}
}
\value{
A remote dataset object. Can be transformed into a data frame with \code{as.data.frame} or updated with \code{update}}
\examples{
\dontrun{
ws = workspace(workspace, authorization.token)
create(iris3, ws, "GenericCSV", "iris3", "Iris three class data set")
}}
