\name{workspace}
\alias{workspace}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create an object representing an ML studio workspace
}
\description{
Create an object that mediates access to a ML studio workspace. This does not actually create an ML studio workspace, it has to exist already.
}
\usage{
workspace(id, authorization.token)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{id}{Workspace id from ML studio -> settings -> workspace id}
  \item{authorization.token}{Authorization token from ML studio -> settings -> authorization.tokens}
}
\details{
A workspace object is returned with the following user-accessible elements:
 \describe{
 \item{experiments, example.experiments, user.experiments}{Collection of experiments in workspace, optionally filtered for example or user experiments}
 \item{datasets, example.datasets, user.datasets}{Collection of datasets in workspace, optionally filtered for example or user datasets}}
}
\value{
A Workspace object
}
\examples{
\dontrun{
# provide id and auth token from ML studio -> settings
workspace(id, authorization.token)
}}
