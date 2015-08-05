\docType{methods}
\name{as.list, Experiments-method}
\alias{as.list, Datasets-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert Collection of Datasets and Experiments to lists
}
\description{
}
\usage{
\method{as.list}{Datasets}(x, ...)
\method{as.list}{Experiments}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The object to convert to a list
}
  \item{\dots}{
Unused, just for compatibility with generic}
}
\details{
}
\value{
A Dataset or Experiment object
}
\examples{
\dontrun{
ws = workspace(id, authorization.token)
as.list(ws$experiments)
as.list(ws$datasets[[1]])
}}
