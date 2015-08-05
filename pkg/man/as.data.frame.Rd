\docType{methods}
\name{as.data.frame, Dataset-method}
\alias{as.data.frame, Dataset-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert Dataset to data.frame
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{as.data.frame}{Dataset}(x, row.names = NULL, optional = FALSE, ...)
}

\arguments{
  \item{x}{
  Dataset to convert
}
  \item{row.names}{
Unused for now, needed for consistency with generic
}
  \item{optional}{
Unused for now, needed for consistency with generic
}
  \item{\dots}{
Unused for now, needed for consistency with generic
}
}
\details{
}
\value{
A data frame with the same contents as the dataset}
\examples{
\dontrun{
ws = workspace(id, authorization.token)
as.data.frame(ws$datasets[[1]])
}}