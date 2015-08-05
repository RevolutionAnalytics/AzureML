\name{names}
\alias{names}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Return the names of available experiments or datasets.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{names}{Experiments}(x)
\method{names}{Datasets}(x)
}
\arguments{
\item{x}{Collection of experiments or datasets to extract names from}
}
\details{
In the case of Experiments, the names are actually experiment ids.
}
\value{
A character vector.
}
\examples{
\dontrun{
ws = workspace(id, authorization.token)
names(ws$experiments)
names(ws$datasets)
}}
