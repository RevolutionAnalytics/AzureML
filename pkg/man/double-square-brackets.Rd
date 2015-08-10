\docType{methods}
\name{[[, Experiments-method}
\alias{[[, Experiments-method}
\alias{[[, Datasets-method}


\title{
Access experiment or dataset by index or name/id}
\description{
Access individual experiments or datasets in a workspace by index or by name for datasets, id for experiments
}
\usage{
\method{[[}{Experiments}(x, i)
\method{[[}{Datasets}(x, i)
\method{[[}{Datasets}(x, name)
\method{[[}{Experiments}(x, id)
\method{[[}{Experiment}(x, node, port, data.type)
}
\arguments{
\item{x}{The experiments or datasets slots of a Workspace object}
\item{i}{The index of the dataset or experiment}
\item{name}{The name of the dataset}
\item{id}{The id of the experiment}
}
\details{
Both names and ids can be obtained with the function names applied to the experiments or datasets, which are elements of the a workspace object.
}
\value{
A dataset or experiment object
}
\examples{
\dontrun{
# provide id and auth token from ML studio -> settings
ws = workspace(id, authorization.token)
experiments(ws)[[1]]
datasets(ws)[[1]]
# name from ML studio ->datasets -> my datasets or samples -> name or
# names(ws$datsets)
datasets(ws)[[name]]
# id from ML studio -> experiments -> my datasets or samples -> name
experiments(ws)[[id]]
}
}

