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
x[[i]]
x[[name]]
x[[id]]
}
\arguments{
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
ws$experiments[[1]]
ws$datasets[[1]]
# name from ML studio ->datasets -> my datasets or samples -> name
ws$datasets[[name]]
# id from ML studio -> experiments -> my datasets or samples -> name
ws$experiments[[id]]
}
}

