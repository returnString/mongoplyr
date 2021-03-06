% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mongo.R
\docType{methods}
\name{mlimit}
\alias{mlimit}
\alias{mlimit,MongoPipeline,numeric-method}
\title{Add a $limit stage to a MongoPipeline.}
\usage{
mlimit(p, limit)

\S4method{mlimit}{MongoPipeline,numeric}(p, limit)
}
\arguments{
\item{p}{A \linkS4class{MongoPipeline} instance.}

\item{limit}{The number of documents to include in the result.}
}
\value{
A copy of the previous pipeline, with the new stage added.
}
\description{
Add a $limit stage to a MongoPipeline.
}
