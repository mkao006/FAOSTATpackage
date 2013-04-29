\name{dataDensity}
\alias{dataDensity}
\title{A plot to show the availability of the data}
\usage{
  dataDensity(data, order = TRUE, threshold = 0.5)
}
\arguments{
  \item{data}{The data frame to examine the sparsity}

  \item{order}{Whether the plot should reorder the data by
  the sparsity}

  \item{threshold}{The level where the horizontal line to
  be added.}
}
\description{
  This function is based on ggmissing of the ggplot2
  package with minor modification.
}

