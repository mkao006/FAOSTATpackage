\name{aggCountry}
\alias{aggCountry}
\title{Function for aggregating entities into countries}
\usage{
  aggCountry(aggVar, weightVar = rep(1, length(aggVar)),
    year = "Year", data,
    relationDF = FAOcountryProfile[, c("FAOST_CODE", "MOTHER_M49_CODE")],
    aggMethod = rep("sum", length(aggVar)),
    keepUnspecified = TRUE, unspecifiedCode = 0)
}
\arguments{
  \item{aggVar}{The vector of names of variables which the
  aggregation will be performed}

  \item{weightVar}{The weighting vector if the aggregation
  method is weighted.mean. The vector should be the same
  length as the arguement \code{aggMethod}.}

  \item{year}{The column which indexes the time.}

  \item{data}{The data frame for the collapse aggregation
  to take place.}

  \item{relationDF}{A relational data.frame which specifies
  the territory and the mother country. At least one column
  or the first column must have the same name corresponding
  to the data.}

  \item{aggMethod}{The method for aggregation, currently
  supports sum, mean and weighted mean when the weights are
  supplied.}

  \item{keepUnspecified}{It is common that certain
  territories are not included in the classification in
  particular the disputed regions. This option controls
  whether these data are aggregated and kept as a seperate
  entity.}

  \item{unspecifiedCode}{The output code of the unspecified
  group.}
}
\description{
  The function takes a relational data.frame and computes
  the aggregation based on the relation specified. The
  defaulted aggregates entities into the United Nations
  Statistics Division M49 country level definition.
}
\details{
  The length of \code{aggVar}, \code{aggMethod},
  \code{weightVar} must be the same.
}
\seealso{
  \code{\link{aggRegion}}
}

