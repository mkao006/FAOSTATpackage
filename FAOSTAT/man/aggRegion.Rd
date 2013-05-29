\name{aggRegion}
\alias{aggRegion}
\title{Function for computing regional aggregates}
\usage{
  aggRegion(aggVar, weightVar = rep(NA, length(aggVar)),
    year = "Year", data,
    relationDF = FAOcountryProfile[, c("FAOST_CODE", "UNSD_MACRO_REG_CODE")],
    aggMethod = rep("sum", length(aggVar)),
    keepUnspecified = TRUE, unspecifiedCode = 0,
    threshold.prop = 0.65, threshold.country = 15)
}
\arguments{
  \item{aggVar}{The vector of names of the variables to be
  aggregated}

  \item{weightVar}{The vector of names of the variables to
  be used as weighting when the aggregation method is
  weighted.}

  \item{year}{The column which contain index the time.}

  \item{data}{The data frame containing the country level
  data}

  \item{relationDF}{A relational data.frame which specifies
  the territory and the mother country. At least one column
  or the first column must have the same name corresponding
  to the data.}

  \item{aggMethod}{Can be a single method for all data or a
  vector specifying different method for each variable
  aggregated.}

  \item{unspecifiedCode}{The output code of the unspecified
  group.}

  \item{threshold.prop}{The missing threshold for the
  aggregation rule to be applied, the default is set to
  only compute aggregation if there are more than 65% of
  data available (0.65).}

  \item{threshold.country}{The change in the country which
  are allowed to vary from year to year in computing the
  aggregates. The default is}

  \item{keepUnspecified}{Whether countries with unspecified
  region should be aggregated into an "Unspecified" group
  or simply drop. Default to create the new group.}
}
\description{
  The function takes a relational data.frame and computes
  the aggregation based on the relation specified.
}
\details{
  The length of \code{aggVar}, \code{aggMethod},
  \code{weightVar} must be the same.

  This function is essentially the same as
  \code{aggCountry}, however there rules in which are
  applied to ensure that the aggregates are comparable over
  time and consistent.

  \itemize{

  \item Aggregation should not be computed if insufficient
  countries have reported data. This correpsonds to the
  argument \code{threshold.prop} which specifies the
  percentage which of country must report data.

  \item Aggregation will only be performed if the number of
  countries reporting are similar accross the years.
  \code{threshold.country} controls this behaviour by
  allowing years which have numbers of reporting country
  vary within this threshold }
}
\examples{
## example.df = data.frame(FAOST_CODE = rep(c(1, 2, 3), 2),
##                        Year = rep(c(2010, 2011), c(3, 3)),
##                        value = rep(c(1, 2, 3), 2),
##                        weight = rep(c(0.3, 0.7, 1), 2))

## Lets aggregate country 1 and 2 into one country and keep country
## 3 seperate.
## relation.df = data.frame(FAOST_CODE = 1:3, NEW_CODE = c(1, 1, 2))
}
\seealso{
  \code{\link{aggCountry}}
}

