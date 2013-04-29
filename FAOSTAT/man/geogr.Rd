\name{geogr}
\alias{geogr}
\title{Geometric growth rate}
\usage{
  geogr(x, n = 1)
}
\arguments{
  \item{x}{The time series for the growth rate to be
  calculated.}

  \item{n}{The period for the growth to be calculated
  over.}
}
\value{
  The n-period geometric growth rate of the time series.
}
\description{
  Function for generating the n-period rolling geometric
  growth rate.
}
\details{
  In order to ensure the growth rate calculated is
  reliable, the following rule are applied. \enumerate{
  \item 50\% of the data must be present.  \item The length
  of the time series must be greater than n } Otherwise the
  growth will not be computed.
}
\examples{
test.ts = abs(rnorm(100))
geogr(test.ts, 1)
geogr(test.ts, 3)
geogr(test.ts, 10)
}

