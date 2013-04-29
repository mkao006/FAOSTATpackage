\name{lsgr}
\alias{lsgr}
\title{Least squares growth rate}
\usage{
  lsgr(x, n = 1)
}
\arguments{
  \item{x}{The time series for the growth rate to be
  calculated}

  \item{n}{The period for the growth to be calculated
  over.}
}
\value{
  The n-period least squares growth rate of the time series
}
\description{
  Function for generating the n-period rolling least
  squares growth rate.
}
\details{
  Missing values are ommited in the regression. (Will need
  to check this.)

  TODO (Michael): There is still some error associated with
  this function, will need to investigate further. Will
  need a rule for this, when the fluctuation is large and
  data are sufficient then take the lsgr, otherwise the
  geogr.

  In order to ensure the growth rate calculated is
  reliable, the following rule are applied. \enumerate{
  \item 50\% of the data must be present.  \item The length
  of the time series must be greater than n. } Otherwise
  the growth will not be computed.
}
\examples{
test.ts = abs(rnorm(100))
lsgr(test.ts, 1)
lsgr(test.ts, 3)
lsgr(test.ts, 10)
}

