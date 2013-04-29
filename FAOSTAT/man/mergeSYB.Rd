\name{mergeSYB}
\alias{mergeSYB}
\title{Function for merging data from different source.}
\usage{
  mergeSYB(x, y, outCode = "FAOST_CODE", all = TRUE, ...)
}
\arguments{
  \item{x}{data frames, or objects to be coerced to one.}

  \item{y}{data frames, or objects to be coerced to one.}

  \item{outCode}{The country code system to be used to join
  the different sources.}

  \item{all}{Same as the merge function, defaulted to an
  outer join.}

  \item{...}{Arguments to be passed on to the merge
  function.}
}
\description{
  This function searches for supported country system and
  translate the data to allow for join.
}
\details{
  The names of the data to be merged has to be the same as
  the FAOcountryProfile code name.
}

