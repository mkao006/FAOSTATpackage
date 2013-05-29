\name{getFAO}
\alias{getFAO}
\title{Access to FAO FAOSTAT API}
\usage{
  getFAO(name = NULL, domainCode = "RL",
    elementCode = 5110, itemCode = 6621, query,
    printURL = FALSE, productionDB = FALSE, useCHMT = TRUE,
    outputFormat = "wide")
}
\arguments{
  \item{name}{The name to be given to the variable.}

  \item{domainCode}{The domain of the data}

  \item{elementCode}{The code of the element}

  \item{itemCode}{The code of the specific item}

  \item{query}{The object created if using the FAOsearch
  function}

  \item{printURL}{Whether the url link for the data should
  be printed}

  \item{productionDB}{Access to the production database,
  defaulted to public}

  \item{useCHMT}{logical, whether the CHMT function should
  be applied to avoid double counting of China.}

  \item{outputFormat}{The format of the data, can be 'long'
  or 'wide'.}
}
\value{
  Outputs a data frame containing the specified data
}
\description{
  A function to access FAOSTAT data through the FAOSTAT API
}
\details{
  Need to account for multiple itemCode, currently only
  support one single variable.

  Add date range in to argument
}
\seealso{
  \code{\link{getWDI}}, \code{\link{getWDItoSYB}},
  \code{\link{getFAOtoSYB}}, \code{\link{FAOsearch}}
}

