\name{indConstruct}
\alias{indConstruct}
\title{Construct indices}
\usage{
  indConstruct(data, origVar, newVarName = NA,
    baseYear = 2000)
}
\arguments{
  \item{data}{The data frame containing the data}

  \item{origVar}{The variable in which the indices is to be
  computed}

  \item{newVarName}{The name assigned to the new variable,
  if missing then .SC/.SH/.GR/.CH/.IND will be appended
  depending on the type of construction.}

  \item{baseYear}{The year which will serve as the base}
}
\value{
  The indice
}
\description{
  A function for constructing indices
}
\examples{
test.df = data.frame(FAOST_CODE = rep(1, 100), Year = 1901:2000,
                      test = 1:100)
indConstruct(test.df, origVar = "test", baseYear = 1950)
}

