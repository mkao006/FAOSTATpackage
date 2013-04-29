\name{constructSYB}
\alias{constructSYB}
\title{Construct/Creat new variable.}
\usage{
  constructSYB(data, origVar1, origVar2, newVarName = NA,
    constructType = c("share", "growth", "change", "index"),
    grFreq = 1, grType = c("ls", "geo"), baseYear = 2000)
}
\arguments{
  \item{data}{The data frame containing the raw variable}

  \item{origVar1}{The variable name to be used in
  construction, refer to Details for more information and
  useage.}

  \item{origVar2}{The variable name to be used in
  construction, refer to Details for more information and
  useage.}

  \item{newVarName}{The name assigned to the new variable,
  if missing then .SC/.SH/.GR/.CH will be appended
  depending on the type of construction}

  \item{constructType}{The type of construction, refer to
  Details for more information.}

  \item{grFreq}{The frequency for the growth rate to be
  computed.}

  \item{grType}{The method for the growth to be calculated,
  currently supports least squares and geometric.}

  \item{baseYear}{The base year to be used for constructing
  index.}
}
\value{
  A data frame containing both the original data frame and
  the processed data and also a list indicating whether the
  construction passed or failed.
}
\description{
  A function used to construct new variables from existing
  variables.
}
\details{
  Currently two types of construction are supported, either
  share or growth rate computation.

  Share can be a share of total or share of another
  variable depending on whether an additional variable is
  supplied or not.
}

