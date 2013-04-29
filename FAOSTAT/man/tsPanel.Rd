\name{tsPanel}
\alias{tsPanel}
\title{Function to plot data by country and time}
\usage{
  tsPanel(Data, country, year, var, smoother = "lm",
    ncol = 10, facetScales = "free_y",
    ylab = substitute(var), xlab = "Year")
}
\arguments{
  \item{Data}{The data frame containing the data}

  \item{country}{The column name containing the country
  name.}

  \item{year}{The columne name containing the time
  information.}

  \item{var}{The variable to be examined.}

  \item{smoother}{The type of smoothing to be used, refer
  to stats_smooth() for available options.}

  \item{ncol}{Number of columns for the matrix panel}

  \item{facetScales}{Controls the axis of the facets, refer
  to facet_wrap() for more detail.}

  \item{ylab}{The y-axis label.}

  \item{xlab}{THe x-axis label.}
}
\description{
  A function exploring the trend od the variable by country
  and over time.
}

