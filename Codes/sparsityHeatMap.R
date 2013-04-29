##' Function for examine sparsity
##'
##' The function plots the availability of a specific variable over time
##' by country and are grouped by the average value of the indicator.
##'
##' @param data The data frame containing the data.
##' @param country The column name containing the country name.
##' @param year The columne name containing the time information.
##' @param var The variable to be examined.
##' @param group The colomun name which the data should be grouped, if
##' missing quantile of the variable is used.
##' @param ncol The number of column for the resulting plot
##' @export

sparsityHeatMap = function(data, country, year, var, group, ncol){
  ## Subset and sort data

  ## Compute the mean over time to determin grouping
  if(missing(group)){
      df = data[, c(country, year, var)]
      df = arrange(df, get(var))
      countryMean.df = ddply(.data = df, .variables = c(country),
          .fun = function(x) mean(x[, var], na.rm = TRUE))
      colnames(countryMean.df)[2] = "Mean"
      df = merge(df, countryMean.df, by = country)
      facet = factor(findInterval(df[, "Mean"], quantile(df[, "Mean"],
          seq(0, 1, length = 4), na.rm = TRUE), rightmost.closed = TRUE))
      levels(facet) = c("Lower", "Middle", "Upper")
  } else {
      df = data[, c(country, year, var, group)]
      df = arrange(df, get(var))
      facet = df[, group]
  }


  ## Create the data frame for whether data is available and the group
  isMissing.df = data.frame(df[, c(country, year)],
    group = as.numeric(findInterval(df[, var],
        classIntervals(df[, var], n = 10)$brks, rightmost.closed = TRUE)),
    ## isMissing = is.na(df[, var]),
    facet = facet, stringsAsFactors = FALSE)


  isMissing.df$group = factor(isMissing.df$group,
      levels = sort(unique(isMissing.df$group), na.last = FALSE),
      exclude = NULL)

  isMissing.df[, country] = factor(isMissing.df[, country])
  breaks = extended(min(isMissing.df[, year]),
      max(isMissing.df[, year]), m = 5)

  ## Plot the data
  ggplot(data = isMissing.df, aes_string(x = year, y = country)) +
    geom_tile(aes(fill = group)) +
    scale_fill_manual(name =
                      paste("Data for '", var, "' is missing?", sep = ""),
                      values = c(rev(heat.colors(12))[-c(1:2)], "white")) +
    scale_x_continuous(breaks = breaks) +
    facet_wrap(~facet, ncol = ncol, scales = "free_y") +
    theme(axis.text.y = element_text(size = 6),
          axis.text.x = element_text(size = 8,
          angle = 30, hjust = 1),
          legend.direction = "horizontal", legend.position = "top") +
    xlab("Year") +  ylab(NULL)
}
