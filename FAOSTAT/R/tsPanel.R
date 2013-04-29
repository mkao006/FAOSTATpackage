##' Function to plot data by country and time
##'
##' A function exploring the trend od the variable by country and over time.
##'
##' @param Data The data frame containing the data
##' @param country The column name containing the country name.
##' @param year The columne name containing the time information.
##' @param var The variable to be examined.
##' @param smoother The type of smoothing to be used, refer to
##' stats_smooth() for available options.
##' @param ncol Number of columns for the matrix panel
##' @param facetScales Controls the axis of the facets, refer to
##' facet_wrap() for more detail.
##' @param ylab The y-axis label.
##' @param xlab THe x-axis label.
##' @export
##'

tsPanel = function(Data, country, year, var, smoother = "lm", ncol = 10,
  facetScales = "free_y", ylab = substitute(var), xlab = "Year"){
  allMiss.df = ddply(Data, .variables = c(country),
    .fun = function(x) all(is.na(x[, var])))
  subData = subset(Data, 
    subset = !(Data[, country] %in% allMiss.df[allMiss.df$V1, country]))
  myPlot =
    ggplot(subData, aes_string(x = year, y = var)) + geom_point() +
      geom_smooth(aes_string(group = country), method = smoother) +
        facet_wrap(eval(country), ncol = ncol,
                   scales = facetScales) +
                     theme(axis.text.y = element_blank(),
                           axis.text.x = element_blank(),
                           axis.ticks = element_blank()) +
                             ylab(ylab) + xlab(xlab)
  print(myPlot)
}



