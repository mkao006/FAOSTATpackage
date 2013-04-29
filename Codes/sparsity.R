##' A function to calculate the sparsity of the variable
##'
##' When two or more variables are merged together, it is very likely
##' that spurious sparsity is created. This is due to the fact that
##' the years between the variables are not identicle and is
##' particularly true for data stored as data.frame with country and
##' year as column indexes.
##'
##' This function computes the sparsity by taking the data which are
##' observed between the first and the last observation.
##'
##' For example, if variable A is observed since 1960 while variable B
##' is collected since 1990. When they are merged together, NA's will
##' be created for variable prior to 1990 and these should not be used
##' to calculate the sparsity of the data.
##'
##' @param year The column which index the time
##' @param var The variable to calculate sparsity
##' @param data The data frame.
##' @export
##'


sparsity = function(year, var, data){
    ## new_data = arrange(data[, c(year, var)], get(year))
    ## minYear = min(na.omit(new_data)[, year])
    ## sub_data = new_data[new_data[, year] >= minYear,]

    rangeYear = range(na.omit(data)[, year])
    sub_data = data[data[, year] %in% rangeYear[1]:rangeYear[2], ]
    sum(is.na(sub_data[, var]))/NROW(sub_data)
}

