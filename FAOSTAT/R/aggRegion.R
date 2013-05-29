##' Function for computing regional aggregates
##'
##' The function takes a relational data.frame and computes the aggregation
##' based on the relation specified.
##'
##' The length of \code{aggVar}, \code{aggMethod}, \code{weightVar} must
##' be the same.
##'
##' This function is essentially the same as \code{aggCountry},
##' however there rules in which are applied to ensure that the
##' aggregates are comparable over time and consistent.
##'
##' \itemize{
##'
##'     \item Aggregation should not be computed if insufficient
##' countries have reported data. This correpsonds to the argument
##' \code{threshold.prop} which specifies the percentage which of
##' country must report data.
##'
##'     \item Aggregation will only be performed if the number of
##' countries reporting are similar accross the
##' years. \code{threshold.country} controls this behaviour by
##' allowing years which have numbers of reporting country vary within
##' this threshold
##' }
##'
##' @param aggVar The vector of names of the variables to be
##' aggregated
##' @param weightVar The vector of names of the variables to be used
##' as weighting when the aggregation method is weighted.
##' @param year The column which contain index the time.
##' @param data The data frame containing the country level data
##' @param relationDF A relational data.frame which specifies the
##' territory and the mother country. At least one column or the first
##' column must have the same name corresponding to the data.
##' @param aggMethod Can be a single method for all data or a vector
##' specifying different method for each variable aggregated.
##' @param unspecifiedCode The output code of the unspecified group.
##' @param threshold.prop The missing threshold for the aggregation
##' rule to be applied, the default is set to only compute aggregation
##' if there are more than 65% of data available (0.65).
##' @param threshold.country The change in the country which are
##' allowed to vary from year to year in computing the aggregates. The
##' default is
##' @param keepUnspecified Whether countries with unspecified region
##' should be aggregated into an "Unspecified" group or simply
##' drop. Default to create the new group.
##'
##' @seealso \code{\link{aggCountry}}
##' @export
##' @examples
##' ## example.df = data.frame(FAOST_CODE = rep(c(1, 2, 3), 2),
##' ##                        Year = rep(c(2010, 2011), c(3, 3)),
##' ##                        value = rep(c(1, 2, 3), 2),
##' ##                        weight = rep(c(0.3, 0.7, 1), 2))
##'
##' ## Lets aggregate country 1 and 2 into one country and keep country
##' ## 3 seperate.
##' ## relation.df = data.frame(FAOST_CODE = 1:3, NEW_CODE = c(1, 1, 2))
##'

## NOTE(Michael): if there is NA in the weight, then it is replaced
##                with zero.


aggRegion = function(aggVar, weightVar = rep(NA, length(aggVar)),
    year = "Year", data,
    relationDF = FAOcountryProfile[, c("FAOST_CODE", "UNSD_MACRO_REG_CODE")],
    aggMethod = rep("sum", length(aggVar)), keepUnspecified = TRUE,
    unspecifiedCode = 0, threshold.prop = 0.65, threshold.country = 15){

    ## Obtain the name of the relationships
    inCode = colnames(relationDF)[1]
    outCode = colnames(relationDF)[2]
    colnames(relationDF) = c("inCode", "outCode")

    ## Checks
    if(!all(unique(data[, inCode]) %in% relationDF[, "inCode"]))
        stop("Not all entries are mateched with a relationship")
    if(!(inCode %in% colnames(data)))
        stop("Input code in relationship data frame not found in data")
    if(!all(identical(length(aggVar), length(aggMethod)),
            identical(length(aggVar), length(weightVar))))
        stop("length of aggVar, aggMethod and weightVar are not all equal")
    if(any(is.na(weightVar[which(aggMethod == "weighted.mean")])))
        stop("Weighting variable missing for some variable")

    ## Subset the data to compute aggregates variables in the data, still
    ## a problem if the weighted variable is not in the data.
    ind = which(aggVar %in% colnames(data))
    aggVar = aggVar[ind]
    aggMethod = aggMethod[ind]
    weightVar = weightVar[ind]

    ## Merge the data with the relation data
    raw.dt = data.table(merge(x = relationDF, y = data, by.x = "inCode",
                        by.y = inCode, all.y = TRUE), equalWeight = 1)
    ## keys = c("inCode", year)
    setkeyv(raw.dt, c("inCode", year))

    n.var = length(aggVar)
    weightVar[is.na(weightVar)] = "equalWeight"

    if(keepUnspecified){
        ## Check the unspecified code and the outputcode
        if(typeof(relationDF[, "outCode"]) != typeof(unspecifiedCode))
            stop("The type of output code and unspecified code need to be the same")
        Unspecified = raw.dt[is.na(outCode), inCode]
        raw.dt[is.na(outCode), outCode := unspecifiedCode]
    } else {
        raw.dt = subset(raw.dt, !is.na(outCode))
    }


    final = unique(raw.dt[, c("outCode", year), with = FALSE])
    setkeyv(final, c("outCode", year))

    foo = function(x, w, FUN){
        w[is.na(w)] = 0
        switch(FUN,
               sum = {tmp = sum(x, na.rm = any(!is.na(x)))},
               mean = {tmp = mean(x, na.rm = TRUE)},
               weighted.mean = {tmp = weighted.mean(x, w, na.rm = TRUE)}
               )
        as.numeric(tmp)
    }

    printLab(paste("Computing Regional Aggregation for ", length(aggVar),
                   " variables", sep = ""))
    pb = txtProgressBar(min = 0, max = n.var, style = 3)
    for(i in 1:n.var){
        base = data.table()
        if(!all(is.na(raw.dt[, aggVar[i], with = FALSE]))){


            ## Aggregation should only be performed on years which have
            ## comparable number of countries.
            tmp.dt = raw.dt[, c("outCode", year, aggVar[i], weightVar[i]),
                            with = FALSE]
            tmp.dt[, nc := sum(is.na(aggVar[i])), by = Year]
            tmp.dt = tmp.dt[tmp.dt$nc >= max(tmp.dt$nc) - threshold.country, ]
            rangeYears = range(na.omit(raw.dt[, c(year, aggVar[i]),
                                       with = FALSE])[, year, with = FALSE])
            for(j in rangeYears[1]:rangeYears[2]){
                if(sum(is.na(raw.dt[Year == j, aggVar[i], with = FALSE] *
                             raw.dt[Year == j, weightVar[i], with = FALSE])) <=
                   (1 - threshold.prop) * NROW(raw.dt[Year == j, aggVar[i],
                                                with = FALSE])){
                    tmp = raw.dt[Year == j,
                        foo(x = get(aggVar[i]), w = get(weightVar[i]),
                            FUN = aggMethod[i]), by = outCode]
                    tmp[, Year := j]
                } else {
                    tmp = unique(raw.dt[Year == j, "outCode", with = FALSE])
                    tmp[, V1 := as.numeric(NA)]
                    tmp[, Year := j]
                }
                base = rbind(base, tmp)
            }
        } else {
            base = unique(raw.dt[, c("outCode", year), with = FALSE])
            base[, V1 := as.numeric(NA)]
        }
        setkeyv(base, c("outCode", year))
        setnames(base, "V1", aggVar[i])
        final = merge(x = final, y = base, all.x = TRUE)
        setTxtProgressBar(pb, i)
    }
    close(pb)
    setnames(final, "outCode", outCode)
    if (keepUnspecified) {
        cat(paste("\nThe following territories have been aggregated into code = ",
                  unspecifiedCode, ":\n", sep = ""))
        print(unique(Unspecified))
    } else{
        cat("\n")
    }
    data.frame(final)
}

utils::globalVariables(names = c("nc", "Year", "V1"))
