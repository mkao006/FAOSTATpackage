##' Function for aggregating entities into countries
##'
##' The function takes a relational data.frame and computes the
##' aggregation based on the relation specified. The defaulted
##' aggregates entities into the United Nations Statistics Division
##' M49 country level definition.
##'
##' The length of \code{aggVar}, \code{aggMethod}, \code{weightVar} must
##' be the same.
##'
##' @param aggVar The vector of names of variables which the
##' aggregation will be performed
##' @param weightVar The weighting vector if the aggregation method is
##' weighted.mean. The vector should be the same length as the
##' arguement \code{aggMethod}.
##' @param year The column which indexes the time.
##' @param data The data frame for the collapse aggregation to take
##' place.
##' @param relationDF A relational data.frame which specifies the
##' territory and the mother country. At least one column or the first
##' column must have the same name corresponding to the data.
##' @param aggMethod The method for aggregation, currently supports
##' sum, mean and weighted mean when the weights are supplied.
##' @param keepUnspecified It is common that certain territories are
##' not included in the classification in particular the disputed
##' regions. This option controls whether these data are aggregated
##' and kept as a seperate entity.
##' @param unspecifiedCode The output code of the unspecified group.
##'
##' @seealso \code{\link{aggRegion}}
##' @export

aggCountry = function(aggVar, weightVar = rep(1, length(aggVar)),
    year = "Year", data,
    relationDF = FAOcountryProfile[, c("FAOST_CODE", "MOTHER_M49_CODE")],
    aggMethod = rep("sum", length(aggVar)), keepUnspecified = TRUE,
    unspecifiedCode = 0){

  ## NOTE(Michael): No need to implement rule for country level
  ##                aggregation, it is assumed if the territory has
  ##                value NA then the statistics is already incorporated at
  ##                the country level.
  relationCode = intersect(colnames(relationDF), colnames(data))
  if(length(relationCode) != 1)
    stop("Incorrect relationship specified between data nad relation data frame")
  outputCode = colnames(relationDF)[colnames(relationDF) != relationCode]

  if(!all(unique(data[, relationCode]) %in% relationDF[, relationCode]))
    stop("Some relation code are missing, make sure all relation are specified")

  ## Check the length
  if((length(aggVar) != length(aggMethod)) |
     (length(aggVar) != length(weightVar)))
    stop("length of aggVar, aggMethod and weightVar are not all equal")

  ## Check the weightVar vector
  if(any(is.na(weightVar[which(aggMethod == "weighted.mean")])))
    stop("weightVar does not match the aggregation method")

  ## Subset the data to compute aggregates variables in the data, still
  ## a problem if the weighted variable is not in the data.
  ind = which(aggVar %in% colnames(data))
  aggVar = aggVar[ind]
  aggMethod = aggMethod[ind]
  weightVar = weightVar[ind]
  n.var = length(aggVar)

  ## Merge the data with the relation data
  raw.dt = data.table(merge(x = relationDF, y = data, by = relationCode,
      all.y = TRUE), equalWeight = 1)
  keys = c(outputCode, year)
  setkeyv(raw.dt, keys)


  if(keepUnspecified){
    Unspecified = unique(raw.dt[is.na(get(outputCode)),
      relationCode, with = FALSE])
    raw.dt[is.na(get(outputCode)), outputCode := as.integer(unspecifiedCode),
           with = FALSE]
  }

  ## Will have to decide whether to aggregate if the mother country is
  ## missing
  foo = function(x, w, FUN, ...){
    w[is.na(x)] = 0
    switch(FUN,
           sum = {tmp = sum(x, na.rm = any(!is.na(x)))},
           mean = {tmp = mean(x, na.rm = TRUE, ...)},
           weighted.mean = {tmp = weighted.mean(x, w, na.rm = TRUE, ...)}
           )
    as.numeric(tmp)
  }

  base = unique(raw.dt[, c(outputCode, year), with = FALSE])
  ## setnames(base, outputCode, relationCode)

  weightVar[is.na(weightVar)] = "equalWeight"

  printLab(paste("Computing Country Aggregation on ", length(aggVar),
                 " variables", sep = ""))
  pb = txtProgressBar(min = 0, max = n.var, style = 3)
  for(i in 1:n.var){
    if(aggMethod[i] == "discard"){
      tmp = subset(raw.dt, select = c(outputCode, year, aggVar[i]),
        subset = get(relationCode) == get(outputCode))
    } else {
      tmp = raw.dt[,
        foo(x = get(aggVar[i]), w = get(weightVar[i]), FUN = aggMethod[i]),
        by = eval(paste(outputCode, ",Year", sep = ""))]
      setnames(tmp, "V1", aggVar[i])
    }
    base = merge(x = base, y = tmp, by = c(outputCode, year))
      setTxtProgressBar(pb, i)
  }
  close(pb)
  if(keepUnspecified){
    cat(paste("\nThe following territories have been aggregated into code = ",
              unspecifiedCode, ":\n", sep = ""))
    print(unique(Unspecified))
  }
  data.frame(base)
}
