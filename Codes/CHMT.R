##' This function avoids double counting of China
##'
##' We assume that if the pre-aggregates exists then the individual
##' level are present. This function should only used when performing
##' aggregation.
##'
##' We decide to use the smaller subsets in the regional level because
##' weighting variable may not exist for other variables for the
##' larger subsets.
##'
##' The function only work for FAOST_CODE, if the country coding
##' system is not in FAOST_CODE then use the translateCountryCode
##' function to translate it.
##'
##' @param var The variables that require to be sanitized.
##' @param data The data frame which contains the data
##' @param year The column which correspond to the year.
##' @export


CHMT = function(var, data, year = "Year"){
    for(i in 1:length(var)){
      if(length(unique(data[data$FAOST_CODE %in% c(41, 351, 357) &
                            !is.na(data[, var[i]]), "FAOST_CODE"])) > 1) {
        cat(paste0("\nNOTE: Multiple China detected in '", var[i],
                   "' sanitization is performed\n"))
        for(j in sort(unique(data[, year]))){
          ## If the China, mainland exist then we will not use
          ## (China + Taiwan) nor the (China + Hong Kong + Macau
          ## + Taiwan).
          if(NROW(data[data$FAOST_CODE == 41 &
                         data[, year] == j, var[i]]) == 1){
            if(!is.na(data[data$FAOST_CODE == 41 &
                             data[, year] == j, var[i]])){
              data[data$FAOST_CODE %in% c(351, 357), var[i]] = NA
              ## If China mainland does not exist then we
              ## will use (China + Taiwan).
            } else if(NROW(data[data$FAOST_CODE == 357 &
                                  data[, year] == j, var[i]]) == 1){
              if(!is.na(data[data$FAOST_CODE == 357 &
                               data[, year] == j, var[i]])){
                data[data$FAOST_CODE %in% c(41, 214, 351),
                     var[i]] = NA
                ## If both fails then we will use (China +
                ## Hong Kong + Macau + Taiwan)
              } else if(NROW(data[data$FAOST_CODE == 351 &
                                    data[, year] == j, var[i]]) == 1){
                if(!is.na(data[data$FAOST_CODE == 351 &
                                 data[, year] == j, var[i]])){
                  data[data$FAOST_CODE %in%
                         c(41, 96, 128, 214, 357), var[i]] = NA
                }
              }
            }
          }
        }
      } 
    }
    data
}



## CHMT = function(var, data, year = "Year", aggLevel = c("country", "region")){
##     aggLevel = match.arg(aggLevel)
##     if(aggLevel == "region"){
##         for(i in 1:length(var)){
##             for(j in sort(unique(data[, year]))){
##                 ## If the China, mainland exist then we will not use
##                 ## (China + Taiwan) nor the (China + Hong Kong + Macau
##                 ## + Taiwan).
##                 if(NROW(data[data$FAOST_CODE == 41 &
##                              data[, year] == j, var[i]]) == 1){
##                     if(!is.na(data[data$FAOST_CODE == 41 &
##                                    data[, year] == j, var[i]])){
##                         data[data$FAOST_CODE %in% c(351, 357), var[i]] = NA
##                         ## If China mainland does not exist then we
##                         ## will use (China + Taiwan).
##                     } else if(NROW(data[data$FAOST_CODE == 357 &
##                                         data[, year] == j, var[i]]) == 1){
##                         if(!is.na(data[data$FAOST_CODE == 357 &
##                                        data[, year] == j, var[i]])){
##                             data[data$FAOST_CODE %in% c(41, 214, 351),
##                                  var[i]] = NA
##                             ## If both fails then we will use (China +
##                             ## Hong Kong + Macau + Taiwan)
##                         } else if(NROW(data[data$FAOST_CODE == 351 &
##                                             data[, year] == j, var[i]]) == 1){
##                             if(!is.na(data[data$FAOST_CODE == 351 &
##                                            data[, year] == j, var[i]])){
##                                 data[data$FAOST_CODE %in%
##                                      c(41, 96, 128, 214, 357), var[i]] = NA
##                             }
##                         }
##                     }
##                 }
##             }
##         }
##     } else if(aggLevel == "country"){
##         for(i in 1:length(var)){
##             for(j in sort(unique(data[, year]))){
##                 ## For country level we use (China + Taiwan) as it is
##                 ## the definition of China.
##                 if(NROW(data[data$FAOST_CODE == 357 &
##                              data[, year] == j, var[i]]) == 1){
##                     if(!is.na(data[data$FAOST_CODE == 357 &
##                                    data[, year] == j, var[i]])){
##                         data[data$FAOST_CODE %in% c(41, 214, 351),
##                              var[i]] = NA
##                         ## If (China + Taiwan) does not exist then we
##                         ## try to obtain it by adding China and
##                         ## Taiwan.
##                     } else if(NROW(data[data$FAOST_CODE == 41 &
##                                         data[, year] == j, var[i]]) == 1){
##                         if(!is.na(data[data$FAOST_CODE == 41 &
##                                        data[, year] == j, var[i]])){
##                             data[data$FAOST_CODE %in% c(351, 357),
##                                  var[i]] = NA
##                             ## If both case fails then we will report
##                             ## the whole (China + Hong Kong + Macau +
##                             ## Taiwan) and remove the individual
##                             ## entries.
##                         }
##                         else if(NROW(data[data$FAOST_CODE == 351 &
##                                           data[, year] == j, var[i]]) == 1){
##                             if(!is.na(data[data$FAOST_CODE == 351 &
##                                            data[, year] == j, var[i]])){
##                                 data[data$FAOST_CODE %in%
##                                      c(41, 96, 128, 214, 357), var[i]] = NA
##                             }
##                         }
##                     }
##                 }
##             }
##         }
##     }
##     data
## }
