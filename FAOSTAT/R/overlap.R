##' This function checks whether there are overlapping between the
##' transitional countries.
##'
##'
##' @param old The FAOST_CODE of the old countries
##' @param new The FAOST_CODE of the new countries
##' @param var The variable to be checked
##' @param year The column which index the time.
##' @param data The data frame
##'
##' @export
##'


overlap = function(old, new, var, year = "Year", data){
    ## if(country != "FAOST_CODE")
    ##     data = translateCountryCode(data, from = country,
    ##         to = "FAOST_CODE")
    n.var = length(var)
    for(i in 1:n.var){
        old_years = unique(data[data$FAOST_CODE %in% old &
                           !is.na(data[, var[i]]), year])
        new_years = unique(data[data$FAOST_CODE %in% new &
                           !is.na(data[, var[i]]), year])
        int_years = intersect(old_years, new_years)
        if(length(int_years) > 0)
            cat(paste0(var[i], " has overlap in:\n\t Year = ",
                       paste0(int_years, collapse = ", "),
                       "\n\tfor FAOST_CODE = ",
                       paste0(c(old, new), collapse = ", "), "\n"))
        ## if(take == "old"){
        ##     data[data$FAOST_CODE %in% new &
        ##          data[, year] %in% int_years, var[i]] = NA
        ## } else if(take == "new"){
        ##     data[data$FAOST_CODE %in% old &
        ##          data[, year] %in% int_years, var[i]] = NA
        ## }
    }
    data
}
