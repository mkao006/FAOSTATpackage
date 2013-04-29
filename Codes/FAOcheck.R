##' This function perform some check on the data
##'
##' @param var The variable to be checked
##' @param year The column which index the time.
##' @param data The data frame
##' @param type The type of check
##'
##' @export




FAOcheck = function(var, year = "Year", data, type = c("overlap", "multiChina")){
    ## if(country != "FAOST_CODE")
    ##     new_data = translateCountryCode(data, from = country,
    ##         to = "FAOST_CODE")
    if("overlap" %in% type){
        printLab("Check for overlap between transitional country")
        ## Check for overlap in Belgium, LuxemBourg and Belgium-Luxembourg
        data = overlap(old = 15, new = c(255, 256), var = var, data = data)
        data = overlap(old = 51, new = c(167, 199), var = var, data = data)
        take = overlap(old = 62, new = c(178, 238), var = var, data = data)
        ## Need more information on Netherlands Antilles and Trust
        ## Territory of Pacific Islands
        data = overlap(old = 186, new = c(272, 273), var = var, data = data)
        data = overlap(old = 228, new = c(57, 63, 73, 119, 126, 146, 185, 208,
                       213, 230, 235, 271), var = var, data = data)
        data = overlap(old = c(246, 247), new = 249, var = var, data = data)
        data = overlap(old = 248, new = c(80, 98, 154, 186, 198),
                var = var, data = data)
        cat("\nNOTE: It is common for data reported by the predecessor\n or the new transitional country to include the new country\n")
    }
    if("multiChina" %in% type){
        printLab("Check for existence of multiple China")
        data = CHMT(var = var, data = data, year = year)
    }
    data
}


