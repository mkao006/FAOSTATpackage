##' Access to FAO FAOSTAT API
##'
##' A function to access FAOSTAT data through the FAOSTAT API
##'
##' Need to account for multiple itemCode, currently only support one
##' single variable.
##'
##' Add date range in to argument
##' @param name The name to be given to the variable.
##' @param domainCode The domain of the data
##' @param elementCode The code of the element
##' @param itemCode The code of the specific item
##' @param query The object created if using the FAOsearch function
##' @param printURL Whether the url link for the data should be printed
##' @param productionDB Access to the production database, defaulted to public
##' @param useCHMT logical, whether the CHMT function should be
##' applied to avoid double counting of China.
##' @param outputFormat The format of the data, can be 'long' or 'wide'.
##' @return Outputs a data frame containing the specified data
##' @export
##'
##' @seealso \code{\link{getWDI}}, \code{\link{getWDItoSYB}},
##' \code{\link{getFAOtoSYB}}, \code{\link{FAOsearch}}
##'

## NOTE(Michael): Maybe change input from csv to json.

getFAO = function(name = NULL, domainCode = "RL", elementCode = 5110,
                  itemCode = 6621, query, printURL = FALSE, productionDB = FALSE,
                  useCHMT = TRUE, outputFormat = "wide"){
    if(!missing(query)){
        if(NROW(query) > 1)
            stop("Use 'getFAOtoSYB' for batch download")
        domainCode = query$domainCode
        itemCode = query$itemCode
        elementCode = query$elementCode
        if(is.null(query$name)){
            name = with(query, paste(domainCode, itemCode, elementCode, sep = "_"))
        } else {
            name = query$name
        }
    }

    if(is.null(name))
        name = paste(domainCode, itemCode, elementCode, sep = "_")

    if(productionDB){
        ## base = "http://ldvapp07.fao.org:8030/wds/api?"
        ## changed by FILIPPO new repository
        ## base = "http://lprapp16.fao.org:4012/wds/api?"
        ## changed by FILIPPO again
        base = "http://lprapp16.fao.org/wds/api?"

        database = "db=faostatprod&"
        selection = "select=D.AreaCode[FAOST_CODE],D.Year[Year],D.Value[Value],&from=data[D],element[E]&"
        condition = paste("where=D.elementcode(", elementCode, "),D.itemcode(",
        itemCode, "),D.domaincode('", domainCode, "')", sep = "")
        join = ",JOIN(D.elementcode:E.elementcode)&orderby=E.elementnamee,D.year"
    } else {
        base = "http://fenixapps.fao.org/wds/api?"
        database = "db=faostat&"
        selection = "select=A.AreaCode[FAOST_CODE],D.year[Year],D.value[Value],&from=data[D],element[E],item[I],area[A]&"
        condition = paste("where=D.elementcode(", elementCode, "),D.itemcode(",
        itemCode, "),D.domaincode('", domainCode, "')", sep = "")
        join = ",JOIN(D.elementcode:E.elementcode),JOIN(D.itemcode:I.itemcode),JOIN(D.areacode:A.areacode)&orderby=E.elementnamee,D.year"
    }

    out = "out=csv&"
    url = paste(base, out, database, selection, condition, join, sep = "")
    if(printURL) print(url)

    faoData = read.csv(file = url, stringsAsFactors = FALSE)
    ## while(inherits(faoData, "try-error")){
    ##   faoData = try(read.csv(url, stringsAsFactors = FALSE))
    ## }
    faoData$FAOST_CODE = as.integer(faoData$FAOST_CODE)
    faoData$Year = as.integer(faoData$Year)

    if(useCHMT)
        faoData = CHMT(var = "Value", data = faoData, year = "Year")

    if(outputFormat == "long" & !empty(faoData)){
        faoData$domainCode = domainCode
        faoData$itemCode = itemCode
        faoData$elementCode = elementCode
        faoData$name = name
    } else if(outputFormat == "wide"){
        colnames(faoData)[colnames(faoData) == "Value"] = name
    }
    faoData
}
