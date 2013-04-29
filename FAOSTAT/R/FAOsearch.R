##' A function to find the domain, element and item code for a
##' specific FAOSTAT query.
##'
##' @export

FAOsearch = function(){
    with(FAOmetaTable, {
        ## Find the Group code
        cat(paste(paste("(", 1:length(groupTable$groupName), ") ",
                        groupTable$groupName, sep = ""), collapse = "\n"))
        gcn = readline("\nWhich Group are you looking for: ")
        gc = groupTable[as.numeric(gcn), "groupCode"]

        ## Find the Domain code
        subdomainTable = subset(domainTable, groupCode == gc)
        cat(paste(paste("(", 1:length(subdomainTable$domainName), ") ",
                        subdomainTable$domainName, sep = ""),
                  collapse = "\n"))
        dcn = readline("\nWhich Domain are you looking for: ")
        dc = subdomainTable[as.numeric(dcn), "domainCode"]

        ## Individual or aggregated item
        cat("(0) Individual item (e.g. Apples, Wheat)\n")
        cat("(1) Aggregated item (e.g. Total cereals, Total meat\n")
        useAgg = readline("Are you looking for individual item or aggregated item:")

        if(as.numeric(useAgg)){
            ## Find the Item Aggregated code
            subitemTable = subset(itemAggTable, domainCode == dc)
            cat(paste(paste("(", 1:length(subitemTable$itemName), ") ",
                            subitemTable$itemName, sep = ""),
                      collapse = "\n"))
            icn = readline("\nWhich Item are you looking for? ('All' for everything):")
            if(icn == "All")
                icn = 1:length(subitemTable$itemName)
            ic = subitemTable[as.numeric(icn), "itemCode"]
        } else {

            ## Find the Item code
            subitemTable = subset(itemTable, domainCode == dc)
            cat(paste(paste("(", 1:length(subitemTable$itemName), ") ",
                            subitemTable$itemName, sep = ""),
                      collapse = "\n"))
            icn = readline("\nWhich Item are you looking for? ('All' for everything): ")
            if(icn == "All")
                icn = 1:length(subitemTable$itemName)
            ic = subitemTable[as.numeric(icn), "itemCode"]
        }

        ## Find the Element code
        subelementTable = subset(elementTable, domainCode == dc)
        cat(paste(paste("(", 1:length(subelementTable$elementName), ") ",
                        subelementTable$elementName, sep = ""),
                  collapse = "\n"))
        ecn = readline("\nWhich Element are you looking for? ('All' for everything):")
        if(ecn == "All")
            ecn = 1:length(subelementTable$elementName)
        ec = subelementTable[as.numeric(ecn), "elementCode"]

        ## Print the result and save
        ## cat(paste("Domain Code = ", dc, "\n", sep = ""))
        ## cat(paste("Item Code = ", ic, "\n", sep = ""))
        ## cat(paste("Element Code = ", ec, "\n", sep = ""))
        ## .LastSearch <<- list(domainCode = dc, itemCode = as.numeric(ic),
        ##                      elementCode = as.numeric(ec))
        tmp = expand.grid(dc, ic, ec, stringsAsFactors = FALSE)
        colnames(tmp) = c("domainCode", "itemCode", "elementCode")
        tmp = merge(tmp, domainTable[, c("domainCode", "domainName")],
            all.x = TRUE)
        tmp = merge(tmp, subitemTable[, c("itemCode", "itemName")],
            all.x = TRUE)
        final.df = merge(tmp, subelementTable[, c("elementCode", "elementName")],
            all.x = TRUE)
        final.df$name =
            with(final.df, paste(domainName, itemName, elementName, sep = "_"))
        final.df$domainName = NULL
        final.df$itemName = NULL
        final.df$elementName = NULL
        .LastSearch <<- final.df

    }
         )
}




