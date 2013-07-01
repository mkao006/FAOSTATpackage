## Create query for FAOSTAT, the domain, element and item codes are
## requird, name is optional.
(FAOquery.df =
  data.frame(name = c("arable_land", "cereal_export", "cereal_production"),
             domainCode = c("RL", "TP", "QC"),
             itemCode = c(6621, 1944, 1717),
             elementCode = c(5110, 5922, 5510),
             stringsAsFactors = FALSE))

Sys.sleep(5)

## Download data from FAOSTAT
FAO.lst = with(FAOquery.df,
    getFAOtoSYB(name = name, domainCode = domainCode,
                itemCode = itemCode, elementCode = elementCode))

## Interactive function to search for the domain, item and element
## codes for download.
FAOsearch()

## Download the query from the FAOsearch() function.
FAO.lst = getFAOtoSYB(query = .LastSearch)
