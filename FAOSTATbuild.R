###########################################################################
## Title: This script builds the FAOSTAT package
## Created: 09/05/2013
## Updated: 09/07/2014
###########################################################################

# Needed libraries --------------------------------------------------------

library(roxygen2)
library(RJSONIO)
library(utils)
library(knitr)

# Ghost script ------------------------------------------------------------

Sys.setenv(R_GSCMD='"C:/Program Files/gs/gs9.07/bin/gswin32c.exe"')

# FAOcountryProfile -------------------------------------------------------

tmp <- 
  read.csv(file = "FAOcountryProfileUTF8.csv", header = TRUE,
           stringsAsFactors = FALSE, na.string = "", encoding = "UTF-8")
tmp[tmp == " "] <- NA
FAOcountryProfile <- 
  tmp[, c("FAOST_CODE", "ADM0_CODE", "ISO2_CODE", "ISO2_WB_CODE",
          "ISO3_CODE", "ISO3_WB_CODE", "UN_CODE", "CRS_CODE", "MOTHER_M49_CODE",
          "M49_FAOST_CODE", "FAO_TABLE_NAME", "OFFICIAL_FAO_NAME", 
          "UNOFFICIAL1_NAME", "UNOFFICIAL2_NAME", "UNOFFICIAL3_NAME", 
          "SHORT_NAME", "ABBR_FAO_NAME_SP", "UN_YEAR_START_INFO",
          "UN_YEAR_END_INFO", "FORMER_MEMBER_INFO", "COMPOSITION_INFO", 
          "UN_STATUS_INFO")]
save(FAOcountryProfile, file = "FAOcountryProfile.RData")

FAOregionProfile <- tmp
save(FAOregionProfile, file = "FAOregionProfile.RData")

# FAOmetaTable ------------------------------------------------------------

## Groups
# urlGrp <- "http://fenix.fao.org/wds/rest/groups/faostat2/en"
urlGrp <- "http://fenixapps2.fao.org/wds/rest/groups/faostat2/en"
## urlGrp <- "http://faostat3.fao.org/wds/rest/groups/faostat2/en"
groupCode <- 
  unique(data.frame(groupCode = sapply(fromJSON(urlGrp, encoding = "UTF-8"), 
                                       function(x) x[1]),
                    groupName = sapply(fromJSON(urlGrp, encoding = "UTF-8"), 
                                       function(x) x[2]),
                    stringsAsFactors = FALSE))
## Removing "Food security", "Investments", "Emergency response"
groupCode <- groupCode[!groupCode[, "groupCode"] %in% c("D", "I", "X"),]

## Domains
# urlDom <- "http://fenix.fao.org/wds/rest/domains/faostat2/"
urlDom <- "http://fenixapps2.fao.org/wds/rest/domains/faostat2/"
## urlDom <- "http://faostat3.fao.org/wds/rest/domains/faostat2/"
base <- data.frame()
for(i in 1:NROW(groupCode)){
  tmp <- fromJSON(paste(urlDom, groupCode[i, "groupCode"], "/en", sep = ""), 
                  encoding = "UTF-8")
  tmp2 <- unique(data.frame(groupCode = groupCode[i, "groupCode"],
                            domainCode = sapply(tmp, function(x) x[1]),
                            domainName = sapply(tmp, function(x) x[2]),
                            stringsAsFactors = FALSE))
  base <- rbind(base, tmp2)
}
domainCode <- base

## Elements
base <- data.frame()
for(i in 1:NROW(domainCode)){
  tmp <- try(fromJSON(paste("http://fenixapps2.fao.org/bletchley/rest/codes/elements/faostat2/",
                            domainCode[i, "domainCode"], "/en", sep = ""), encoding = "UTF-8"))
  if(!inherits(tmp, "try-error")){
    tmp2 <- unique(data.frame(domainCode = domainCode[i, "domainCode"],
                              elementCode = sapply(tmp, function(x) x[1]),
                              elementName = sapply(tmp, function(x)
                                paste0(x[2], "(", x[3], ")")),
                              stringsAsFactors = FALSE))
    base <- rbind(base, tmp2)
  }
}
elemCode <- base

## Items
base <- data.frame()
for(i in 1:NROW(domainCode)){
   tmp <- try(fromJSON(paste("http://fenixapps2.fao.org/bletchley/rest/codes/items/faostat2/",
     domainCode[i, "domainCode"], "/en", sep = ""), encoding = "UTF-8"))
     if(!inherits(tmp, "try-error")){
        tmp2 <- unique(data.frame(domainCode = domainCode[i, "domainCode"],
                                 itemCode = sapply(tmp, function(x) x[1]),
                                 itemName = sapply(tmp, function(x) x[2]),
                      stringsAsFactors = FALSE))
        base <- rbind(base, tmp2)
     } 
}
itemCode <- base

## Items aggregated
base <- data.frame()
for(i in 1:NROW(domainCode)){
    tmp <- try(fromJSON(paste("http://fenixapps2.fao.org/bletchley/rest/codes/itemsaggregated/faostat2/",
      domainCode[i, "domainCode"], "/en", sep = ""), encoding = "UTF-8"))
    if(!inherits(tmp, "try-error")){
        tmp2 <- unique(data.frame(domainCode = domainCode[i, "domainCode"],
                                 itemCode = sapply(tmp, function(x) x[1]),
                                 itemName = sapply(tmp, function(x) x[2]),
                      stringsAsFactors = FALSE))
        base <- rbind(base, tmp2)
    } 
}
itemAggCode <- base

## Table
FAOmetaTable <- 
  list(groupTable = groupCode, domainTable = domainCode,
       itemTable = itemCode, itemAggTable = itemAggCode, elementTable = elemCode)
save(FAOmetaTable, file = "FAOmetaTable.RData")

# Building the package ----------------------------------------------------

## Remove the folder if it exists
if(file.exists("./FAOSTAT"))
    unlink("FAOSTAT", recursive = TRUE)

## Build the package
package.skeleton("FAOSTAT", code_files = paste("./Codes/",
                           dir("./Codes/", pattern = "\\.R$"), sep = ""),
                 force = FALSE)

## Include the data
dir.create("FAOSTAT/data")
file.copy(from = "./FAOcountryProfile.RData",
          to = "FAOSTAT/data/", overwrite = TRUE)
file.copy(from = "./FAOregionProfile.RData",
          to = "FAOSTAT/data/", overwrite = TRUE)
file.copy(from = "./FAOmetaTable.RData",
          to = "FAOSTAT/data/", overwrite = TRUE)
file.copy(from = "./DESCRIPTION", to = "FAOSTAT/",
          overwrite = TRUE)
unlink("./FAOSTAT/Read\\-and\\-delete\\-me")

## Include Demo
dir.create("FAOSTAT/demo")
file.copy(from = "./FAOSTATdemo.R",
          to = "FAOSTAT/demo/", overwrite = TRUE)
cat("FAOSTATdemo      Demonstration for the FAOSTAT package\n",
    file = "FAOSTAT/demo/00Index")

## Use roxygen to build the documentation
roxygenize("FAOSTAT")

## Include vignette
dir.create("./FAOSTAT/vignettes/")
dir.create("./FAOSTAT/inst/")
dir.create("./FAOSTAT/inst/doc/")
file.copy(from = "./Documentation/FAOSTAT.pdf",
          to = "./FAOSTAT/inst/doc/", overwrite = TRUE)
file.copy(from = "./Documentation/FAOSTAT.Rnw",
          to = "./FAOSTAT/vignettes/", overwrite = TRUE)

## Build and check the package
system("R CMD INSTALL --build FAOSTAT")
system("R CMD build FAOSTAT")
system("R CMD check --as-cran FAOSTAT")

###########################################################################
## End
###########################################################################
