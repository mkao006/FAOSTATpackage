########################################################################
## Simple script for building the FAOSTAT package
########################################################################

library(roxygen2)
library(RJSONIO)
library(utils)

## NOTE (FILIPPO): run the command
## Sys.setenv(R_GSCMD='"C:/Program Files/gs/gs9.07/bin/gswin32c.exe"')
## before the script.

## FAOcountryProfile data
## ---------------------------------------------------------------------

tmp = read.csv(file = "FAOcountryProfileUTF8.csv", header = TRUE,
    stringsAsFactors = FALSE, na.string = "", encoding = "UTF-8")
tmp[tmp == " "] = NA

## Sort and select column names by class
cty = grep("_CODE$|_NAME$", colnames(tmp), value = TRUE)
cty = cty[-grep("OLD|REG", cty)]
FAOcountryProfile = tmp[, cty]

## Save
save(FAOcountryProfile, file = "FAOcountryProfile.RData")



## FAOregionProfile data
## ---------------------------------------------------------------------


## Sort and select column names by class
FAOregionProfile = tmp[, c("FAOST_CODE", "UN_CODE",
    grep("_REG", colnames(tmp), value = TRUE))]

## Save
save(FAOregionProfile, file = "FAOregionProfile.RData")



# ## FAOmetaTable
# ## ---------------------------------------------------------------------
# 
# 
# urlGrp = "http://fenix.fao.org/wds/rest/groups/faostat2/en"
# 
# ## NOTE (FILIPPO): removing the first row we are removing Production
# ## group.
# # groupCode = unique(data.frame(groupCode = sapply(fromJSON(urlGrp,
# #                                   encoding = "UTF-8"), function(x) x[1]),
# #                               groupName = sapply(fromJSON(urlGrp,
# #                                   encoding = "UTF-8"), function(x) x[2]),
# #                               stringsAsFactors = FALSE))[-1, ]
# 
# groupCode = unique(data.frame(groupCode = sapply(fromJSON(urlGrp,
#                                                           encoding = "UTF-8"), function(x) x[1]),
#                               groupName = sapply(fromJSON(urlGrp,
#                                                           encoding = "UTF-8"), function(x) x[2]),
#                               stringsAsFactors = FALSE))
# 
# 
# urlDom = "http://fenix.fao.org/wds/rest/domains/faostat2/Q/en"
# 
# # ## The wds call is different to the rest, so the first row is removed.
# # base = data.frame()
# # for(i in 1:NROW(groupCode)){
# #     tmp = fromJSON(paste("http://fenix.fao.org/wds/rest/domains/faostat2/",
# #         groupCode[i, "groupCode"], "/en", sep = ""), encoding = "UTF-8")
# #     tmp2 = unique(data.frame(groupCode = groupCode[i, "groupCode"],
# #                              domainCode = sapply(tmp, function(x) x[1]),
# #                              domainName = sapply(tmp, function(x) x[2]),
# #                   stringsAsFactors = FALSE))[-1, ]
# #     base = rbind(base, tmp2)
# # }
# # domainCode = base
# ## NOTE (FILIPPO): removing the first row we remove Crops
# base = data.frame()
# for(i in 1:NROW(groupCode)){
#   tmp = fromJSON(paste("http://fenix.fao.org/wds/rest/domains/faostat2/",
#                        groupCode[i, "groupCode"], "/en", sep = ""), encoding = "UTF-8")
#   tmp2 = unique(data.frame(groupCode = groupCode[i, "groupCode"],
#                            domainCode = sapply(tmp, function(x) x[1]),
#                            domainName = sapply(tmp, function(x) x[2]),
#                            stringsAsFactors = FALSE))
#   base = rbind(base, tmp2)
# }
# domainCode = base
# 
# 
# base = data.frame()
# for(i in 1:NROW(domainCode)){
#    tmp = try(fromJSON(paste("http://fenix.fao.org/bletchley/rest/codes/items/faostat2/",
#      domainCode[i, "domainCode"], "/en", sep = ""), encoding = "UTF-8"))
#      if(!inherits(tmp, "try-error")){
#         tmp2 = unique(data.frame(domainCode = domainCode[i, "domainCode"],
#                                  itemCode = sapply(tmp, function(x) x[1]),
#                                  itemName = sapply(tmp, function(x) x[2]),
#                       stringsAsFactors = FALSE))
#         base = rbind(base, tmp2)
#      } 
# }
# itemCode = base
# 
# 
# base = data.frame()
# for(i in 1:NROW(domainCode)){
#     tmp = try(fromJSON(paste("http://fenix.fao.org/bletchley/rest/codes/itemsaggregated/faostat2/",
#       domainCode[i, "domainCode"], "/en", sep = ""), encoding = "UTF-8"))
#     if(!inherits(tmp, "try-error")){
#         tmp2 = unique(data.frame(domainCode = domainCode[i, "domainCode"],
#                                  itemCode = sapply(tmp, function(x) x[1]),
#                                  itemName = sapply(tmp, function(x) x[2]),
#                       stringsAsFactors = FALSE))
#         base = rbind(base, tmp2)
#     } 
# }
# itemAggCode = base
# 
# 
# base = data.frame()
# for(i in 1:NROW(domainCode)){
#     tmp = try(fromJSON(paste("http://fenix.fao.org/bletchley/rest/codes/elements/faostat2/",
#       domainCode[i, "domainCode"], "/en", sep = ""), encoding = "UTF-8"))
#     if(!inherits(tmp, "try-error")){
#         tmp2 = unique(data.frame(domainCode = domainCode[i, "domainCode"],
#                                  elementCode = sapply(tmp, function(x) x[1]),
#                                  elementName = sapply(tmp, function(x)
#                                      paste0(x[2], "(", x[3], ")")),
#                       stringsAsFactors = FALSE))
#         base = rbind(base, tmp2)
#     }
# }
# elemCode = base
# 
# FAOmetaTable = list(groupTable = groupCode, domainTable = domainCode,
#                     itemTable = itemCode, itemAggTable = itemAggCode,
#                     elementTable = elemCode)
# 
# ## Save
# save(FAOmetaTable, file = "FAOmetaTable.RData")



## Building the package
## ---------------------------------------------------------------------

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
## file.copy(from = "./CITATION", to = "FAOSTAT/inst/CITATION",
##           overwrite = TRUE)
unlink("./FAOSTAT/inst/", recursive = TRUE)

## Include vignette
dir.create("./FAOSTAT/vignettes/")
file.copy(from = "./Documentation/FAOSTAT.pdf",
          to = "./FAOSTAT/vignettes/", overwrite = TRUE)


## Create the vignette hack from (http://yihui.name/knitr/demo/vignette/)
## This is not required for R 3.0.0
## cat("%\\VignetteIndexEntry{General Manual}\n\\documentclass{article}\n\\begin{document}\n\\end{document}",
##     file = "./FAOSTAT/inst/doc/FAOSTAT.Rnw")

## Build and check the package
system("R CMD INSTALL --build FAOSTAT")
system("R CMD build FAOSTAT")
system("R CMD check --as-cran FAOSTAT")

