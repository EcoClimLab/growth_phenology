##########################################################
## Purpose: Filter satellite data via QAQC flags from Google Earth Engine
## Creator: Ian McGregor, imcgreg@ncsu.edu, for Dow et al., 2022
## Corresponding author: Kristina Anderson-Teixeira, teixeirak@si.edu
## System: R Version 4.1.1
##########################################################

## -----------------------------------------------------------------------------
# Structure of code
#
# There are 2 sections here representing the different GEE products used. First,
# MCD12Q2 v6 gives dates of phenometrics for the two study regions (please see
# https://developers.google.com/earth-engine/datasets/catalog/MODIS_006_MCD12Q2#bands
# for details and descriptions of the QA flags).
#
# Second, MCD43A4 gives daily band values for our two study regions (please see
# https://developers.google.com/earth-engine/datasets/catalog/MODIS_006_MCD43A4#bands
# for details). Note that the full QA flag descriptions are included in a separate
# product, MCD43A2 (https://developers.google.com/earth-engine/datasets/catalog/MODIS_006_MCD43A2).
#
# Finally, the tables (scbiPhen and scbiEVI2) used were generated from GEE directly. 
# Please see this link for the code: https://code.earthengine.google.com/0b14ff8145db942d54f4ceb1f2bcddc0
# and this link for the asset: https://code.earthengine.google.com/?asset=users/imcgreg/forestGEOsites
## -----------------------------------------------------------------------------
source("scbi/scbiPhenQA_functions.R")

phenFile <- "scbi/scbiPhen.csv"
phenFileSave <- "scbi/scbiPhenFiltered.csv"
evi2File <- "scbi/scbiEVI2.csv"
evi2FileSave <- "scbi/scbiEVI2Filtered.csv"

# 1. Filter phenology data from MCD12Q2
phen <- filterPhenology(filePath=phenFile, saveFile=TRUE, savePath=phenFileSave)

# 2. Filter EVI2 data calculated from MCD43A4
evi2 <- filterEVI2(filePath=evi2File, saveFile=TRUE, savePath=evi2FileSave)





