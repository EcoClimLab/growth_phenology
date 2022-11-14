##########################################################
## Purpose: Functions for filtering QA flags for script scbiPhenQA.R
## Creator: Ian McGregor, imcgreg@ncsu.edu, for Dow et al., 2022
## Corresponding author: Kristina Anderson-Teixeira, teixeirak@si.edu
## System: R Version 4.1.1
##########################################################
library(data.table)

# Step 1: Filter phenology data from MCD12Q2
## -----------------------------------------------------------------------------
## checkQAall = keep rows that have overall valid observations
## checkQAdetailed = go through detailed QA bits and filter (keep "good" or "best")
## filterPhenology = format the data and filter by QA overall and QA detailed
##
## MCD12Q2: https://developers.google.com/earth-engine/datasets/catalog/MODIS_006_MCD12Q2#bands
## -----------------------------------------------------------------------------
checkQAall <- function(row, colName="", dt=phen){
  qaVal <- as.numeric(dt[row, get(colName)])
  keepRow <- ifelse(qaVal > 1, FALSE, TRUE)
  return(keepRow)
}
checkQAdetailed <- function(row, colName="", targetCols="", dt=phen){
  dat <- dt[row, ]
  qaVal <- as.numeric(dt[row, get(colName)])
  
  # is this cycle 1 or 2?
  cycle <- gsub("QA_Detailed_", "", colName)
  
  # get the column numbers whose values we are checking
  colsNum <- which(grepl(cycle, colnames(dt)))[1:7]
  
  # bit N (N) = bits are counted from 0 [bit N], but R counts the vector from 1 (N)
  # if there is a 1...  
  ## in bit 0 (1), greenup is bad
  ## in bit 2 (3), midgreenup is bad
  ## in bit 4 (5), maturity is bad
  ## in bit 6 (7), peak is bad
  ## in bit 8 (9), senescence is bad
  ## in bit 10 (11), midgreendown is bad
  ## in bit 12 (13), dormancy is bad
  
  # go through each of the bits and see if we want to filter out
  ## this function comes from the MCD12Q2 user guide pg 8
  ## https://modis-land.gsfc.nasa.gov/pdf/MCD12Q2_Collection6_UserGuide.pdf
  unpackedDetailedQA <- function(X){
    bits <- as.integer(intToBits(X))
    quals <- sapply(seq(1,16,by=2), function(i) sum(bits[i:(i+1)] * 2^c(0,1)))[1:7]
    return(quals)
  }
  
  ## we only want to keep best (0) or good (1) quality
  qa <- unpackedDetailedQA(qaVal)
  out <- ifelse(qa < 2, TRUE, FALSE)
  
  # replace the false values with NA (i.e. this is our way of filtering)
  if(any(which(out==FALSE))){
    dat[, colsNum[which(out==FALSE)]] <- NA
  }
  
  return(dat)
}
filterPhenology <- function(filePath="scbi/scbiPhen.csv", saveFile=TRUE, 
                            savePath="scbi/scbiPhenFiltered.csv"){
  phen <- fread(filePath) # Read in the data
  
  # Step 1: Remove columns that are all NA (to remove unnecessary cols)
  colNA <- sapply(colnames(phen), function(X) return(all(is.na(phen[,get(X)]))))
  colNA <- colnames(phen)[colNA]
  phen[, c(colNA) := NULL]
  
  # Step 2: Filter by QA flags
  ## note we are keeping "good" and "best" quality observations
  
  ## Step 2A: Remove bad rows for QA_Overall
  qaCols <- colnames(phen)[grepl("QA", colnames(phen))]
  qaRowsAll <- sapply(1:nrow(phen), checkQAall, colName=qaCols[1], dt=phen)
  phen <- phen[qaRowsAll, ]
  
  ## Step 2B: Now remove bad observations from QA_Detailed
  targetCols <- colnames(phen)[grepl("_1", colnames(phen))][1:7]
  qaDetailedIndex <- lapply(1:nrow(phen), checkQAdetailed, colName=qaCols[2], dt=phen)
  phenFiltered <- rbindlist(qaDetailedIndex)
  
  if(saveFile) fwrite(phenFiltered, savePath)
  
  return(phenFiltered)
}

################################################################################
# Step 2: Filter phenology data from MCD43A4 / MCD43A2
## -----------------------------------------------------------------------------
## filterEVI2 = remove observations that are missing, and those that have bad QA values
##
## Note:
## The QA flags for MCD43A4 state that 0 is good quality (with full inversion), 
## while 1 indicates a variety of possibilities as indicated in the QA flags 
## from MCD43A2. As the QA flags match, and we are primarily concerned with good 
## and best quality, we only need to keep the QA flags of 0 for the two bands 
## used to calculate EVI2.
##
## MCD43A4: https://developers.google.com/earth-engine/datasets/catalog/MODIS_006_MCD43A4#bands
## MCD43A2: https://developers.google.com/earth-engine/datasets/catalog/MODIS_006_MCD43A2
## -----------------------------------------------------------------------------
filterEVI2 <- function(filePath="scbi/scbiEVI2New.csv", saveFile=TRUE, savePath="scbi/scbiEVI2New.csv"){
  dt <- fread(filePath) # Read in the data
  
  # Remove days that have no observation
  dt <- dt[complete.cases(dt), ]
  
  # Remove days that have a QA of 1 in either band. (i.e. want best quality)
  dt <- dt[BRDF_Albedo_Band_Mandatory_Quality_Band1 == 0 &
             BRDF_Albedo_Band_Mandatory_Quality_Band2 == 0, ]
  
  if(saveFile) fwrite(dt, savePath)
  
  return(dt)
}
