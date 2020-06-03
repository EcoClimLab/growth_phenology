######################################################
# Purpose: Determine growth over time from dendroband measurements
# Developed by: Ian McGregor - mcgregori@si.edu
# R version 3.5.2 - First created March 2018, updated August 2019
######################################################

#1. format dendroband data ####
setwd("C:/Users/world/Desktop/Github/Dendrobands")
files <- dir("data", pattern="_20[1-2][0-9]*.csv")
dates <- c(2010:2020)

#1a. this loop breaks up each year's dendroband trees into separate dataframes by stemID 
##grouping by intraannual ####

make_growth_list <- function(dirs, years){
  all_years_intra <- list()
  for (k in seq(along=dirs)){
    file <- dirs[[k]]
    yr <- read.csv(paste0("data/", file), stringsAsFactors = FALSE)
    yr_intra <- yr[yr$intraannual==1, ]
    yr_intra$dbh <- as.numeric(yr_intra$dbh)
    
    all_years_intra[[k]] <- split(yr_intra, yr_intra$stemID)
    #  if (file == dirs[[1]]){
    #   all_years[[k]] <- split(yr, yr$stemID)
    #  }
    #  else{
    #    all_years[[k]] <- split(yr_intra, yr_intra$stemID)
    #  }
  }
  tent_name <- paste0("trees", sep="_", years)
  names(all_years_intra) <- tent_name
  
  #the below loop takes all the unique stemIDs from each year and rbinds them.
  all_stems_intra <- list()
  
  for(stemID in sort(unique(unlist(sapply(all_years_intra, names))))) {
    all_stems_intra[[paste0("stemID_", stemID)]] <- do.call(rbind, lapply(years, function(year) all_years_intra[[paste0("trees", sep="_", year)]][[stemID]]))
  }
  
  intra_years <- list2env(all_years_intra)
  all_years_intra <<- as.list(intra_years)
  
  intra_stems <- list2env(all_stems_intra)
  all_stems_intra <<- as.list(intra_stems)
}

make_growth_list(files, dates)
#######
# Graph the dendroband measures by surveyID ##
Stem <- all_stems_intra[[1]]
plot(Stem[,13]~as.factor(Stem[,3]), main = unique(Stem[,1]))
all_stems_intra <- all_stems_intra[-1]
