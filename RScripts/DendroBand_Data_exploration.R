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

#Compare max growth timing ##
library("plyr", lib.loc="~/R/win-library/3.5")
library("dplyr", lib.loc="~/R/win-library/3.5")
Stem <- all_stems_intra[[1]]
# convert measure to DBH (Use function in Dendrobands/Rscripts/analysis/convert_caliper_meas_to_dbh.R (I pasted it below for now))

#This one works as far as I can tell ##
for(i in c(1:nrow(Stem))){
  Stem[i+1,22] <- findDendroDBH(Stem[i,22], Stem[i,13],Stem[i+1,13])
}
#I want to add something to check if the band was replaced, and if it was, use the new DBH measurement but this isn't working ##
for(i in c(1:nrow(Stem))){
  Stem[i+1,22] <- findDendroDBH(Stem[ifelse(Stem$new.band == 0, Stem[i,22], Stem[i+1,22]),22], Stem[i,13],Stem[i+1,13])
}
# heres another way I tried ##
for(i in c(1:nrow(Stem))){
  Stem[i+1,22] <- findDendroDBH(ifelse(Stem$new.band != 0, Stem[i+1,22], Stem[i,22]), Stem[i,13],Stem[i+1,13])
}

End <- data.frame()
#For loop to pull out survey where 25%, 50%, and 75% of total growth were achieved ##
for (i in c(2011:2020)){
  Stem1 <- subset(Stem, Stem$year == i)
  Stem1 <- mutate(Stem1, dif = measure-lag(measure))
  Stem1[1,32] <- 0
  Stem1$dif <- ifelse(abs(Stem1$dif) >= 10, 0, Stem1$dif)
  Stem1$addition <- cumsum(Stem1$dif)
  Stem1$tot <- sum(Stem1$dif)
  Stem1$perc <- NA
  for(j in c(.25,.50,.75)){
  try <- which(abs(Stem1$addition-Stem1$tot*j)==min(abs(Stem1$addition-Stem1$tot*j)))
  Stem1$perc[try] <- j
  }
  Final <- Stem1[complete.cases(Stem1[ ,35]),]
  
  End <- rbind(End, Final)
  End$tot <- ifelse(End$tot < 0, NA, End$tot)
  End <- End[complete.cases(End[ ,34]),]
}

#To do:
#calculate DOY, plot DOY on Y and year as factor on X then calculate LM for relationship ##
#Edit for loop to cycle through all trees ##
#Figure out how to handle years with multiple occurences of .25, .5. or .75 point ##



#Caliper to DBH function ##
objectiveFuncDendro= function(diameter2,diameter1,gap1,gap2){
  if(gap1>diameter1) return(20)
  if(gap2>diameter2) return(20)
  
  delta=abs(diameter1 - diameter2 + (1/pi) * diameter2 * asin(gap2/diameter2) - (1/pi) * diameter1 * asin(gap1/diameter1))
  
  return(return(delta))
}

findOneDendroDBH= function(dbh1,m1,m2,func=objectiveFuncDendro){
  if(is.na(dbh1)|is.na(m1)|is.na(m2)|dbh1<=0) return(NA)
  
  if(m2>0) upper=dbh1+m2
  else upper=dbh1+1
  if(m2<m1) lower=0
  else lower=dbh1
  
  result=optimize(f=func,interval=c(lower,upper),diameter1=dbh1,gap1=m1,gap2=m2)
  return(result$minimum)
}

findDendroDBH= function(dbh1,m1,m2,func=objectiveFuncDendro){
  records=max(length(dbh1),length(m1),length(m2))
  
  if(length(dbh1)==1) dbh1=rep(dbh1,records)
  if(length(m1)==1) m1=rep(m1,records)
  if(length(m2)==1) m2=rep(m2,records)
  
  dbh2=numeric()
  for(i in 1:records) dbh2[i]=findOneDendroDBH(dbh1[i],m1[i],m2[i],func)
  return(dbh2)
}
 ###






dbh1 = c(100, 200, 300, 100, 200, 300)
m1 = c(0, 0, 0, 0, 0, 0)
m2 = c(2, 2, 2, 50, 50, 50)
dbh2 = findDendroDBH(dbh1, m1, m2)
data.frame(dbh1, m1, m2, dbh2)


dbh2 = findDendroDBH(Stem$dbh, Stem[seq(1,nrow(Stem),1),13], Stem[seq(2,nrow(Stem),1),13])

dbh2 = findDendroDBH(Stem[1,22], Stem[1,13], Stem[2,13])
dbh3 = findDendroDBH(dbh2, Stem[2,13], Stem[3,13])
debrah <- unique(Stem$dbh)


#Extra code ##
Stem1$perc <- ifelse(Stem1$addition >= Stem1$tot*.20 & Stem1$addition < Stem1$tot*.3, 25, ifelse(Stem1$addition > Stem1$tot*.4 & Stem1$addition < Stem1$tot*.6, 50, ifelse(Stem1$addition > Stem1$tot*.70 & Stem1$addition < Stem1$tot*.8, 75, 0)))
try <- which(abs(Stem1$addition-Stem1$tot*.25)==min(abs(Stem1$addition-Stem1$tot*.25)))
Stem1$perc[try] <- 25

try <- which(abs(Stem1$addition-Stem1$tot*.5)==min(abs(Stem1$addition-Stem1$tot*.5)))
Stem1$perc[try] <- 50
try <- which(abs(Stem1$addition-Stem1$tot*.75)==min(abs(Stem1$addition-Stem1$tot*.75)))
Stem1$perc[try] <- 75