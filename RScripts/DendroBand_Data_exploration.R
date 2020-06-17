######################################################
# Purpose: Informal script to explore the SCBI dendroband data
# Developed by: Cameron Dow - DowC@si.edu
# R studio version 1.1.456 - First created June 2020, updated 
######################################################

# 
library(plyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(RDendrom)
library(zoo)
library(lubridate)
#Taken from: Github/Dendrobands/Rscripts/analysis/growth_over_time.R . 
#format dendroband data ####
# You'll have to set your WD to the dendroband repo in your github desktop. Your setwd command will look different than mine #
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

# convert measure to DBH (Use function in Dendrobands/Rscripts/analysis/convert_caliper_meas_to_dbh.R (I pasted it below for now))
#Caliper to DBH function by Ian##
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

################################################################################
#Taken from Ian's growth_over_time.R ##
#5. loop to create timeseries of dbh measurements manually ####

##before we knew Sean was working on a package (RDendrom), Valentine and I tried to create the same functionality manually (#5 and #6 in this script). Since we do have RDendrom, this manual work is deprecated, but I've decided to leave it here in case it's needed later.
## *** to run this section, you need to run section #1 above first. ****

#description of loop ####
##1. First, assigns the first dbh of the growth column as the first dbh.
##2. Second, is conditional:
##2i.If new.band=0 (no band change), we have a measure, and we have a previous dbh2, use Condit's function to determine next dbh2 based on caliper measurement. 
##2ii. If new.band=0, we have a measure, and the previous dbh2 is NA, use Condit's function by comparing the new measure with the most recent non-NA dbh2.
##2iii. If new.band=0 and the previous measure is NA, give dbh2 a value of NA.
##2iv. If new.band=1 (band and measurement change), we have a measure, and there's a new dbh, assign that dbh to dbh2.
##2v. If new.band=1, we have a measure, and there's no new dbh (indicating a new dbh wasn't recorded when the band was changed), dbh2 is the sum of the differences of the previous dbh2's added to the most recent dbh2.
##2vi. UNCOMMON If new.band=1 , measure is NA, and the dbh in the original column is unchanged , dbh2 is the sum of the differences of the previous dbh2's added to the most recent dbh2.
##2vii. UNCOMMON If new.band=1, measure is NA, and dbh is different, dbh2 is the new dbh plus the mean of the differences of the previous dbh2's. 

##intraannual data ####
for(stems in names(all_stems_intra)) {
  tree.n <- all_stems_intra[[stems]]
  tree.n$dbh2 <- NA
  tree.n$dbh2[1] <- tree.n$dbh[1]
  
  tree.n$dbh2 <- as.numeric(tree.n$dbh2)
  tree.n$measure <- as.numeric(tree.n$measure)
  tree.n$dbh <- as.numeric(tree.n$dbh)
  
  q <- mean(unlist(tapply(tree.n$measure, tree.n$dendroID, diff)), na.rm=TRUE)
  
  for(i in 2:(nrow(tree.n))){
    tree.n$dbh2[[i]] <- 
      
      ifelse(tree.n$new.band[[i]] == 0 & tree.n$survey.ID[[i]] == 2014.01 & !identical(tree.n$dbh[[i]], tree.n$dbh[[i-1]]),
             tree.n$dbh[[i]],
             
             ifelse(tree.n$new.band[[i]] == 0 & !is.na(tree.n$measure[[i]]) & !is.na(tree.n$dbh2[[i-1]]),
                    findDendroDBH(tree.n$dbh2[[i-1]], tree.n$measure[[i-1]], tree.n$measure[[i]]),
                    
                    ifelse(tree.n$new.band[[i]] == 0 & !is.na(tree.n$measure[[i]]) & is.na(tree.n$dbh2[[i-1]]), 
                           findDendroDBH(tail(na.locf(tree.n$dbh2[1:i-1]), n=1), tail(na.locf(tree.n$measure[1:i-1]), n=1), tree.n$measure[[i]]),
                           
                           ifelse(tree.n$new.band[[i]] == 0 & is.na(tree.n$measure[[i]]), NA,
                                  
                                  ifelse(tree.n$new.band[[i]]==1 & !is.na(tree.n$measure[[i]]) & !identical(tree.n$dbh[[i]], tree.n$dbh[[i-1]]),
                                         tree.n$dbh[[i]],
                                         
                                         ifelse(tree.n$new.band[[i]] == 1 & !is.na(tree.n$measure[[i]]) & identical(tree.n$dbh[[i]], tree.n$dbh[[i-1]]),
                                                max(tree.n$dbh2[1: i-1], na.rm = T) + mean(diff(tree.n$dbh2[1: i-1]), na.rm = T),
                                                
                                                ifelse(tree.n$new.band[[i]] == 1 & is.na(tree.n$measure[[i]]) & identical(tree.n$dbh[[i]], tree.n$dbh[[i-1]]),
                                                       max(tree.n$dbh2[1: i-1], na.rm = T) + mean(diff(tree.n$dbh2[1:(i-1)]), na.rm=T),
                                                       
                                                       ifelse(tree.n$new.band[[i]] == 1 & is.na(tree.n$measure[[i]]) & !identical(tree.n$dbh[[i]], tree.n$dbh[[i-1]]),
                                                              tree.n$dbh[i] + mean(diff(tree.n$dbh2[1:(i-1)]), na.rm=TRUE),
                                                              tree.n$dbh2))))))))
  }
  all_stems_intra[[stems]] <- tree.n
}



#Loop to pull out survey where 25%, 50%, and 75% of total growth were achieved ##
#Run these four items before the loop

Stem <- all_stems_intra[[1]]
End <- data.frame()
numbers <- c(.25,.50,.75)

for(p in 1:length(all_stems_intra)){ #Loop to cycle through stems in all_stems_intra
  for (i in c(2011:2019)){ #Loop to cycle through years within a stem
    Stem1 <- subset(Stem, Stem$year == i)
    Stem1 <- mutate(Stem1, dif = dbh2-lag(dbh2)) #calculates the differences between subsequent measures
    Stem1[1,33] <- 0 #make the first measure 0 instead of NA
    Stem1$dif <- ifelse(abs(Stem1$dif) >= 10, 0, Stem1$dif) #remove any measure greater than 10mm. From what I could tell, these were mostly caused by band replacements or mistakes in data entry of measurement.
    Stem1$addition <- cumsum(Stem1$dif) #make a rolling total of the differences
    Stem1$tot <- sum(Stem1$dif) #Find the total change within the year
    Stem1$perc <- NA #create the row where percentages will be appended
    for(j in c(.25,.50,.75)){ #Loop to find when 25%, 50%, and 75% of yearly growth occured
      try <- which(abs(Stem1$addition-Stem1$tot*j)==min(abs(Stem1$addition-Stem1$tot*j))) #find where the absolute value of the rolling total subtracted by the value we want (total*.25, .50, or .75) is closest to zero
      Stem1$perc[try] <- j #assign the number to the value found above
      
    }
    Final <- Stem1[complete.cases(Stem1[ ,36]),] #remove all rows except those with a value assigned 
    if(sum(Final$perc) == 1.5){ #ifelse to remove instances where .25, .50, and .75 values could not be found. Some years were returning multiples of each percentage or only 2 out of the 3 percentages.
      Final <- Final
    }else{
      Final <- NULL
    }
    End <- rbind(End, Final) #bind the loop DF to the end DF
    End$tot <- ifelse(End$tot < 0, NA, End$tot)
    End <- End[complete.cases(End[ ,35]),]
  }
  Stem <- all_stems_intra[[p+1]]
}
#End of Loop will give you: Error in all_stems_intra[[p + 1]] : subscript out of bounds. This is expected, it's ok to move on. End dataframe should now be full of all trees from the intraannual DF
#Check that .25, .50, and .75 occured an equal number of times
count(End, perc)

#assign DOY to each percentage
#Change character dates to numeric
End$month <- ifelse(End$month == "January", 1, 
                    ifelse(End$month == "February", 2,
                           ifelse(End$month == "March", 3,
                                  ifelse(End$month == "April", 4,
                                         ifelse(End$month == "May", 5,
                                                ifelse(End$month =="June", 6,
                                                       ifelse(End$month == "July", 7,
                                                              ifelse(End$month =="August", 8,
                                                                     ifelse(End$month =="September", 9,
                                                                            ifelse(End$month =="October", 10,
                                                                                   ifelse(End$month =="November", 11,
                                                                                          ifelse(End$month =="December", 12, End$month))))))))))))
End$DOY <- as.Date(with(End, paste(year, month, day, sep="-")), "%Y-%m-%d")
End$DOY <- yday(End$DOY)


#Start analysis
#25% growth
End25 <- subset(End, perc == .25)
Means25 <- aggregate(DOY~year, data = End25, FUN = "mean")
plot(Means25$DOY~Means25$year, xlab = "Year", ylab = "Mean Day of Year", main = "Mean DOY where 25% growth was achieved")

abline(lm(Means25$DOY~Means25$year))
plot(lm(Means25$DOY~Means25$year), 5) #2012 is an outlier year, remove it for this but come back to it later?

Means25 <- Means25[-2,]
plot(Means25$DOY~Means25$year, xlab = "Year", ylab = "Mean Day of Year", main = "Mean DOY where 25% growth was achieved")

abline(lm(Means25$DOY~Means25$year))

#50% growth
End50 <- subset(End, perc == .50)
Means50 <- aggregate(DOY~year, data = End50, FUN = "mean")
plot(Means50$DOY~Means50$year, xlab = "Year", ylab = "Mean Day of Year", main = "Mean DOY where 50% growth was achieved")

abline(lm(Means50$DOY~Means50$year))
plot(lm(Means50$DOY~Means50$year), 5) #2019 is an outlier somehow? 
Means50 <- Means50[-9,]
plot(Means50$DOY~Means50$year, xlab = "Year", ylab = "Mean Day of Year", main = "Mean DOY where 50% growth was achieved")

abline(lm(Means50$DOY~Means50$year))

#75% growth
End75 <- subset(End, perc == .75)
Means75 <- aggregate(DOY~year, data = End75, FUN = "mean")
plot(Means75$DOY~Means75$year, xlab = "Year", ylab = "Mean Day of Year", main = "Mean DOY where 75% growth was achieved")

abline(lm(Means75$DOY~Means75$year))
plot(lm(Means75$DOY~Means75$year),5) #again, 2019 is an outlier? I don't understand why.
Mean75 <- Means75[-9,]
plot(Means75$DOY~Means75$year, xlab = "Year", ylab = "Mean Day of Year", main = "Mean DOY where 75% growth was achieved")

abline(lm(Means75$DOY~Means75$year))

#To do:
#Growth rates
#Incorporate leaf phenology data. NEON tower and ground observation.
#Compare ring diffuse / ring porous
#Reduce everything down to species level
#Compare years with differing environmental factors (precip, drought/drought timing?, heat)
#Average growth in spring months between years? -> temps / precip trends?
#Larger trees vulnerable to drought?
#O'rangeville used max growth from previous year as starting DBH
#Extra code ##

dbh1 = c(100, 200, 300, 100, 200, 300)
m1 = c(0, 0, 0, 0, 0, 0)
m2 = c(2, 2, 2, 50, 50, 50)
dbh2 = findDendroDBH(dbh1, m1, m2)
data.frame(dbh1, m1, m2, dbh2)


dbh2 = findDendroDBH(Stem$dbh, Stem[seq(1,nrow(Stem),1),13], Stem[seq(2,nrow(Stem),1),13])

dbh2 = findDendroDBH(Stem[1,22], Stem[1,13], Stem[2,13])
dbh3 = findDendroDBH(dbh2, Stem[2,13], Stem[3,13])
debrah <- unique(Stem$dbh)



Stem1$perc <- ifelse(Stem1$addition >= Stem1$tot*.20 & Stem1$addition < Stem1$tot*.3, 25, ifelse(Stem1$addition > Stem1$tot*.4 & Stem1$addition < Stem1$tot*.6, 50, ifelse(Stem1$addition > Stem1$tot*.70 & Stem1$addition < Stem1$tot*.8, 75, 0)))
try <- which(abs(Stem1$addition-Stem1$tot*.25)==min(abs(Stem1$addition-Stem1$tot*.25)))
Stem1$perc[try] <- 25

try <- which(abs(Stem1$addition-Stem1$tot*.5)==min(abs(Stem1$addition-Stem1$tot*.5)))
Stem1$perc[try] <- 50
try <- which(abs(Stem1$addition-Stem1$tot*.75)==min(abs(Stem1$addition-Stem1$tot*.75)))
Stem1$perc[try] <- 75

#This one works as far as I can tell ##
#This for loop will change the DBH column in DF: Stem to DBH measures based on caliper measurements ##
# It acheives this by using Stem[i,22] (Stem$DBH) as DBH1, Stem[i,13] as Meausure 1, and Stem[i+1,13] as Measure 2
for(i in c(1:nrow(Stem))){
  Stem[i+1,22]<- findDendroDBH(Stem[i,22], Stem[i,13],Stem[i+1,13])
}

# If you look at tree 192244 (and others), you'll notice the DBH was remeasured at some point.
#I want to add something to check if the DBH was remeasured, and if it was, use the new DBH measurement as DBH1 in Ian's function ##
# there is also a problem when the band is replaced. If you look at tree 192244 you'll notice the band was replaced before the 5/18/2017 measurement, leading to M1 in the previous calculation to = 134.34 while M2 = 11.24. Id like to add another check in the for loop to account for this ##
for(i in c(1:nrow(Stem))){
  Stem[i+1,22]<- findDendroDBH(Stem[i,22], #Maybe this should check if the DBH was remeasured since the initial measure, and if it was, it should use the measured DBH instead of calculated? 
                               ifelse(Stem[i+1,23] == 0, Stem[i,13], Stem[i+1,13]),                #If the band was replaced, shift M1 and M2 down one. Not a perfect solution but it'll be close enough for now. What do you think?
                               ifelse(Stem[i+1,23] == 0, Stem[i+1,13], Stem[i+2,13]))              # Same deal as above
}


# Graph the DBH ##
Stem <- Stem[,complete.cases(1)]
Stem$DOY <- as.Date(with(Stem, paste(year, month, day, sep="-")), "%Y-%m-%d")
Stem$DOY <- yday(Stem$DOY)
Stem <- mutate(Stem, dif = DOY-lag(DOY))
for(i in (2011:2020)){
  Stem1 <- subset(Stem, year == i)
  Stem1 <- paste0("Stem_", i)
}
ggplot(Stem1, aes(x=DOY, y=dbh)) + geom_line(color = "#0c4c8a")+ labs(title = unique(Stem$tag))

test_intra$DOY <- as.Date(with(test_intra, paste(YEAR, month, day, sep="-")), "%Y-%m-%d")
test_intra$DOY <- yday(test_intra$DOY)