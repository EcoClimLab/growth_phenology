# Import MASS first b/c of conflict between MASS::select() and dplyr::select()
library(MASS)
library(tidyverse)
library(RColorBrewer)
library(RDendrom)
library(zoo)



# Set location of separate Dendrobands GitHub repo. We need this to import data
# and functions
# dendro_repo <- "C:/Users/world/Desktop/Github/Dendrobands"
dendro_repo <- "~/Documents/Trees/Dendrobands/"



#-------------------------------------------------------------------------------
# This loop takes the original dendro data csv's and creates to lists:
# - all_years_intra: a list where each element corresponds to a year
# - all_stems_intra: a list where each element corresponds to a stemID
files <- dendro_repo %>%
  str_c("data/") %>%
  dir(pattern="_201[0-9]*.csv", full.names = TRUE)
dates <- c(2010:2019)

make_growth_list <- function(dirs, years) {
  #----
  # Save all data as list by year here:
  all_years_intra <- list()

  for (k in seq(along = dirs)) {
    yr_intra <- dirs[k] %>%
      read_csv() %>%
      filter(intraannual == 1) %>%
      mutate(dbh = as.numeric(dbh))

    all_years_intra[[k]] <- split(yr_intra, yr_intra$stemID)
  }
  names(all_years_intra) <- str_c("trees", years, sep = "_")

  #----
  # Save all data as list by stemID here:
  all_stems_intra <- list()

  # Get chr vector of unique stemID's
  all_stemIDs <- all_years_intra %>%
    sapply(names) %>%
    unlist() %>%
    unique() %>%
    sort()

  # Ooof
  for(stemID in all_stemIDs) {
    all_stems_intra[[paste0("stemID_", stemID)]] <- years %>%
      lapply(function(year) all_years_intra[[paste0("trees", sep="_", year)]][[stemID]]) %>%
      bind_rows()
  }

  # Attach lists to R environment
  all_years_intra <<- all_years_intra %>%
    list2env() %>%
    as.list()
  all_stems_intra <<- all_stems_intra %>%
    list2env() %>%
    as.list()
}
make_growth_list(files, dates)



#-------------------------------------------------------------------------------
# Import utility functions
#
# From Dendro GitHub repo:
# -objectiveFuncDendro()
# -findOneDendroDBH()
# -findDendroDBH()
dendro_repo %>%
  str_c("Rscripts/analysis/convert_caliper_meas_to_dbh.R") %>%
  source()

# From ???
source("Rscripts/functions_albert.R")



#-------------------------------------------------------------------------------
# Taken from Ian's Dendrobands/RScripts/analysis/growth_over_time.R
# Loop to create timeseries of dbh measurements manually

# Before we knew Sean was working on a package (RDendrom), Valentine and I tried
# to create the same functionality manually (#5 and #6 in this script). Since we
# do have RDendrom, this manual work is deprecated, but I've decided to leave it
# here in case it's needed later.

# Description of loop
# 1. First, assigns the first dbh of the growth column as the first dbh.
# 2. Second, is conditional:
# 2i If new.band=0 (no band change), we have a measure, and we have a previous
# dbh2, use Condit's function to determine next dbh2 based on caliper
# measurement.
# 2ii If new.band=0, we have a measure, and the previous dbh2 is NA, use
# Condit's function by comparing the new measure with the most recent non-NA
# dbh2.
# 2iii If new.band=0 and the previous measure is NA, give dbh2 a value of NA.
# 2iv If new.band=1 (band and measurement change), we have a measure, and
# there's a new dbh, assign that dbh to dbh2.
# 2v If new.band=1, we have a measure, and there's no new dbh (indicating a
# new dbh wasn't recorded when the band was changed), dbh2 is the sum of the
# differences of the previous dbh2's added to the most recent dbh2.
# 2vi UNCOMMON If new.band=1, measure is NA, and the dbh in the original
# column is unchanged, dbh2 is the sum of the differences of the previous
# dbh2's added to the most recent dbh2.
# 2vii UNCOMMON If new.band=1, measure is NA, and dbh is different, dbh2 is
# the new dbh plus the mean of the differences of the previous dbh2's.

for(stems in names(all_stems_intra)) {
  tree.n <- all_stems_intra[[stems]] %>%
    mutate(
      dbh2 = NA,
      dbh2 = ifelse(row_number() == 1, dbh[1], dbh2),
      dbh2 = as.numeric(dbh2),
      measure = as.numeric(measure),
      dbh = as.numeric(dbh)
    )

  # Mean difference in dendrometer readings
  q <- tree.n %>%
    group_by(dendroID) %>%
    mutate(diff = measure - lag(measure)) %>%
    ungroup() %>%
    summarize(mean_diff = mean(diff, na.rm = TRUE)) %>%
    pull(mean_diff)

  tree.n <- tree.n %>%
    mutate(
      has.measure = !is.na(measure),
      has.prev.dbh2 = !is.na(lag(dbh2)),
      new.dbh = dbh != lag(dbh)
    ) %>%
    mutate(
      dbh2_case = case_when(
        !new.band & survey.ID[i] == 2014.01 & new.dbh ~ "1",
        !new.band & has.measure & has.prev.dbh2 ~ "2i",
        !new.band & has.measure & !has.prev.dbh2 ~ "2ii",
        !new.band & !has.measure  ~ "2iii",
        new.band & has.measure & new.dbh ~ "2iv",
        new.band & has.measure & !new.dbh ~ "2v",
        new.band & !has.measure & !new.dbh ~ "2vi",
        new.band & !has.measure & new.dbh ~ "2vii"
      )
    )

  for(i in 2:(nrow(tree.n))){
    tree.n$dbh2[i] <-
      ifelse(
        # Not sure about this case
        "1",
        tree.n$dbh[i],
        ifelse(
          # If not new.band, has a measure, and has a previous dbh2:
          # use Condit's function to determine next dbh2 based on caliper measurement.
          "2i",
          findDendroDBH(tree.n$dbh2[i-1], tree.n$measure[i-1], tree.n$measure[i]),
          ifelse(
            # If not new.band, has a measure, and no previous dbh2:
            # use Condit's function by comparing the new measure with the most recent non-NA dbh2.
            "2ii",
            findDendroDBH(tail(na.locf(tree.n$dbh2[1:i-1]), n=1), tail(na.locf(tree.n$measure[1:i-1]), n=1), tree.n$measure[i]),
            ifelse(
              # If not new.band and no previous measure:
              # give dbh2 a value of NA.
              "2iii", NA,
              ifelse(
                # If new.band, has a measure, and there's a new dbh:
                # assign that dbh to dbh2.
                "2iv",
                tree.n$dbh[i],
                ifelse(
                  # If new.band, has a measure, and there's no new dbh (indicating a new dbh wasn't recorded when the band was changed):
                  # dbh2 is the sum of the differences of the previous dbh2's added to the most recent dbh2.
                  "2v",
                  max(tree.n$dbh2[1: i-1], na.rm = T) + mean(diff(tree.n$dbh2[1: i-1]), na.rm = T),
                  ifelse(
                    # UNCOMMON If new.band, no measure, and the dbh in the original column is unchanged:
                    # dbh2 is the sum of the differences of the previous dbh2's added to the most recent dbh2.
                    "2vi",
                    max(tree.n$dbh2[1: i-1], na.rm = T) + mean(diff(tree.n$dbh2[1:(i-1)]), na.rm=T),
                    ifelse(
                      # UNCOMMON If new.band, no measure, and dbh is different:
                      # dbh2 is the new dbh plus the mean of the differences of the previous dbh2's.
                      "2vii",
                      tree.n$dbh[i] + mean(diff(tree.n$dbh2[1:(i-1)]), na.rm=TRUE),
                      # else leave NA
                      tree.n$dbh2))))))))
  }
  all_stems_intra[[stems]] <- tree.n
}


# for(i in 2:(nrow(tree.n))){
#   dbh_case <- rep(FALSE, 8)
#   dbh_case[1] <- tree.n$new.band[[i]] == 0 & tree.n$survey.ID[[i]] == 2014.01 & !identical(tree.n$dbh[[i]], tree.n$dbh[[i-1]])
#   dbh_case[2] <- tree.n$new.band[[i]] == 0 & !is.na(tree.n$measure[[i]]) & !is.na(tree.n$dbh2[[i-1]])
#   dbh_case[3] <- tree.n$new.band[[i]] == 0 & !is.na(tree.n$measure[[i]]) & is.na(tree.n$dbh2[[i-1]])
#   dbh_case[4] <- tree.n$new.band[[i]] == 0 & is.na(tree.n$measure[[i]])
#   dbh_case[5] <- tree.n$new.band[[i]] == 1 & !is.na(tree.n$measure[[i]]) & !identical(tree.n$dbh[[i]], tree.n$dbh[[i-1]])
#   dbh_case[6] <- tree.n$new.band[[i]] == 1 & !is.na(tree.n$measure[[i]]) & identical(tree.n$dbh[[i]], tree.n$dbh[[i-1]])
#   dbh_case[7] <- tree.n$new.band[[i]] == 1 & is.na(tree.n$measure[[i]]) & identical(tree.n$dbh[[i]], tree.n$dbh[[i-1]])
#   dbh_case[8] <- tree.n$new.band[[i]] == 1 & is.na(tree.n$measure[[i]]) & !identical(tree.n$dbh[[i]], tree.n$dbh[[i-1]])
#
#   tree.n$dbh2[[i]] <-
#     ifelse(
#       dbh_case[1], tree.n$dbh[[i]],
#       ifelse(
#         dbh_case[2], findDendroDBH(tree.n$dbh2[[i-1]], tree.n$measure[[i-1]], tree.n$measure[[i]]),
#         ifelse(
#           dbh_case[3], findDendroDBH(tail(na.locf(tree.n$dbh2[1:i-1]), n=1), tail(na.locf(tree.n$measure[1:i-1]), n=1), tree.n$measure[[i]]),
#           ifelse(
#             dbh_case[4], NA,
#             ifelse(
#               dbh_case[5], tree.n$dbh[[i]],
#               ifelse(
#                 dbh_case[6], max(tree.n$dbh2[1: i-1], na.rm = T) + mean(diff(tree.n$dbh2[1: i-1]), na.rm = T),
#                 ifelse(
#                   dbh_case[7], max(tree.n$dbh2[1: i-1], na.rm = T) + mean(diff(tree.n$dbh2[1:(i-1)]), na.rm=T),
#                   ifelse(
#                     dbh_case[8], tree.n$dbh[i] + mean(diff(tree.n$dbh2[1:(i-1)]), na.rm=TRUE), tree.n$dbh2))))))))
# }


# tree.n %>%
#   mutate(
#     dbh2 =
#       case_when(
#         new.band == 0 & survey.ID == 2014.01 & !identical(dbh, lag(dbh)) ~ dbh,
#         new.band == 0 & !is.na(measure) & !is.na(lag(dbh2)) ~ findDendroDBH(lag(dbh2), lag(measure), measure),
#         # new.band == 0 & !is.na(measure) & is.na(lag(dbh2)) ~ findDendroDBH(tail(na.locf(dbh2[1:(row_number()-1)]), n = 1), tail(na.locf(measure[1:(row_number()-1)]), n = 1), measure),
#         new.band == 0 & is.na(measure) ~ NA_real_,
#         new.band == 1 & !is.na(measure) & !identical(dbh, lag(dbh)) ~ dbh,
#         new.band == 1 & !is.na(measure) & identical(dbh, lag(dbh)) ~ cummax(dbh2[1:(row_number()-1)], na.rm = T) + mean(diff(dbh2[1:(row_number()-1)]), na.rm = T),
#         #new.band == 1 & is.na(measure) & identical(dbh, lag(dbh)) ~ max(dbh2[1:(row_number()-1)], na.rm = T) + mean(diff(dbh2[1:(row_number()-1)]), na.rm = T),
#         #new.band == 1 & is.na(measure) & !identical(dbh, lag(dbh)) ~ dbh + mean(diff(dbh2[1:(row_number()-1)]), na.rm = TRUE),
#         TRUE ~ dbh2
#       )
#   )




#splist <- c("acru", "caca", "caco", "cagl", "caovl", "cato", "ceca", "cofl", "fagr", "fram", "juni", "litu", "nysy", "pist", "ploc", "qual", "qupr", "quru", "quve", "tiam", "ulru")
# Reformat data into a similar format as used in the example of this script ##
#for(q in 2010:2019){
#Stem <- subset(all_stems_intra[[1]], year == q) #Get original DOY where measurements were taken. Do they need to be consistent between years or would a little wiggle room be fine?
#for(w in splist){
#  Stem <- subset(Stem, sp == w)
#Stem$month <- ifelse(Stem$month == "January", 1,
#                     ifelse(Stem$month == "February", 2,
#                            ifelse(Stem$month == "March", 3,
#                                   ifelse(Stem$month == "April", 4,
#                                          ifelse(Stem$month == "May", 5,
#                                                 ifelse(Stem$month =="June", 6,
#                                                        ifelse(Stem$month == "July", 7,
#                                                               ifelse(Stem$month =="August", 8,
#                                                                      ifelse(Stem$month =="September", 9,
#                                                                             ifelse(Stem$month =="October", 10,
#                                                                                    ifelse(Stem$month =="November", 11,
#                                                                                           ifelse(Stem$month =="December", 12, Stem$month))))))))))))
#Stem$DOY <- as.Date(with(Stem, paste(year, month, day, sep="-")), "%Y-%m-%d")
#Stem$DOY <- yday(Stem$DOY)

#DOY_inDF1 <- Stem$DOY # List with original DOY. Used to make any measurement taken on days other than these = NA in DF resulting from following loop
#Stem$dbh2 <- ifelse(Stem$DOY == c(DOY_inDF1), Stem$dbh2, NA) #Added this to make any measurement not on the same DOY as the DOY i will use for analysis = NA

all_stems <- data.frame(NULL)

#Stem[NULL ,c(1:length(Stem$tag))]




for(e in 1:length(all_stems_intra)){ # Loop to merge all dendroband surveys into one dataframe
  Stem <- all_stems_intra[[e]]
  Stem$month <- ifelse(Stem$month == "January", 1,
                       ifelse(Stem$month == "February", 2,
                              ifelse(Stem$month == "March", 3,
                                     ifelse(Stem$month == "April", 4,
                                            ifelse(Stem$month == "May", 5,
                                                   ifelse(Stem$month =="June", 6,
                                                          ifelse(Stem$month == "July", 7,
                                                                 ifelse(Stem$month =="August", 8,
                                                                        ifelse(Stem$month =="September", 9,
                                                                               ifelse(Stem$month =="October", 10,
                                                                                      ifelse(Stem$month =="November", 11,
                                                                                             ifelse(Stem$month =="December", 12, Stem$month))))))))))))
  Stem$DOY <- as.Date(with(Stem, paste(year, month, day, sep="-")), "%Y-%m-%d")
  Stem$DOY <- yday(Stem$DOY)
  all_stems <- rbind(all_stems, Stem)}
all_stems <- all_stems[complete.cases(all_stems$measure),]

#Objects needed for the following loop
data <- data.frame(NULL)
throwaways <- data.frame(NULL)
check_list <- NA
DOY2 <- NULL
masterDF <- data.frame(1,2,3,4,5,6,7,8)
colnames(masterDF) <- c("tot", "perc", "tag", "DOY", "sp", "year", "dbh", "max_rate_DOY")

#The following loop will cycle through years, species, and tags to create a final "master dataframe"
#It includes two of Sean's functions wrapped with some of my code
for(q in 2012:2019){
  Stem2 <- subset(all_stems, year == q)
  for(w in unique(Stem2$sp)){ #removes trees with less than 10 measurements in each year
    Stem3 <- subset(Stem2, sp == w)
    count_df <- count(Stem3, tag)
    count_df <- subset(count_df, n >= 10)
    Stem3 <- Stem3[Stem3$tag %in% count_df$tag,]
    #original_list <- unique(Stem3$tag)
    for(m in unique(Stem3$tag)){ #remove trees with very small or negative total growth
      growthcheck <- subset(Stem3, tag == m)
      check_list <- append(check_list,(ifelse( growthcheck[nrow(growthcheck), 32] - growthcheck[1,32] <= 1.25 , unique(growthcheck$tag), NA))) #1.25 is arbitrarily chosen. I'm considering decreasing it
      check_list <- check_list[complete.cases(check_list)]
    }
    Stem3 <- Stem3[!(Stem3$tag %in% check_list),]
    skip_to_next <- FALSE #If all trees fail to meet the minimum growth criteria, an error is produced which stops the loop
    tryCatch( #Go to next species if all tags of previous species failed the growth check
    Stem3$tag_stem <- paste0(Stem3$tag, sep = "_", Stem3$stemtag) #add a unique tag for each stem of trees with multiple stems
    ,error = function(b) { skip_to_next <<- TRUE})
    if(skip_to_next) { next } #If you know how to make this display a warning when it encounters an error i'd like to add that eventually
for(e in unique(Stem3$tag_stem)){

  data <- data.frame(NULL)

  #spline_test <- subset(Stem3, tag_stem == e) #check for outliers/mistakes in the data
  #fit1 <- smooth.spline(x = spline_test$DOY, y = spline_test$measure ,df = 4)
  #plot(spline_test$measure ~ spline_test$DOY)
  #lines(fit1)
  #spline_test$splinepred <- fit1[["y"]]
  #spline_test$dummy <- ifelse(abs(spline_test$splinepred - spline_test$measure) >= 7, 1, 0)
  #throwaways <- rbind(throwaways, spline_test[(spline_test$dummy == 1),])
  #stemstag <- spline_test[(spline_test$dummy == 0),]
  #stemstag <- stemstag[,c(-34,-35)]
  #rm(spline_test)

  stemstag <- subset(Stem3, tag_stem == e)
  #stemstag <- mutate(stemstag, dif = measure-lag(measure))

  data <- rbind(data, stemstag$dbh2)

  colnames(data) <- paste0("X", stemstag$DOY) #This is the format that Sean's functions use

#tags <- unique(stemsDF$tag)
#day <- subset(stemstag, tag == 132539)
#treeone <- data[1,]
#DOY <- data.frame(sort(unique(stemsDF$DOY)))
#DOY <- mutate(DOY, dif = sort.unique.stemsDF.DOY..-lag(sort.unique.stemsDF.DOY..))
#DOY[1,2] <- 0
#for(i in 1:length(unique(stemsDF$DOY))){
#  DOY[i,1] <- ifelse(DOY[i,2] <= 3 & DOY[i,2] >= 1, DOY[i-1,1], DOY[i,1])
#}
#DOY <- unique(DOY$sort.unique.stemsDF.DOY..)
#colnames(data) <- paste0("X", DOY)

### R code from vignette source 'MODEL_FIT_NEW_revised.Rnw'
### Encoding: UTF-8


###################################################
### code chunk number 9: lg5.model.runs
###################################################

# The list/unlist is done so that we can see the values attributed to the
# parameters and then make them numeric so that the optim function can use them
# as a numeric vector.


##  STARTING PARAMETERS AND MIN AND MAX VALUES FOR THE L-BFGS-B CALLS

  #This is the beginning of the first of Sean's functions - I can try to help with questions about this but parts of it are over my head
dbh.data <- data
tree.no <- dim(dbh.data)[1]
doy.full <- get.doy(dbh.data)
lg5.hess <- vector("list", tree.no)
winning.optim.call <- c()
optim.output.df <- c()
resids.mat <- matrix(NA, tree.no, length(doy.full))
#pdf("FIGURES/lg5_fit_all.pdf")
#par(mfrow = c(2,2))
pb <- txtProgressBar(style = 3)
for(r in 1:tree.no) { #Sean's first function
  setTxtProgressBar(pb, r / tree.no, title = NULL, label = NULL)
  par(mfrow = c(1,1))
  #	pdf(file = sprintf("FIGURES/lg5_fit_%i.pdf", i))
  dbh <- as.numeric(dbh.data[r,])

  complete <- complete.cases(dbh)
  dbh <- dbh[complete]
  doy <- doy.full[complete]
  doy.ip.hat <- doy[(which(dbh > mean(dbh)))[1]]
  par.list <- list(L = min(dbh, na.rm = TRUE), K = max(dbh, na.rm = TRUE), doy.ip = doy.ip.hat, r = .08, theta = 1)
  params <- as.numeric(unlist(par.list))
  params.start <- params
  optim.min <- c((min(dbh, na.rm = TRUE) * 0.99), quantile(dbh, 0.5, na.rm = TRUE), 0, 0, 0.01)

  optim.max <- c(min(dbh, na.rm = TRUE), max(dbh, na.rm = TRUE), 350, 0.1, 15)
  resid.sd <- 0.1
  hess.tmp <- vector("list", 6)
  ##  THESE ARE THE CALLS TO OPTIM  ##
  # weigted values have false ML estimates, so the estimate is re-assessed based on the optimized parameters in an unweighted call


#  tryCatch(
  lg5.output.LB <- optim(par = params, doy = doy, dbh = dbh, resid.sd = resid.sd, fn = lg5.ML, method = "L-BFGS-B", lower = optim.min, upper = optim.max, hessian = TRUE, control = list(trace = 0))
#  , error = function(e) { skip_to_next <<- TRUE})

#  if(skip_to_next) { next }
  hess.tmp[[1]] <- lg5.output.LB$hessian
  params <- lg5.output.LB$par

  lg5.output.LB.wt <- optim(par = params, doy = doy, dbh = dbh, resid.sd = resid.sd, fn = lg5.ML.wt, method = "L-BFGS-B", lower = optim.min, upper = optim.max, hessian = TRUE, control = list(trace = 0))
  lg5.output.LB.wt$value <- lg5.ML(params = lg5.output.LB.wt$par, doy, dbh, resid.sd = resid.sd)
  hess.tmp[[2]] <- lg5.output.LB.wt$hessian

  lg5.output.NM <- optim(par = params, fn = lg5.ML, resid.sd = resid.sd,  method = "Nelder-Mead", hessian = TRUE, control = list(trace = 0), doy = doy, dbh = dbh)
  hess.tmp[[3]] <- lg5.output.NM$hessian

  lg5.output.NM.wt <- optim(par = params, fn = lg5.ML.wt, resid.sd = resid.sd,  method = "Nelder-Mead", hessian = TRUE, control = list(trace = 0), doy = doy, dbh = dbh)
  lg5.output.NM.wt$value <- lg5.ML(lg5.output.NM.wt$par, doy, dbh, resid.sd = resid.sd)
  hess.tmp[[4]] <- lg5.output.NM.wt$hessian

  lg5.output.SANN <- optim(par = params, doy = doy, dbh = dbh, resid.sd = resid.sd, fn = lg5.ML, method = "SANN",  hessian = TRUE, control = list(maxit = 30000, trace = F))
  hess.tmp[[5]] <- lg5.output.SANN$hessian

  lg5.output.SANN.wt <- optim(par = params, doy = doy, dbh = dbh, resid.sd = resid.sd, fn = lg5.ML.wt, method = "SANN",  hessian = TRUE, control = list(maxit = 30000, trace = F))
  hess.tmp[[6]] <- lg5.output.SANN.wt$hessian

  lg5.output.SANN.wt$value <- lg5.ML(lg5.output.SANN.wt$par, doy, dbh, resid.sd = resid.sd)


  ## CONSOLIDATE THE RESULTS  ##

  optim.output <- rbind(c(params.start, NA), c(lg5.output.LB$par, lg5.output.LB$value), c(lg5.output.LB.wt$par, lg5.output.LB.wt$value), c(lg5.output.NM$par, lg5.output.NM$value), c(lg5.output.NM.wt$par, lg5.output.NM.wt$value), c(lg5.output.SANN$par, lg5.output.SANN$value), c(lg5.output.SANN.wt$par, lg5.output.SANN.wt$value))

  winner <- rep(".", length = 7)
  winner[1] <- NA
  winner[which(optim.output[,6] == min(optim.output[-1,6], na.rm = T))] <- "*"

  calls <- c("Starting", "L-BFGS-B", "L-BFGS-B wt", "N-M", "N-M wt", "SANN", "SANN wt")

  ##  MAKE A DATAFRAME OF THE RESULTS  ##
  optim.output.tmp <- round(optim.output, digits = 4)
  optim.output.df <- as.data.frame(rbind(optim.output.df, cbind(r, calls, optim.output.tmp, winner)))


  #############################
  lg5.hess[[r]] <- hess.tmp[which(winner == "*")]
  winner <- match(min(optim.output[-1, 6], na.rm = T), optim.output[-1, 6])

  win.vec <- rep(2, 6)
  win.vec[winner] <- 1
  cols <- brewer.pal(dim(optim.output)[1], name = "Dark2")
  if(r == 1) {
    optim.output1 <- optim.output

    win.vec1 <- win.vec
    optim.output.df.1 <- optim.output.df[, -1]
    names(optim.output.df.1) <- c("Optim.call", "L", "K", "doy_ip", "r", "theta", "ML", "Best.ML")
    doy1 <- doy
    dbh1 <- dbh
  }
  plot(doy, dbh, xlab = "Day of the year", ylab = "DBH (cm)", pch = 19, col = "gray15", main = sprintf("Annual Growth for tree %i", i), cex = 1)

  days <- seq(365)
  for(t in 2:dim(optim.output)[1]) {
    lines(days, lg5.pred(params = optim.output[t ,], doy = days), col = cols[t - 1], lty = win.vec[t - 1], lwd = 1)
  }

  legend("bottomright", legend = calls[-1], col = cols, lwd = 2, lty = win.vec)


  #	dev.off()
  winning.optim.call[r] <- c("L-BFGS-B", "L-BFGS-B wt", "N-M", "N-M wt", "SANN", "SANN wt")[winner]
  resids.mat[r, complete] <- get.lg5.resids(params = lg5.output.LB.wt$par, doy, dbh)
}
close(pb)
dev.off()


names(optim.output.df) <- c("Tree.no", "Optim.call", "L", "K", "doy_ip", "r", "theta", "ML", "Best.ML")
#write.csv(optim.output.df, file = "Optim_output.csv", quote = FALSE, row.names = FALSE)


winner.tab <- table(winning.optim.call)
winner.tab


###################################################
### code chunk number 10: upper.lower.bounds
###################################################

# The list/unlist is done so that we can see the values attributed to the
# parameters and then make them numeric so that the optim function can use them
# as a numeric vector.

dbh.data <- data
tree.no <- dim(dbh.data)[1]
doy.full <- get.doy(dbh.data)
Param.df <- as.data.frame(array(dim = c(tree.no, 7)))

deviation.val <- c(0.01)
seq.l <- 200

start.d <- matrix(NA, tree.no, 4)
end.d <- matrix(NA, tree.no, 4)


#pdf("FIGURES/HI_LO_fit_all.pdf")
#par(mfrow = c(2,2))

for(y in 1:tree.no) { #Sean's second function -- same deal
  print(y)

  #	par(mfrow = c(1,1))
  #	pdf(file = sprintf("FIGURES/Low_up_%i.pdf", i))

  dbh <- as.numeric(dbh.data[y,])

  complete <- complete.cases(dbh)
  dbh <- dbh[complete]
  doy <- doy.full[complete]
  doy.ip.hat <- doy[(which(dbh > mean(dbh)))[1]]
  par.list <- list(L = min(dbh, na.rm = TRUE), K = max(dbh, na.rm = TRUE), doy.ip = doy.ip.hat, r = 0.08, theta = 1)
  params <- as.numeric(unlist(par.list))

  optim.min <- c((min(dbh, na.rm = TRUE) * 0.99), quantile(dbh, 0.5, na.rm = TRUE), 0, 0, 0.01)

  optim.max <- c(min(dbh, na.rm = TRUE), max(dbh, na.rm = TRUE), 350, 0.1, 15)
  resid.sd <- 0.1

  ##  THESE ARE THE CALLS TO OPTIM  ##


  lg5.output.LB <- optim(par = params, doy = doy, dbh = dbh, resid.sd = resid.sd, fn = lg5.ML, method = "L-BFGS-B", lower = optim.min, upper = optim.max, hessian = TRUE, control = list(trace = 0))
  params <- lg5.output.LB$par

  win.optim.call <- winning.optim.call[y]
  if(win.optim.call == "L-BFGS-B wt") {
    hi.lo.output <- optim(par = params, doy = doy, dbh = dbh, resid.sd = resid.sd, fn = lg5.ML.wt, method = "L-BFGS-B", hessian = TRUE, control = list(trace = 0))
  }else{
    hi.lo.output <- optim(par = params, doy = doy, dbh = dbh, resid.sd = resid.sd, fn = lg5.ML.wt, method = "Nelder-Mead", hessian = TRUE, control = list(trace = 0))
  }

  params.tmp <- hi.lo.output$par
  ## Now call the boundary functions
#  tryCatch(
  start.d[y ,] <- start.diam(params = params.tmp, seq.l = seq.l,  doy = doy, dbh, deviation.val = deviation.val, figure = FALSE, resid.sd) #Both start and end estimates are returning 200 numbers instead of 1
#  error = function(e) { skip_to_next <<- TRUE})
#  if(skip_to_next) { next }
#  tryCatch(
  end.d[y, ] <- end.diam(params = params.tmp, seq.l = seq.l, doy = doy, dbh, deviation.val = deviation.val, figure = FALSE, resid.sd)  #fix this ... dev.val too small
#  error = function(e) { skip_to_next <<- TRUE})
#if(skip_to_next) { next }
  a <- c(start.d[y, 1], end.d[y, 1])

  #If you want to see the plooted model, remove the '#' in the following two lines:
  #plot(doy, dbh, xlab = "Day of the year", ylab = "DBH (cm)", pch = 19, col = "gray15", main = sprintf("Annual Growth for tree %i", i), cex = 1)
  #lines(days, lg5.pred.a(a, params = params.tmp, doy = days, asymptote = "both"), col = "darkred", lty = 1, lwd = 1)
  Param.df[y, 6:7] <- a

  Param.df[1, 1:5] <- params.tmp

  #	dev.off()
}

#dev.off()


names(Param.df) <- c("L", "K", "doy_ip", "r", "theta", "a", "b")
#write.csv(Param.df, file = "SCBI_hi_lo_lg5.csv", quote = FALSE, row.names = FALSE)


#Max growth rate DOY
maxDOY <- NULL
###################################################
### code chunk number 13: day_values
###################################################
params <- Param.df[1,]
start.day <- round(pred.doy(params, params$a))
stop.day <- round(pred.doy(params, params$b))
.deriv <- lg5.deriv(paras = params, days, growth = params$b - params$a)
fifty.day <- round(pred.doy(params, mean(c(params$a, params$b))))

#for(i in 1:nrow(Param.df)){
#  max <- max.growth.day(Param.df[i,])
#  maxDOY <- append(maxDOY, max)
#}
max <- max.growth.day(Param.df[1,]) #

#maxDF <- data.frame(1:length(maxDOY), maxDOY)
#hist(maxDF$maxDOY, breaks = 11)
#mean(maxDF$maxDOY)
#sd(maxDF$maxDOY)

#Growth over the year
#lg5.pred <- function(params, doy)
curveDF <- data.frame(NULL)
endcurveDF <- data.frame(c(60:274)) ###################### Change start and end date here
pb <- txtProgressBar(style = 3)
#for( u in 1:nrow(Param.df)){
#  setTxtProgressBar(pb, u / nrow(Param.df), title = NULL, label = NULL)
 growth_curve <- Param.df[1,]
   for(i in 60:274){ ##################################### Change period of data here
     prediction <- lg5.pred(growth_curve, i)
     curveDF <- rbind(curveDF, prediction)

   }
 #colnames(curveDF) <- paste0("Tree", u)
 curveDF <- mutate(curveDF, dif = curveDF[,1]-lag(curveDF[,1]))

 endcurveDF <- cbind(curveDF, endcurveDF)
 endcurveDF <- endcurveDF[complete.cases(endcurveDF),]
 #curveDF <- data.frame(NULL)

#plotting each tree
seq <- seq(2, ncol(endcurveDF), 2)
growth_rate <- endcurveDF[,seq]
#plot.ts(growth_rate, plot.type = "single")

#plotting mean growth for trees in DF
#means <- data.frame(rowMeans(differences))
#means <- data.frame(means[-1,])
#max <- subset(means, means$means..1... == max(means$means..1...))
#plot(means$means..1...)

#25%, 50%, 75% intervals
#growthvariables <- data.frame(NULL)
#for(o in 1:ncol(differences)){
  tempdifferences <- data.frame(growth_rate[complete.cases(growth_rate)])#data.frame(differences[2:nrow(differences),i])
  colnames(tempdifferences) <- "dif"
  tempdifferences$addition <- cumsum(tempdifferences$dif) #make a rolling total of the differences
#  tryCatch(
  tempdifferences$tot <- sum(tempdifferences$dif)  #Find the total change within the year. Error produced when params not assigned to params.df
#  error = function(e) { skip_to_next <<- TRUE})
#  if(skip_to_next) { next }
  tempdifferences$perc <- NA #create the row where percentages will be appended
  for(p in c(.25,.50,.75)){ #Loop to find when 25%, 50%, and 75% of yearly growth occurred
    try <- which(abs(tempdifferences$addition-tempdifferences$tot*p)==min(abs(tempdifferences$addition-tempdifferences$tot*p))) #find where the absolute value of the rolling total subtracted by the value we want (total*.25, .50, or .75) is closest to zero
    tempdifferences$perc[try] <- p #assign the number to the value found above

  }
  tempdifferences <- tempdifferences[complete.cases(tempdifferences),]

  #tempdifferences$tree <- paste0("tree", i)
  tempdifferences$tag <- e ####
  tempdifferences$DOY <- paste0(row.names(tempdifferences))
  if(sum(tempdifferences$perc) == 1.5){ #ifelse to remove instances where .25, .50, and .75 values could not be found. Some years were returning multiples of each percentage or only 2 out of the 3 percentages.
    tempdifferences <- tempdifferences
  }else{
    tempdifferences <- NULL
  }

#  growthvariables <- rbind(growthvariables, tempdifferences)
#}

phenology_DF <- tempdifferences[,3:6]
phenology_DF$sp <- w
phenology_DF$year <- q
phenology_DF$dbh <- median(stemstag$dbh2)
#max_rate_DOY <- subset(endcurveDF, endcurveDF$dif == max(endcurveDF$dif))
phenology_DF$max_rate_DOY <- max
tryCatch(
masterDF <- rbind(masterDF, phenology_DF)
,error = function(e) { skip_to_next <<- TRUE})
if(skip_to_next) { next }
rm(growth_rate)
rm(endcurveDF)
rm(Param.df)
rm(data)
}

    }}
warnings()
masterDF <- masterDF[-1,]
masterDF$wood_type <- ifelse(masterDF$sp == "quru" | masterDF$sp == "qual" , "ring porous", ifelse(masterDF$sp == "litu"|masterDF$sp == "fagr", "diffuse-porous", "other"))
write.csv(masterDF, file = "Wood_pheno_table_V1.csv", row.names = FALSE)

for(p in 1:ncol(endcurveDF)){ #Loop to cycle through trees in predicted dbh DF
  #for (i in c(2011:2019)){ #Loop to cycle through years within a stem
    #Stem1 <- subset(Stem, Stem$year == i)
    #Stem1 <- mutate(Stem1, dif = dbh2-lag(dbh2)) #calculates the differences between subsequent measures
    #Stem1[1,33] <- 0 #make the first measure 0 instead of NA
    #Stem1$dif <- ifelse(abs(Stem1$dif) >= 10, 0, Stem1$dif) #remove any measure greater than 10mm. From what I could tell, these were mostly caused by band replacements or mistakes in data entry of measurement.
    #Stem1$addition <- cumsum(Stem1$dif) #make a rolling total of the differences
    #Stem1$tot <- sum(Stem1$dif) #Find the total change within the year
    #Stem1$perc <- NA #create the row where percentages will be appended
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
###################################################
### code chunk number 11: sparse.data
###################################################
samp.output <- list()
ct <- 0
for(i in (length(doy) - 9):1) {
  ct <- ct + 1
  doy.samp <- c(floor(seq(1, 33, length.out = 34 - i)))
  doy <- doy1[doy.samp]
  dbh <- dbh1[doy.samp]

  doy.ip.hat <- doy[(which(dbh > mean(dbh)))[1]]
  par.list <- list(L = min(dbh, na.rm = TRUE), K = max(dbh, na.rm = TRUE), doy.ip = doy.ip.hat, r = 0.08, theta = 1)
  params <- as.numeric(unlist(par.list))

  optim.min <- c((min(dbh, na.rm = TRUE) * 0.99), quantile(dbh, 0.5, na.rm = TRUE), 0, 0, 0.01)

  optim.max <- c(min(dbh, na.rm = TRUE), max(dbh, na.rm = TRUE), 350, 0.1, 15)
  resid.sd <- 0.1

  ##  THESE ARE THE CALLS TO OPTIM  ##


  lg5.output.LB <- optim(par = params, doy = doy, dbh = dbh, resid.sd = resid.sd, fn = lg5.ML, method = "L-BFGS-B", hessian = TRUE, control = list(trace = 0))
  params <- lg5.output.LB$par

  samp.output[[ct]] <- optim(par = params, doy = doy, dbh = dbh, resid.sd = resid.sd, fn = lg5.ML, method = "Nelder-Mead", hessian = TRUE, control = list(trace = 0))
  samp.output[[ct]]$n <- length(doy)
}
samp.n <- c()
result.samp <- vector("list", 5)
for(rs in 1:5) {
  result.samp[[rs]]$par <- matrix(NA, 4, length(samp.output))
}
for(so in 1:length(samp.output)) {
  var.tmp <- 2 * sqrt(diag(ginv(samp.output[[so]]$hessian)))
  par.tmp <- samp.output[[so]]$par
  samp.n <- samp.output[[so]]$n
  for(p in 1:length(par.tmp)) {
    result.samp[[p]]$par[, so] <- rbind(par.tmp[p], (par.tmp[p] - var.tmp[p]), (par.tmp[p] + var.tmp[p]), samp.n)

  }
}


###################################################
### code chunk number 12: extensions
###################################################
# Extended functions for inference


# This function predicts a diameter given the day of the year and a vector of parameters for the lg5 model.
# It is called by the lg5.ss and lg5.plot functions.







###################################################
### code chunk number 14: outerHull
###################################################

#pdf("figures/deficit_plot.pdf", width = 7, height = 10)

layout(matrix(c(1,1,1,1,2,2,3,3), ncol = 2, byrow = TRUE))
QH.list <- vector("list", dim(Param.df)[1])
WD.sum <- vector(length = dim(Param.df)[1])
D.sum <- vector(length = dim(Param.df)[1])
RGR <- vector(length = dim(Param.df)[1])
GR <- vector(length = dim(Param.df)[1])
Size <- vector(length = dim(Param.df)[1])

for(t in 1:dim(Param.df)[1]) {
  params <- Param.df[t, ]
  dbh <- as.numeric(dbh.data[t, ])
  doy.full <- get.doy(dbh.data)
  QH.list[[t]] <- fit.outer.hull(dbh, doy.full, params)
  OH.list <- QH.list[[t]]
  doyP2 <- OH.list$doyP2
  dbhP2 <- OH.list$dbhP2
  doyP <- OH.list$doyP
  dbhP <- OH.list$dbhP
  OH.fit <- OH.list$OH.fit

  D.sum[t] <- sum(OH.list$Deficit)
  WD.sum[t] <- sum(OH.list$Weighted.deficit)
  RGR[t] <- as.numeric(log(params$b) - log(params$a))
  GR[t] <- as.numeric(params$b - params$a)
  Size[t] <- params$a
  # Figure out start and stop values and days ...
  start.d <- start.diam(params = as.numeric(params), seq.l = seq.l,  doy = doy, dbh, deviation.val = deviation.val, figure = FALSE, resid.sd)
  end.d <- end.diam(params = as.numeric(params), seq.l, doy, dbh, deviation.val, figure = FALSE, resid.sd)
  asym <- c(start.d[1], end.d[1])

}


###################################################
### code chunk number 15: comp.results.table
###################################################
# xtable(optim.output.df.1,
# 	digits = 4, caption = "Results from model runs using the 5-parameter
# 	Logistic Function. Parameter values are listed beside the optimization method.", label = "tab1")
write.csv(optim.output.df.1, file = "Optim_output.csv", quote = FALSE, row.names = FALSE)

###################################################
### code chunk number 16: data_fig
###################################################
plot(doy.1, dbh.1, xlab = "Day of the year", ylab = "DBH (cm)", pch = 18,
     col = "tomato", main = "Cumulative annual growth")


###################################################
### code chunk number 17: four_examples
###################################################
par(mfrow = c(2,2), mar = c(4,4,2,2))
trees <- c(1, 5, 9, 17)
for(ct in 1:length(trees)) {
  t <- trees[ct]
  cex.val <- 0.8
  dbh <- as.numeric(dbh.data[t,])
  params.tmp <- as.numeric(Param.df[t, ])
  params.out.tmp <- subset(optim.output.df, Tree.no == t, select = -c(Tree.no, Optim.call, Best.ML))

  params.out <- as.matrix(params.out.tmp)
  complete <- complete.cases(dbh)
  dbh <- dbh[complete]
  doy <- doy.full[complete]
  cols <- brewer.pal(dim(optim.output)[1], name = "Dark2")

  plot(doy, dbh, xlab = "Day of the year", ylab = "DBH (cm)", pch = 19, col = "gray15",
       main = sprintf("Annual Growth for tree %i", t), cex = 0.8, cex.axis = cex.val,
       cex.lab = cex.val, cex.main = cex.val)

  days <- seq(365)
  for(j in 2:dim(params.out)[1]) {
    lines(days, lg5.pred(params = as.numeric(params.out[j ,]), doy = days),
          col = cols[j - 1], lty = 2, lwd = 0.75)
  }
  a <- params.tmp[6:7]
  lines(days, lg5.pred.a(a, params = params.tmp, doy = days, asymptote = "both"),
        col = "darkred", lty = 1.5, lwd = 1)
  legend("topleft", legend = sprintf("%s)", letters[ct]), bty = "n",
         cex = cex.val, inset)
  legend("bottomright", legend = c(calls[-1], "Lo-Hi"), col = c(cols, "darkred"),
         lty = c(rep(2, 6), 1), lwd = 1, cex = 0.5)
}



###################################################
### code chunk number 18: plot_residuals
###################################################
par(mfrow = c(7, 1), mar = c(1,4,1,1))
doy <- doy1
dbh <- dbh1
for(i in 2:(dim(optim.output)[1])){
  params <- as.numeric(optim.output1[i,])
  lg5.resids <- get.lg5.resids(params, doy, dbh)
  plot(doy, lg5.resids, type = "b", col = "tomato", xlab = "Day of the Year",
       pch = 19, ylab = sprintf("%s", calls[i]))
  abline(h = 0, col = "darkgrey")
}
mean.resids <- apply(resids.mat, MAR = 2,  mean, na.rm = TRUE)
sd.resids <- 1.96 * (apply(resids.mat, MAR = 2,  sd, na.rm = TRUE)  / sqrt(20))

plot(doy1, mean.resids, col = "steelblue", type = "b", pch = 19, ylab = "All trees")
segments(doy1, mean.resids - sd.resids, doy1, mean.resids + sd.resids,
         col = "steelblue", lty = 1, lwd = 1.5)
abline(h = 0, col = "darkgray")


###################################################
### code chunk number 19: sparse_fig
###################################################
par.name <- c("L", "K", "doy.ip", "r", "theta")
par(mfrow = c(5,1), oma = c(1,1,1,1), mar = c(4,3,2,2))
for(rs in 1:length(result.samp)){
  samp.data <- result.samp[[rs]]$par
  if(rs == 3) {
    y.lim <-  c(150, 250)
  } else {
    y.lim <- range(samp.data[2:3,])
  }
  plot(samp.data[4,], samp.data[3,], main = sprintf("%s", par.name[rs]),
       ylab = "Estimate", xlab = ifelse(rs == 5, "Data points", ""),
       col = "white", ylim = y.lim)
  polygon(c(samp.data[4,], rev(samp.data[4,])), c(samp.data[2,],
                                                  rev(samp.data[3,])), col = "lightblue")
  lines(samp.data[4,], samp.data[1,], col = "darkred", type = "b", pch = 19)
}



###################################################
### code chunk number 20: quantile_hull
###################################################

#pdf("FIGURES/All_CH.pdf")
layout(matrix(c(1,1,1,1,2,2,3,3,4,4), ncol = 2, byrow = TRUE))
for(t in 1) { #:dim(Param.df)[1]) {
  params <- Param.df[t, ]
  dbh <- as.numeric(dbh.data[t, ])
  doy.full <- get.doy(dbh.data)
  doy <- doy.full[complete.cases(dbh)]
  dbh <- dbh[complete.cases(dbh)]
  OH.list <- fit.outer.hull(dbh, doy, params, quant = 0.8)

  doyP2 <- OH.list$doyP2
  dbhP2 <- OH.list$dbhP2
  doyP <- OH.list$doyP
  dbhP <- OH.list$dbhP
  OH.fit <- OH.list$OH.fit
  # TODO: figure out start and stop values ...
  start.d <- start.diam(params = as.numeric(params), seq.l = seq.l,  doy = doy, dbh, deviation.val = deviation.val, figure = FALSE, resid.sd)
  end.d <- end.diam(params = as.numeric(params), seq.l, doy, dbh, deviation.val, figure = FALSE, resid.sd)
  asym <- c(start.d[1], end.d[1])

  plot(doy, dbh, xlab = "", ylab = "DBH (cm)", pch = 19, col = "gray15", main = sprintf("Annual Growth for tree %i", t), cex = 1)
  points(doyP2, dbhP2, pch = 19, col = "tomato")
  days <- seq(365)
  lines(days, lg5.pred(params = OH.fit$par, doy = days), col = cols[1], lty = 1, lwd = 1)
  #lines(days, lg5.pred.a(asym, params = OH.fit$par, doy = days, asymptote = "upper"), col = cols[1], lty = 1, lwd = 1)
  lines(days, lg5.pred.a(asym, params = as.numeric(params), doy = days), col = cols[2], lty = 2, lwd = 1)
  legend("bottomright", lty = c(1,2), col = cols[1:2], legend = c("Quantile Hull", "ML fit"))
  text(110, 55.2, labels = "a)")

  plot(doyP, OH.list$Deficit, pch = 19, type = "b", xlim = range(doy), col = "steelblue", xlab = "", ylab = "Deficit")
  abline(h = 0)
  text(110, min(OH.list$Deficit), "b)", pos = 3)

  plot(doyP, OH.list$Weighted.deficit, pch = 19, type = "b", xlim = range(doy), col = "steelblue", xlab = "", ylab = "Weighted deficit")
  abline(h = 0)
  text(110, min(OH.list$Weighted.deficit), "c)", pos = 3)
  ##
}
precip.data <- read.csv("WaterBalance.csv", header = TRUE)
plot(precip.data$doy, precip.data$cum.NET.3.1, col = "orange", type = "l",
     xlim = range(doy), pch = 19, xlab = "Day of the Year", ylab = "Water balance (mm)")
text(110, min(precip.data$cum.NET.3.1), "d)", pos = 3)







#Extra code
### removed from reformat section
for(j in 2011:2019)

  Stem_year <- subset(Stem, year == 2012)


}
Stem <- subset(all_stems_intra[[1]], year == 2019)
Stem$month <- ifelse(Stem$month == "January", 1,
                     ifelse(Stem$month == "February", 2,
                            ifelse(Stem$month == "March", 3,
                                   ifelse(Stem$month == "April", 4,
                                          ifelse(Stem$month == "May", 5,
                                                 ifelse(Stem$month =="June", 6,
                                                        ifelse(Stem$month == "July", 7,
                                                               ifelse(Stem$month =="August", 8,
                                                                      ifelse(Stem$month =="September", 9,
                                                                             ifelse(Stem$month =="October", 10,
                                                                                    ifelse(Stem$month =="November", 11,
                                                                                           ifelse(Stem$month =="December", 12, Stem$month))))))))))))
Stem$DOY <- as.Date(with(Stem, paste(year, month, day, sep="-")), "%Y-%m-%d")
Stem$DOY <- yday(Stem$DOY)
colnames(all_stems) <- paste0("X", Stem$DOY)

complete2 <- all_stems[complete.cases(all_stems),]

#removed from original script
###################################################
### code chunk number 5: data
###################################################
doy.1 <- c(112, 120, 124, 130, 134, 139,
           144, 148, 153, 158, 165, 172, 179, 187, 190, 193, 200, 207, 214, 217, 221,
           225, 228, 232, 237, 244, 251, 256, 263, 270, 285, 291, 301)
dbh.1 <- c(18.99449, 18.99512, 19.00085, 19.01008, 19.03077, 19.06038, 19.07406,
           19.08584, 19.12659, 19.17465, 19.19821, 19.24150, 19.27619, 19.30579,
           19.30898, 19.34113, 19.37105, 19.38919, 19.39365, 19.41243, 19.41911,
           19.43471, 19.43726, 19.44076, 19.44553, 19.44458, 19.44521, 19.44903,
           19.44872, 19.45254, 19.45731, 19.45890, 19.45890)
