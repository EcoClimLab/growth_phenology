# Setup ------------------------------------------------------------------------
#
# This script takes as input the SCBI dendroband measurements and fits Sean McMahon's LG5
# dendrometer model for each tree-year. The outputs are two .csv files of RAW
#
# 1. wood phenology DOY25, DOY50, DOY75 data
# 2. raw fitted parameters and related values (max rate, max rate DOY, etc.)
#
# which we will then "clean" for outliers in data_cleaning.R.
#
# To get a quick overview of the sections of this code, go to RStudio menu bar ->
# Edit -> Folding -> Collapse all.
#

library(data.table)
library(plyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(RDendrom)
library(zoo)
library(lubridate)
library(RColorBrewer)
library(MASS)

# Load Sean McMahon's Dendroband functions
source("RScripts/dendroband_functions.R")




# Format dendroband data ----------------------------------------------------
# files <- dir("data", pattern="_201[0-9]*.csv")
files <- dir("data", pattern = "_20[1-2][0-9]*.csv")
dates <- c(2011:2020)

# 1a. this loop breaks up each year's dendroband trees into separate dataframes by stemID
# grouping by intraannual

make_growth_list <- function(dirs, years) {
  all_years_intra <- list()
  for (k in seq(along = dirs)) {
    file <- dirs[[k]]
    yr <- read.csv(paste0("data/", file), stringsAsFactors = FALSE)
    yr_intra <- yr[yr$intraannual == 1, ]
    yr_intra$dbh <- as.numeric(yr_intra$dbh)

    all_years_intra[[k]] <- split(yr_intra, yr_intra$stemID)
    #  if (file == dirs[[1]]){
    #   all_years[[k]] <- split(yr, yr$stemID)
    #  }
    #  else{
    #    all_years[[k]] <- split(yr_intra, yr_intra$stemID)
    #  }
  }
  tent_name <- paste0("trees", sep = "_", years)
  names(all_years_intra) <- tent_name

  # the below loop takes all the unique stemIDs from each year and rbinds them.
  all_stems_intra <- list()

  for (stemID in sort(unique(unlist(sapply(all_years_intra, names))))) {
    all_stems_intra[[paste0("stemID_", stemID)]] <- do.call(rbind, lapply(years, function(year) all_years_intra[[paste0("trees", sep = "_", year)]][[stemID]]))
  }

  intra_years <- list2env(all_years_intra)
  all_years_intra <<- as.list(intra_years)

  intra_stems <- list2env(all_stems_intra)
  all_stems_intra <<- as.list(intra_stems)
}

make_growth_list(files, dates)





# Loop to create timeseries of dbh measurements manually -----
# Taken from Ian's growth_over_time.R

# before we knew Sean was working on a package (RDendrom), Valentine and I tried
# to create the same functionality manually (#5 and #6 in this script). Since we
# do have RDendrom, this manual work is deprecated, but I've decided to leave it
# here in case it's needed later. To run this section, you need to run section
# #1 above first.

# description of loop
## 1. First, assigns the first dbh of the growth column as the first dbh.
## 2. Second, is conditional:
## 2i.If new.band=0 (no band change), we have a measure, and we have a previous dbh2, use Condit's function to determine next dbh2 based on caliper measurement.
## 2ii. If new.band=0, we have a measure, and the previous dbh2 is NA, use Condit's function by comparing the new measure with the most recent non-NA dbh2.
## 2iii. If new.band=0 and the previous measure is NA, give dbh2 a value of NA.
## 2iv. If new.band=1 (band and measurement change), we have a measure, and there's a new dbh, assign that dbh to dbh2.
## 2v. If new.band=1, we have a measure, and there's no new dbh (indicating a new dbh wasn't recorded when the band was changed), dbh2 is the sum of the differences of the previous dbh2's added to the most recent dbh2.
## 2vi. UNCOMMON If new.band=1 , measure is NA, and the dbh in the original column is unchanged , dbh2 is the sum of the differences of the previous dbh2's added to the most recent dbh2.
## 2vii. UNCOMMON If new.band=1, measure is NA, and dbh is different, dbh2 is the new dbh plus the mean of the differences of the previous dbh2's.
## intraannual data
for (stems in names(all_stems_intra)) {
  tree.n <- all_stems_intra[[stems]]
  tree.n$dbh2 <- NA
  tree.n$dbh2[1] <- tree.n$dbh[1]

  tree.n$dbh2 <- as.numeric(tree.n$dbh2)
  #tree.n$measure <- ifelse(tree.n$flag == 0, as.numeric(tree.n$measure), as.numeric(tree.n$newmeasure))
  tree.n$dbh <- as.numeric(tree.n$dbh)
  tree.n$measure <- as.numeric(tree.n$measure)
  #q <- mean(unlist(tapply(tree.n$measure, tree.n$dendroID, diff)), na.rm = TRUE)

  for (i in 2:(nrow(tree.n))) {
    tree.n$dbh2[[i]] <-

      ifelse(tree.n$new.band[[i]] == 0 & tree.n$survey.ID[[i]] == 2014.01 & !identical(tree.n$dbh[[i]], tree.n$dbh[[i - 1]]),
        tree.n$dbh[[i]],

        ifelse(tree.n$new.band[[i]] == 0 & !is.na(tree.n$measure[[i]]) & !is.na(tree.n$dbh2[[i - 1]]),
          findDendroDBH(tree.n$dbh2[[i - 1]], tree.n$measure[[i - 1]], tree.n$measure[[i]]),

          ifelse(tree.n$new.band[[i]] == 0 & !is.na(tree.n$measure[[i]]) & is.na(tree.n$dbh2[[i - 1]]),
            findDendroDBH(tail(na.locf(tree.n$dbh2[1:i - 1]), n = 1), tail(na.locf(tree.n$measure[1:i - 1]), n = 1), tree.n$measure[[i]]),

            ifelse(tree.n$new.band[[i]] == 0 & is.na(tree.n$measure[[i]]), NA,

              ifelse(tree.n$new.band[[i]] == 1 & !is.na(tree.n$measure[[i]]) & !identical(tree.n$dbh[[i]], tree.n$dbh[[i - 1]]),
                tree.n$dbh[[i]],

                ifelse(tree.n$new.band[[i]] == 1 & !is.na(tree.n$measure[[i]]) & identical(tree.n$dbh[[i]], tree.n$dbh[[i - 1]]),
                  max(tree.n$dbh2[1:i - 1], na.rm = T) + mean(diff(tree.n$dbh2[1:i - 1]), na.rm = T),

                  ifelse(tree.n$new.band[[i]] == 1 & is.na(tree.n$measure[[i]]) & identical(tree.n$dbh[[i]], tree.n$dbh[[i - 1]]),
                    max(tree.n$dbh2[1:i - 1], na.rm = T) + mean(diff(tree.n$dbh2[1:(i - 1)]), na.rm = T),

                    ifelse(tree.n$new.band[[i]] == 1 & is.na(tree.n$measure[[i]]) & !identical(tree.n$dbh[[i]], tree.n$dbh[[i - 1]]),
                      tree.n$dbh[i] + mean(diff(tree.n$dbh2[1:(i - 1)]), na.rm = TRUE),
                      tree.n$dbh2
                    )
                  )
                )
              )
            )
          )
        )
      )
  }
  all_stems_intra[[stems]] <- tree.n
}
# splist <- c("acru", "caca", "caco", "cagl", "caovl", "cato", "ceca", "cofl", "fagr", "fram", "juni", "litu", "nysy", "pist", "ploc", "qual", "qupr", "quru", "quve", "tiam", "ulru")
# Reformat data into a similar format as used in the example of this script ##
# for(q in 2010:2019){
# Stem <- subset(all_stems_intra[[1]], year == q) #Get original DOY where measurements were taken. Do they need to be consistent between years or would a little wiggle room be fine?
# for(w in splist){
#  Stem <- subset(Stem, sp == w)
# Stem$month <- ifelse(Stem$month == "January", 1,
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
# Stem$DOY <- as.Date(with(Stem, paste(year, month, day, sep="-")), "%Y-%m-%d")
# Stem$DOY <- yday(Stem$DOY)

# DOY_inDF1 <- Stem$DOY # List with original DOY. Used to make any measurement taken on days other than these = NA in DF resulting from following loop
# Stem$dbh2 <- ifelse(Stem$DOY == c(DOY_inDF1), Stem$dbh2, NA) #Added this to make any measurement not on the same DOY as the DOY i will use for analysis = NA

all_stems <- data.frame(NULL)

# Stem[NULL ,c(1:length(Stem$tag))]




for (e in 1:length(all_stems_intra)) { # Loop to merge all dendroband surveys into one dataframe
  Stem <- all_stems_intra[[e]]
  Stem$month <- ifelse(Stem$month == "January", 1,
    ifelse(Stem$month == "February", 2,
      ifelse(Stem$month == "March", 3,
        ifelse(Stem$month == "April", 4,
          ifelse(Stem$month == "May", 5,
            ifelse(Stem$month == "June", 6,
              ifelse(Stem$month == "July", 7,
                ifelse(Stem$month == "August", 8,
                  ifelse(Stem$month == "September", 9,
                    ifelse(Stem$month == "October", 10,
                      ifelse(Stem$month == "November", 11,
                        ifelse(Stem$month == "December", 12, Stem$month)
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
  Stem$DOY <- as.Date(with(Stem, paste(year, month, day, sep = "-")), "%Y-%m-%d")
  Stem$DOY <- yday(Stem$DOY)
  all_stems <- rbind(all_stems, Stem)
}
all_stems <- all_stems[complete.cases(all_stems$dbh2), ]
all_stems <- all_stems[, c(1:31, 34, 35)]

# Added by bert: write all_stems observed data to csv
write.csv(all_stems, file = "Data/all_stems.csv", row.names = FALSE)
all_stems <- read.csv("Data/all_stems.csv")
all_stems <- read_csv("~/GitHub/Dendrobands/data/scbi.dendroAll_2020.csv")


## Manual check of data ##

# all_stems$tag_stem <- paste0(all_stems$tag, "_", all_stems$stemtag)
# all_stems$tag_year <- paste(all_stems$tag_stem, "_", all_stems$year)

# write.csv(all_stems, file = "all_stems.csv", row.names = FALSE)

# library(readr)
# all_stems <- read_csv("C:/Users/world/Desktop/Github/growth_phenology/Data/all_stems.csv")

# currenttag <- all_stems[all_stems$tag_year %in% all_stems[1,35],]
# plot(currenttag$measure~currenttag$DOY, main = unique(currenttag$tag_year), xlab = "DOY", ylab = "Dendroband Measure")

# all_stems <- all_stems[!(all_stems$tag_year %in% currenttag$tag_year),]





# Loop to create a final "master dataframe" -------
# The following loop will cycle through years, species, and tags to create a final "master dataframe"
# It includes two of Sean's functions wrapped with some of my code
all_stems <- read_csv("data/all_stems.csv")
all_stems$dbh2 <- all_stems$dbh2/10
# Objects needed for the following loop
data <- data.frame(NULL)
# throwaways <- data.frame(NULL)
check_list <- NA
DOY2 <- NULL
masterDF <- data.frame(1, 2, 3, 4, 5, 6, 7, 8, 9)
colnames(masterDF) <- c("tot", "perc", "tag", "DOY", "sp", "year", "dbh", "max_rate_DOY", "max_rate")

# Added by bert: track LG5 parameters for each tag/year in this list
tag_years <- all_stems %>%
  transmute(tag_year = str_c(tag, year, sep = "-")) %>%
  pull(tag_year) %>%
  unique()

LG5_parameters <- vector(mode = "list", length = length(tag_years))
names(LG5_parameters) <- tag_years
growth <- NULL
all_stems$tag_stem <- paste0(all_stems$tag, "_", all_stems$stemtag)

for (q in 2020) {
  skip_to_next <- FALSE
  Stem2 <- subset(all_stems, year == q)
  for (w in unique(Stem2$sp)) { # removes trees with less than 10 measurements in each year
    Stem3 <- subset(Stem2, sp == w)
    count_df <- count(Stem3, tag_stem)
    count_df <- subset(count_df, n >= 10)
    Stem3 <- Stem3[Stem3$tag_stem %in% count_df$tag_stem, ]
    # original_list <- unique(Stem3$tag)
    for (m in unique(Stem3$tag_stem)) { # remove trees with very small or negative total growth
      growthcheck <- subset(Stem3, tag_stem == m)
      check_list <- append(check_list, (ifelse(growthcheck[nrow(growthcheck), 32] - growthcheck[1, 32] <= 0, unique(growthcheck$tag), ifelse(growthcheck[nrow(growthcheck), 32] - growthcheck[1, 32] >= 12.5, unique(growthcheck$tag), NA)))) # 1.25 is arbitrarily chosen. I'm considering decreasing it
      growth <- append(growth, growthcheck[nrow(growthcheck), 32] - growthcheck[1, 32])
      if (growthcheck[nrow(growthcheck), 32] - growthcheck[1, 32] <= 0){
        plot(growthcheck$dbh2 ~ growthcheck$DOY, main = growthcheck$year)
        print(growthcheck$tag_stem)
        print(growthcheck$year)
      }
      check_list <- check_list[complete.cases(check_list)]
    }
    Stem3 <- Stem3[!(Stem3$tag %in% check_list), ]
    skip_to_next <- FALSE # If all trees fail to meet the minimum growth criteria, an error is produced which stops the loop
    tryCatch( # Go to next species if all tags of previous species failed the growth check
      Stem3$tag_stem <- paste0(Stem3$tag, sep = "_", Stem3$stemtag) # add a unique tag for each stem of trees with multiple stems
      ,
      error = function(b) {
        skip_to_next <<- TRUE
      }
    )
    if (skip_to_next) {
      next
    } # If you know how to make this display a warning when it encounters an error i'd like to add that eventually
    for (e in unique(Stem3$tag_stem)) {
      data <- data.frame(NULL)

      # spline_test <- subset(Stem3, tag_stem == e) #check for outliers/mistakes in the data
      # fit1 <- smooth.spline(x = spline_test$DOY, y = spline_test$measure ,df = 4)
      # plot(spline_test$measure ~ spline_test$DOY)
      # lines(fit1)
      # spline_test$splinepred <- fit1[["y"]]
      # spline_test$dummy <- ifelse(abs(spline_test$splinepred - spline_test$measure) >= 7, 1, 0)
      # throwaways <- rbind(throwaways, spline_test[(spline_test$dummy == 1),])
      # stemstag <- spline_test[(spline_test$dummy == 0),]
      # stemstag <- stemstag[,c(-34,-35)]
      # rm(spline_test)

      stemstag <- subset(Stem3, tag_stem == e)
      # stemstag <- mutate(stemstag, dif = measure-lag(measure))

      data <- rbind(data, stemstag$dbh2)

      colnames(data) <- paste0("X", stemstag$DOY) # This is the format that Sean's functions use

      # tags <- unique(stemsDF$tag)
      # day <- subset(stemstag, tag == 132539)
      # treeone <- data[1,]
      # DOY <- data.frame(sort(unique(stemsDF$DOY)))
      # DOY <- mutate(DOY, dif = sort.unique.stemsDF.DOY..-lag(sort.unique.stemsDF.DOY..))
      # DOY[1,2] <- 0
      # for(i in 1:length(unique(stemsDF$DOY))){
      #  DOY[i,1] <- ifelse(DOY[i,2] <= 3 & DOY[i,2] >= 1, DOY[i-1,1], DOY[i,1])
      # }
      # DOY <- unique(DOY$sort.unique.stemsDF.DOY..)
      # colnames(data) <- paste0("X", DOY)

      ### R code from vignette source 'MODEL_FIT_NEW_revised.Rnw'
      ### Encoding: UTF-8



      # a) lg5.model.runs ------
      # The list/unlist is done so that we can see the values attributed to the
      # parameters and then make them numeric so that the optim function can use them
      # as a numeric vector.

      ##  STARTING PARAMETERS AND MIN AND MAX VALUES FOR THE L-BFGS-B CALLS

      # This is the beginning of the first of Sean's functions - I can try to help with questions about this but parts of it are over my head
      dbh.data <- data
      tree.no <- dim(dbh.data)[1]
      doy.full <- get.doy(dbh.data)
      lg5.hess <- vector("list", tree.no)
      winning.optim.call <- c()
      optim.output.df <- c()
      resids.mat <- matrix(NA, tree.no, length(doy.full))
      # pdf("FIGURES/lg5_fit_all.pdf")
      # par(mfrow = c(2,2))
      #pb <- txtProgressBar(style = 3)
      for (r in 1:tree.no) { # Sean's first function
        #setTxtProgressBar(pb, r / tree.no, title = NULL, label = NULL)
        par(mfrow = c(1, 1))
        # 	pdf(file = sprintf("FIGURES/lg5_fit_%i.pdf", i))
        dbh <- as.numeric(dbh.data[r, ])

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

        lg5.output.NM <- optim(par = params, fn = lg5.ML, resid.sd = resid.sd, method = "Nelder-Mead", hessian = TRUE, control = list(trace = 0), doy = doy, dbh = dbh)
        hess.tmp[[3]] <- lg5.output.NM$hessian

        lg5.output.NM.wt <- optim(par = params, fn = lg5.ML.wt, resid.sd = resid.sd, method = "Nelder-Mead", hessian = TRUE, control = list(trace = 0), doy = doy, dbh = dbh)
        lg5.output.NM.wt$value <- lg5.ML(lg5.output.NM.wt$par, doy, dbh, resid.sd = resid.sd)
        hess.tmp[[4]] <- lg5.output.NM.wt$hessian

        lg5.output.SANN <- optim(par = params, doy = doy, dbh = dbh, resid.sd = resid.sd, fn = lg5.ML, method = "SANN", hessian = TRUE, control = list(maxit = 30000, trace = F))
        hess.tmp[[5]] <- lg5.output.SANN$hessian

        lg5.output.SANN.wt <- optim(par = params, doy = doy, dbh = dbh, resid.sd = resid.sd, fn = lg5.ML.wt, method = "SANN", hessian = TRUE, control = list(maxit = 30000, trace = F))
        hess.tmp[[6]] <- lg5.output.SANN.wt$hessian

        lg5.output.SANN.wt$value <- lg5.ML(lg5.output.SANN.wt$par, doy, dbh, resid.sd = resid.sd)


        ## CONSOLIDATE THE RESULTS  ##

        optim.output <- rbind(c(params.start, NA), c(lg5.output.LB$par, lg5.output.LB$value), c(lg5.output.LB.wt$par, lg5.output.LB.wt$value), c(lg5.output.NM$par, lg5.output.NM$value), c(lg5.output.NM.wt$par, lg5.output.NM.wt$value), c(lg5.output.SANN$par, lg5.output.SANN$value), c(lg5.output.SANN.wt$par, lg5.output.SANN.wt$value))

        winner <- rep(".", length = 7)
        winner[1] <- NA
        winner[which(optim.output[, 6] == min(optim.output[-1, 6], na.rm = T))] <- "*"

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
        if (r == 1) {
          optim.output1 <- optim.output

          win.vec1 <- win.vec
          optim.output.df.1 <- optim.output.df[, -1]
          names(optim.output.df.1) <- c("Optim.call", "L", "K", "doy_ip", "r", "theta", "ML", "Best.ML")
          doy1 <- doy
          dbh1 <- dbh
        }
        # plot(doy, dbh, xlab = "Day of the year", ylab = "DBH (cm)", pch = 19, col = "gray15", main = sprintf("Annual Growth for tree %i", i), cex = 1)

        days <- seq(365)
        # for(t in 2:dim(optim.output)[1]) {
        #  lines(days, lg5.pred(params = optim.output[t ,], doy = days), col = cols[t - 1], lty = win.vec[t - 1], lwd = 1)
        # }

        # legend("bottomright", legend = calls[-1], col = cols, lwd = 2, lty = win.vec)


        # 	dev.off()
        winning.optim.call[r] <- c("L-BFGS-B", "L-BFGS-B wt", "N-M", "N-M wt", "SANN", "SANN wt")[winner]
        resids.mat[r, complete] <- get.lg5.resids(params = lg5.output.LB.wt$par, doy, dbh)
      }
      #close(pb)
      # dev.off()


      names(optim.output.df) <- c("Tree.no", "Optim.call", "L", "K", "doy_ip", "r", "theta", "ML", "Best.ML")
      # write.csv(optim.output.df, file = "Optim_output.csv", quote = FALSE, row.names = FALSE)
      ML_value <- as.character(optim.output.df[optim.output.df$Optim.call == winning.optim.call, 8])

      winner.tab <- table(winning.optim.call)
      winner.tab


      # b) upper.lower.bounds -----
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


      # pdf("FIGURES/HI_LO_fit_all.pdf")
      # par(mfrow = c(2,2))

      for (y in 1:tree.no) { # Sean's second function -- same deal
        print(y)

        # 	par(mfrow = c(1,1))
        # 	pdf(file = sprintf("FIGURES/Low_up_%i.pdf", i))

        dbh <- as.numeric(dbh.data[y, ])

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
        if (win.optim.call == "L-BFGS-B wt") {
          hi.lo.output <- optim(par = params, doy = doy, dbh = dbh, resid.sd = resid.sd, fn = lg5.ML.wt, method = "L-BFGS-B", hessian = TRUE, control = list(trace = 0))
        } else {
          hi.lo.output <- optim(par = params, doy = doy, dbh = dbh, resid.sd = resid.sd, fn = lg5.ML.wt, method = "Nelder-Mead", hessian = TRUE, control = list(trace = 0))
        }

        params.tmp <- hi.lo.output$par
        ## Now call the boundary functions
          tryCatch(
        start.d[y, ] <- start.diam(params = params.tmp, seq.l = seq.l, doy = doy, dbh, deviation.val = deviation.val, figure = FALSE, resid.sd) # Both start and end estimates are returning 200 numbers instead of 1
          ,error = function(e) {
            message(paste("start D", unique(stemstag$tag_stem), unique(stemstag$year)))
            })
          #if(skip_to_next) {
          #  }
          tryCatch(
        end.d[y, ] <- end.diam(params = params.tmp, seq.l = seq.l, doy = doy, dbh, deviation.val = deviation.val, figure = FALSE, resid.sd) # fix this ... dev.val too small
          ,error = function(e) {
            message(paste("End D", unique(stemstag$tag_stem), unique(stemstag$year)))

        })
         #if(skip_to_next) { next }
        a <- c(start.d[y, 1], end.d[y, 1])

        # If you want to see the ploted model, remove the '#' in the following two lines:
        plot(doy, dbh, xlab = "Day of the year", ylab = "DBH (cm)", pch = 19, col = "gray15", main = ?sprintf("Annual Growth for tree %y", y), cex = 1)
        lines(days, lg5.pred.a(a, params = params.tmp, doy = days, asymptote = "both"), col = "darkred", lty = 1, lwd = 1)
        Param.df[y, 6:7] <- a

        Param.df[1, 1:5] <- params.tmp

        # 	dev.off()
      }

      # dev.off()


      names(Param.df) <- c("L", "K", "doy_ip", "r", "theta", "a", "b")
      # write.csv(Param.df, file = "SCBI_hi_lo_lg5.csv", quote = FALSE, row.names = FALSE)
      #K <- as.numeric(Param.df[1,2])
      #L <- as.numeric(Param.df[1,1])
      #doy.ip <- as.numeric(Param.df[1,3])
      #r <- as.numeric(Param.df[1,4])
      #theta <- as.numeric(Param.df[1,5])
      #params_function <- c(K, L, doy.ip, r, theta)
      #dbh <- dbh
      #doy <- doy
      #ML_value <- get.lg5.ML(params_function, doy,dbh, .5)
      # Added by bert: save tag/year + parameter values
      param_values_to_save <- c(stemstag$tag[1], stemstag$year[1],  Param.df, ML_value) %>% unlist()
      names(param_values_to_save) <- c("tag", "year", names(Param.df), "ML_value")
      tagyear_id <- stemstag %>%
        slice(1) %>%
        transmute(tag_year = str_c(tag, year, sep = "-")) %>%
        pull(tag_year)
      LG5_parameters[[tagyear_id]] <- param_values_to_save


      # Max growth rate DOY
      maxDOY <- NULL
      # c) Day_values ------
      params <- Param.df[1, ]
      start.day <- round(pred.doy(params, params$a))
      stop.day <- round(pred.doy(params, params$b))
      .deriv <- lg5.deriv(paras = params, days, growth = params$b - params$a)
      fifty.day <- round(pred.doy(params, mean(c(params$a, params$b))))

      # for(i in 1:nrow(Param.df)){
      #  max <- max.growth.day(Param.df[i,])
      #  maxDOY <- append(maxDOY, max)
      # }

      max <- max.growth.day(Param.df[1, ]) #
      tryCatch(
      maxrate <- max.growth.rate(Param.df[1, ]) #
      , error = function(e) {
        message(paste(unique(stemstag$tag_stem), unique(stemstag$year), "maxrate NA"))
        }
      )
#if (skip_to_next) {
#  next
#}
      #  if(skip_to_next) { next }
      # maxDF <- data.frame(1:length(maxDOY), maxDOY)
      # hist(maxDF$maxDOY, breaks = 11)
      # mean(maxDF$maxDOY)
      # sd(maxDF$maxDOY)

      # Growth over the year
      # lg5.pred <- function(params, doy)
      curveDF <- data.frame(NULL)
      endcurveDF <- data.frame(c(1:365)) ###################### Change start and end date here
      #pb <- txtProgressBar(style = 3)
      # for( u in 1:nrow(Param.df)){
      #  setTxtProgressBar(pb, u / nrow(Param.df), title = NULL, label = NULL)
      growth_curve <- Param.df[1, ]
      for (i in 1:365) { ##################################### Change period of data here
        prediction <- lg5.pred(growth_curve, i)
        curveDF <- rbind(curveDF, prediction)
      }
      # colnames(curveDF) <- paste0("Tree", u)
      curveDF <- mutate(curveDF, dif = curveDF[, 1] - lag(curveDF[, 1]))

      endcurveDF <- cbind(curveDF, endcurveDF)
      endcurveDF <- endcurveDF[complete.cases(endcurveDF), ]
      # curveDF <- data.frame(NULL)

      # plotting each tree
      seq <- seq(2, ncol(endcurveDF), 2)
      growth_rate <- endcurveDF[, seq]
      # plot.ts(growth_rate, plot.type = "single")

      # plotting mean growth for trees in DF
      # means <- data.frame(rowMeans(differences))
      # means <- data.frame(means[-1,])
      # max <- subset(means, means$means..1... == max(means$means..1...))
      # plot(means$means..1...)

      # 25%, 50%, 75% intervals
      # growthvariables <- data.frame(NULL)
      # for(o in 1:ncol(differences)){
      tempdifferences <- data.frame(growth_rate[complete.cases(growth_rate)]) # data.frame(differences[2:nrow(differences),i])
      colnames(tempdifferences) <- "dif"
      tempdifferences$addition <- cumsum(tempdifferences$dif) # make a rolling total of the differences
      #  tryCatch(
      tempdifferences$tot <- sum(tempdifferences$dif) # Find the total change within the year. Error produced when params not assigned to params.df
      #  error = function(e) { skip_to_next <<- TRUE})
      #  if(skip_to_next) { next }
      tempdifferences$perc <- NA # create the row where percentages will be appended
      for (p in c(.25, .50, .75)) { # Loop to find when 25%, 50%, and 75% of yearly growth occurred
        try <- which(abs(tempdifferences$addition - tempdifferences$tot * p) == min(abs(tempdifferences$addition - tempdifferences$tot * p))) # find where the absolute value of the rolling total subtracted by the value we want (total*.25, .50, or .75) is closest to zero
        tempdifferences$perc[try] <- p # assign the number to the value found above
      }
      tempdifferences <- tempdifferences[complete.cases(tempdifferences), ]

      # tempdifferences$tree <- paste0("tree", i)
      tempdifferences$tag <- e ####
      tempdifferences$DOY <- paste0(row.names(tempdifferences))
      if (sum(tempdifferences$perc) == 1.5) { # ifelse to remove instances where .25, .50, and .75 values could not be found. Some years were returning multiples of each percentage or only 2 out of the 3 percentages.
        tempdifferences <- tempdifferences
      } else {
        tempdifferences <- NULL
      }

      #  growthvariables <- rbind(growthvariables, tempdifferences)
      # }

      phenology_DF <- tempdifferences[, 3:6]
      phenology_DF$sp <- w
      phenology_DF$year <- q
      phenology_DF$dbh <- median(stemstag$dbh2)
      # max_rate_DOY <- subset(endcurveDF, endcurveDF$dif == max(endcurveDF$dif))
      phenology_DF$max_rate_DOY <- max
      phenology_DF$max_rate <- maxrate
      skip_to_next <- FALSE
      tryCatch( #If all 3 percs were not present, DF was wiped and error will be produced here
        masterDF <- rbind(masterDF, phenology_DF),
        error = function(e) {
          message(paste("throw out", unique(stemstag$tag_stem), unique(stemstag$year)))
          skip_to_next <<- TRUE
        }
      )
      if (skip_to_next) {
        next
      }
      rm(growth_rate)
      rm(endcurveDF)
      rm(Param.df)
      rm(data)
    }
  }
}




# Write master data frames ----
## Wood phenology data ----
warnings()
masterDF <- masterDF[-1, ]
masterDF$wood_type <- ifelse(masterDF$sp == "quru" | masterDF$sp == "qual", "ring porous", ifelse(masterDF$sp == "litu" | masterDF$sp == "fagr", "diffuse-porous", "other"))
write.csv(masterDF, file = "Data/Wood_pheno_table_SCBI_RAW.csv", row.names = FALSE)
masterDF$DOY <- as.numeric(masterDF$DOY)

## Fitted parameter data ----
# Added by bert: save parameter values
LG5_parameters %>%
  bind_rows() %>%
  write_csv(file = "Data/LG5_parameter_values_SCBI_RAW.csv")

#LG5_try <- bind_rows(LG5_parameters)
#write.csv(bind_rows(LG5_parameters), file = "Data/LG5_parameter_values_SCBI_RAW.csv")

