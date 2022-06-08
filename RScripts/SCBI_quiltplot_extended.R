######################################################
# Purpose: Calculate correlations and response (and plot correlations) between tree-ring chronologies and climate variables
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 3.5.1 (2018-07-02)
######################################################

# Clean environment ####
rm(list = ls())

set.seed(42)

# Load libraries ####
library(dplR)
library(bootRes)
library(caTools)
library(RCurl)

#Change my.dccplot function to remove previous / current year lines
#Manually rearrange x to put in desired plotting order right before plotting
source("RScripts/0-My_dplR_functions.R")

# Define parameters and variables ####

## saving or not saving outputs ? ####
save.plots <- TRUE
save.result.table <- TRUE

## Define order of the species in the  plots, based on ANPP contribution####
#ANPP_contribution <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/summary_data/ANPP_total_and_by_species.csv"), header=T)

#SPECIES_IN_ORDER <- toupper(ANPP_contribution$species[ ANPP_contribution$species %in% c("litu", "qual", "quru", "quve", "qupr", "fram", "cagl", "caco", "cato", "juni", "fagr", "caovl", "pist", "frni")])
#SPECIES_IN_ORDER <- gsub("CAOVL", "CAOV", SPECIES_IN_ORDER)

SPECIES_IN_ORDER <- c("FAGR", "LITU", "CACO", "CAGL", "CAOVL", "CATO", "FRAM", "FRNI", "QUAL", "QUPR", "QURU", "QUVE")

## Define sets of climate data to use ####

climate.data.types <- c("CRU_SCBI_1901_2016")

## Define sets of methods to run ####

methods.to.run <- c("correlation") # c("correlation", "response", "moving_correlation")

## Define how to run it regarding the starting year ####
type.of.start.date <- c("1901_2009", "1920_1949", "1950_1979", "1980_2009")


## Define sss threshold ####
sss.threshold = 0.80
## Define start and end month for anlaysis ####
start <- -1 # April of previous year
end <- 10 # August of current year

start.frs <- -10 # october of previous year (for freeze days variable only - otherwise error because all 0 in other months)
end.frs <- 5 # may of current year (for freeze days variable only)

# Load and prepare core data ####

filenames <- list.dirs("data/tree_rings/chronologies/SCBI/cores/", full.names = F, recursive = F  )
filenames <- filenames[!grepl("[a-z]", filenames)] # keep only all caps names

all_sss <- NULL

sd_coreres <- NULL # will store SD of the detrended chronologie
mean_core_raw_per_species <- NULL # will store the mean radisu increment per indviduals


for(f in filenames) {
  # get the raw data
  core_raw <- read.rwl(paste0("data/tree_rings/chronologies/SCBI/cores/", f,"/", tolower(f), "_drop.rwl"))
  mean_core_raw_per_species <- c(mean_core_raw_per_species, mean(apply(core_raw, 2, mean, na.rm = T)))

  # get the detrended data
  core <- read.table(paste0("data/tree_rings/chronologies/SCBI/cores/", f,"/ARSTANfiles/", tolower(f), "_drop.rwl_tabs.txt"), sep = "\t", h = T)
  core <- data.frame(res = core$res,  samp.depth = core$num, row.names = core$year)

  # output the SD of the detrended chronologies (see issue # 63 on GitHub)
  sd_coreres <- rbind(sd_coreres, data.frame(Species = f, SD = round(sd(core$res), 2)))

  # get the Subsample Signal Strength (sss as function of the number of trees in sample, the last one appearing in the "xxx_drop.rxl_out.txt files)

  sss <- readLines(paste0("data/tree_rings/chronologies/SCBI/cores/", f,"/ARSTANfiles/", tolower(f), "_drop.rwl_out.txt"))
  sss <- sss[grep("sss", sss)]

  sss <- sss[grep("  sss:   ", sss)[c(rep(FALSE, 3*length(seq(grep("  sss:   ", sss)))/4), rep(TRUE, 1*length(seq(grep("  sss:   ", sss)))/4))]] # keep only last rows that have sss: in them

  sss <- sub("  sss:   ", "", sss)
  sss <- as.numeric(unlist(strsplit(sss, " " ))) # keep only numbers and store them as a vector

  sss <- data.frame(Species = f, "Num_of_trees" = 1:length(sss), sss)

  Year_to_Num_of_trees <- apply(core_raw, 1, function(x) sum(!is.na(x)))
  Year_to_Num_of_trees <- data.frame(Species = f, Year = as.numeric(names(Year_to_Num_of_trees)), Num_of_trees= Year_to_Num_of_trees)

  match(Year_to_Num_of_trees$Num_of_trees, sss$Num_of_trees)

  Year_to_Num_of_trees$sss <- NA
  for(i in 1:nrow(Year_to_Num_of_trees)) {

    Year_to_Num_of_trees$sss[i] <- rev(sss[sss$Num_of_trees <= Year_to_Num_of_trees$Num_of_trees[i],]$sss)[1]

  }

  sss <- Year_to_Num_of_trees

  assign(f, core)
  assign(paste0(f, "_sss"), sss)

  all_sss <- rbind(all_sss, sss)

}

# save SSS for all species

##############HERE###########HERE#################HERE###############HERE###
                                #
                                #
#write.csv(all_sss, file = "results/tree_cores/SCBI_SSS_as_a_function_of_the_number_of_trees_in_sample.csv", row.names = F)

# save sd_coreres for all species
write.csv(sd_coreres, file = "results/tree_cores/SCBI_SD_of_each_detrended_chornologies.csv", row.names = F)

# save mean radius increment
## see: https://github.com/SCBI-ForestGEO/climate_sensitivity_cores/issues/62

##############HERE###########HERE#################HERE###############HERE###
                              #
                              #
#write.csv(data.frame(Species = filenames, mean_rad_inc = mean_core_raw_per_species), file = "results/tree_cores/SCBI_mean_radius_increment.csv", row.names = F)


## Define start and end year for analysis, common to all species and one for each species ####

start.years.sss.df <- data.frame(1,1)
start.years.sss.df <- start.years.sss.df[-1,]


start.years.sss <- NULL # species specific
for(f in filenames) {
  sss <- get(paste0(f, "_sss"))
  start.years.sss <- c(start.years.sss, sss[sss$sss >= sss.threshold, ]$Year[1])

  }
start.years.sss.df <- data.frame(start.years.sss,filenames)

full.time.frame.end.year = 2009  # for now and later for sd.clim_fill_time_frame (in 0-My_dplR_functions.R)

# Plot SSS for the the decided threshold ####

if(save.plots) tiff("results/tree_cores/SCBI_SSS_as_a_function_of_the_number_of_trees_in_sample.tiff", res = 150, width = 169, height = 169, units = "mm", pointsize = 10)

op <- par(mfrow = c(2, 1), oma = c(5, 5, 2, 0), mar = c(0, 0, 0, 1))

cols <- data.frame(col = rainbow(length(filenames)), row.names = filenames, stringsAsFactors = F)
years <- NULL
list("c","b")

all_sss$Species <- as.factor(all_sss$Species)# i added

for(sp in levels(all_sss$Species)){
  x = all_sss[all_sss$Species %in% sp,]
  year <- x$Year[x$sss > sss.threshold][1]
  years <- c(years, year)
}

plot.nb <- 1

for(sp in levels(all_sss$Species)){

  x <- all_sss[all_sss$Species %in% sp,]
  x <- x[x$Year <= full.time.frame.end.year,]
  # n.core <- x$Num_of_trees[x$sss > sss.threshold][1]

  if(plot.nb %in% 1) {
    plot(Num_of_trees ~ Year, data = x, type = "l", col = cols[sp,], xlim = c(min(all_sss$Year), full.time.frame.end.year), ylim = range(all_sss$Num_of_trees), lwd = 2, log = "y", las = 1, ylab = "", xaxt = "n")
    mtext(side= 2 , "log(No. cores)", line = 3)
    axis(1, labels = F, tcl = 0.5)
    axis(1, labels = F, tcl = -0.5)
    mtext("a)", side = 1, line = -1, adj = 0.01, font = 2)

  } else {
    lines(Num_of_trees ~ Year, data = x, col = cols[sp,], lwd = 2)
  }

  plot.nb <- plot.nb +1
}

abline(v =years,  col = cols$col, lty = 2)
legend("topleft", col = cols$col, lty = 1, bty = "n", legend = paste(levels(all_sss$Species), years, sep = " - "), lwd = 2, cex = 0.8)

plot.nb <- 1

for(sp in levels(all_sss$Species)){
  x <- all_sss[all_sss$Species %in% sp,]
  x <- x[x$Year <= full.time.frame.end.year,]

  year <- x$Year[x$sss > sss.threshold][1]

  if(plot.nb %in% 1) {
    plot(sss ~ Year, data = x, type = "l", col = cols[sp,], xlim = c(min(all_sss$Year), full.time.frame.end.year), lwd = 2, las = 1, xaxt = "n")
    abline(v = year, lty = 2, col = cols[sp,])
    abline(h = 0.75, lty = 3)
    axis(1, labels = T, tcl = 0.5)
    axis(1, labels = F, tcl = -0.5)
    mtext(side= 2 , "sss", line = 3)
    mtext("b)", side = 1, line = -1, adj = 0.01, font = 2)

  } else {
    lines(sss ~ Year, data = x, col = cols[sp,], lwd = 2)
    abline(v = x$Year[x$sss > sss.threshold][1], lty = 2, col = cols[sp,])
  }
  plot.nb <- plot.nb +1
}

title(paste("SSS threshold =", sss.threshold), outer = T)
par(op)

if(save.plots) dev.off()
par(op)

##############################################################################################
all.dcc.output <- NULL#
corr.dcc.output <- NULL#
species <- filenames
climate_variables <- c("tmx", "tmn")
for(f in species) {
  print(f)

  end.year <- 2009
  start.year <- start.years.sss.df[filenames == f,1]


  # load species chronology data ####
  core <- get(f)

  # load climate data for corresponding site (not necessary since you have only one site, but renaming to clim so that the rest works)  ####
  clim <- read.csv(paste0("Data/climate data/CRU/Formated_CRU_SCBI_1901_2016.csv"))
  clim <- clim[,c(1,2,9,11)]
  ### crop last year to full.time.frame.end.year
  clim <- clim[clim$year <= end.year, ]


  # trim measurement years ####
  ## remove years of core measurement that are before climate record (+ first few first months to be able to look at window before measurement)
  #Wasn't sure what window_range was meant to be, so just removed it. I assume this will make the first year's correlation less reliable but since there is a lot more years it shouldn't have a big impact?
  core <- core[as.numeric(rownames(core)) >= (min(as.numeric(clim$year))),]#window_range[1]/12), ]


  ## remove years that are after climate record
  core <- core[as.numeric(rownames(core)) <= max(as.numeric(clim$year)), ]

  start.year <- max(min(clim$year), start.year)# max(min(clim$year), start.years[which(site_sps[!site_sps %in% species_to_drop] %in% f)])

  # run analysis for each variable
  for (v in  climate_variables) {
    print(v)


    corr.dcc.output <- my.dcc(chrono = core["res"], clim = clim[, c("year", "month", v)], method = "correlation", start = start, end =  end, timespan = c(start.year, end.year), ci = 0.05, ci2 = 0.002)
    all.dcc.output <- rbind(all.dcc.output, data.frame(cbind(Species = substr(f, 1, 4), corr.dcc.output)))#

  }

  ### plot ####

  # you should know ploting function

}


all.dcc.output$variable <- substr(paste(row.names(all.dcc.output)), 1, 3)#get variable from row name
all.dcc.output$month <- substr(paste(row.names(all.dcc.output)), 5, 12)#get month from row name

#write.csv(all.dcc.output, file = "results/SCBI_core_corr.csv", row.names = FALSE)
#} # for(f in species)

#############################################
##Copy/Paste this section from other script##
#############################################
save.plots = TRUE
for(v in climate_variables) {
  print(v)

  X <- all.dcc.output[all.dcc.output$variable %in% v, ]

  x <- data.frame(reshape(X[, c("month", "Species", "coef")], idvar = "month", timevar = "Species", direction = "wide"))
  rownames(x) <- ifelse(grepl("curr",  rownames(x)), toupper(rownames(x)), tolower( rownames(x)))
  rownames(x) <- gsub(".*curr.|.*prev.", "",   rownames(x), ignore.case = T)

  x.sig <- reshape(X[, c("month", "Species", "significant")], idvar = "month", timevar = "Species", direction = "wide")
  x.sig2 <- reshape(X[, c("month", "Species", "significant2")], idvar = "month", timevar = "Species", direction = "wide")

  colnames(x) <- gsub("coef.", "", colnames(x))
  colnames(x.sig) <- gsub("significant.", "", colnames(x.sig))
  colnames(x.sig2) <- gsub("significant2.", "", colnames(x.sig2))

  x <- x[, -1]
  x.sig <- x.sig[, -1]
  x.sig2 <- x.sig2[, -1]

  # x <- x[, rev(SPECIES_IN_ORDER[!SPECIES_IN_ORDER %in% gsub("CAOVL", "CAOV", species_to_drop)])]
  #  x.sig <- x.sig[, rev(SPECIES_IN_ORDER[!SPECIES_IN_ORDER %in% gsub("CAOVL", "CAOV", species_to_drop)])]
  #  x.sig2 <- x.sig2[, rev(SPECIES_IN_ORDER[!SPECIES_IN_ORDER %in% gsub("CAOVL", "CAOV", species_to_drop)])]

  # if(save.plots)  {
  #    dir.create(paste0("results/", type.start, "/figures/monthly_", method.to.run), showWarnings = F)
  #    dir.create(paste0("results/", type.start, "/figures/monthly_", method.to.run, "/", c), showWarnings = F)
  #    tiff(paste0("results/", type.start, "/figures/monthly_", method.to.run, "/", c, "/", v, ".tif"), res = 150, width = 169, height = 169, units = "mm", pointsize = 10)
  #  }

  v <-  toupper(v)
  v <- gsub("PDSI_PREWHITEN" , "PDSI", v)
  #x <- x[,c(5,9,1,2,3,4,6,7,11,12,13,14)]
  x <- x[,c(9,5,14,13,12,11,6,4,3,2,1)]
  x.sig <- x.sig[,c(9,5,14,13,12,11,6,4,3,2,1)]
  x.sig2 <- x.sig2[,c(9,5,14,13,12,11,6,4,3,2,1)]

  png(paste0("results/tree_cores/quiltplots/plot_images/", "monthly_", "correlation", "SCBI","EXTENDED", v, ".png"), res = 150, width = 169, height = 169, units = "mm", pointsize = 10)

  my.dccplot(x = as.data.frame(t(x)), sig = as.data.frame(t(x.sig)), sig2 = as.data.frame(t(x.sig2)),  main = ifelse(v %in% "PETminusPRE", "PET-PRE", v), method = "correlation")

  if(save.plots) dev.off()
}
write.csv(all.dcc.output, file = "Results/tree_cores/quiltplots/plot_data/SCBI/scbi_core_corr_EXTENDED.csv", row.names = FALSE)
##################################################################################################
