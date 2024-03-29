# clear environment ####
rm(list = ls())
#Adding hf/scbi broke plotting function
#Possibly add the core and clim data pre-dcc creation

#tightly condensed plot by latitude.
#Two panels, rp/dp
set.seed(42)

# load libraries ####
library(bootRes)
library(dplR) # for read.rwl
library(climwin)
library(tidyverse)
library(lubridate)
library(readxl)
source("Rscripts/0-My_dplR_functions.R")

#Load in lat lon for plotting sort
TRW_coord <- read_excel("data/tree_rings/TRW_coord2.xlsx")
TRW_coord <- TRW_coord[,c(1,3)]
#Add original two locations to include in final quilt plot
originals <- data.frame(42.5388, "HF")
originals <- rbind(originals, c(38.8935, "SCBI"))
names(originals) <- c("Latitude", "Location")
TRW_coord <- rbind(TRW_coord, originals)
#Prepare csv's
crns <- read.csv("data/tree_rings/chronologies/all_crns_res_1901.csv")
#TRW_coord <- read_excel("Data/tree_rings/Other/TRW_coord.xlsx")

# Bert approach
crns <- read_csv("data/tree_rings/chronologies/all_crns_res_1901.csv") %>%
  # clean up
  select(-c(BearIs, OH_Gol_QUAL_1,`Greenbrook_Sanctuary,_NJ_LITU_LITU`)) %>%
  rename(IL_Fer_LITU = IL_Fer_LTU)

crns_long <- crns %>%
  # convert to long format
  pivot_longer(-Year, names_to = "site_sp", values_to = "ring_width") %>%
  # drop years with missing data
  filter(!is.na(ring_width))

crns_long_start_end <- crns_long %>%
  # for each site/sp extract start and end
  group_by(site_sp) %>%
  summarize(start = min(Year), end = max(Year)) %>%
  # split site_sp variable into site, sp
  mutate(
    site = str_sub(site_sp, 1, -6),
    sp = str_sub(site_sp, -4, n())
  )

# Generate named vector: start year
start.years.sss_bert <- crns_long_start_end$start
names(start.years.sss_bert) <- crns_long_start_end$site_sp
start.years.sss_bert

# end year



# Load climate data ####

## needs to be in different format: column for Date, year and then one column per climate variable
climate_variables <- c("tmn", "tmx")
clim_v <- NULL
# something like this should do
for(clim_v in climate_variables) {
  print(clim_v)
  x <- read.csv(paste0("Data/climate data/CRU/", clim_v,  ".1901.2019-Other_sites-3-11.csv"))


  ### subset for the sites we care about

  #x <- droplevels(x[x$sites.sitename %in% "Harvard_Forest", ])

  ### reshape to long format
  x_long <- reshape(x,
                    times = names(x)[-1],
                    timevar = "Date",
                    varying = list(names(x)[-1]),
                    direction = "long", v.names = clim_v)
  ### format date
  x_long$Date <- gsub("X", "", x_long$Date)
  x_long$Date <- as.Date(x_long$Date , format = "%Y.%m.%d")#changed format to work with Harvard data


  ### combine all variables in one
  if(clim_v == climate_variables[1]) all_Clim <- x_long[, c(1:3)]
  else all_Clim <- merge(all_Clim, x_long[, c(1:3)], by = c("sites.sitename", "Date"), all = T)

}

### add year column
all_Clim$year <- as.numeric(format(as.Date(all_Clim$Date, format = "%d/%m/%Y"), "%Y"))
### add month column
all_Clim$month <- as.numeric(format(as.Date(all_Clim$Date, format = "%d/%m/%Y"), "%m"))

clim_means <- all_Clim %>%
  filter(month == 4) %>%
  group_by(sites.sitename) %>%
  summarize(tmn = mean(tmn),
            tmx = mean(tmx)) %>%
  rename(Location = sites.sitename)

#SCBI clim_means
scbi_clim <- read.csv(paste0("Data/climate data/SCBI/Formated_CRU_SCBI_1901_2016.csv"))
scbi_clim <- scbi_clim[,c(1,2,9,11)]
scbi_clim$Location <- "SCBI"

scbi_clim_means <- scbi_clim %>%
  filter(month == 4) %>%
  group_by(Location) %>%
  summarize(tmn = mean(tmn),
            tmx = mean(tmx))
  #rename(Location = sites.sitename)

#Harvard forest clim_means
hf_clim <- subset(clim_means, clim_means$Location == "HF_LyfordPlots")
hf_clim$Location <- "HF"

# hf_clim <- read_csv("climate data/HF/Harvard_forest_cru.csv")
# hf_clim$year <- as.numeric(format(as.Date(hf_clim$Date, format = "%Y-%m-%d"), "%Y"))
# ### add month column
# hf_clim$month <- as.numeric(format(as.Date(hf_clim$Date, format = "%Y-%m-%d"), "%m"))
#
# hf_clim_means <- hf_clim %>%
#   filter(month == 4) %>%
#   group_by(sites.sitename) %>%
#   summarize(tmn = mean(tmn),
#             tmx = mean(tmx)) %>%
#   rename(Location = sites.sitename)

#Merge the 3 so we can sort later
clim_means <-rbind(clim_means, scbi_clim_means,hf_clim)
write.csv(clim_means, file = "clim_means_all.csv", row.names = FALSE)
# Get unique site and species names ----
species <- NULL
sites <- NULL
for(i in 1:ncol(crns)){
  new <- crns[c(1:119), c(1,1+i)]
  name <- colnames(new)
  name <- name[2]

  sp <- substr(name,nchar(as.character(name))-3, nchar(as.character(name)))
  species <- append(species,sp)

  site <- substr(name,1, nchar(as.character(name))-5)
  sites <- append(sites,site)
  #write.csv(assign(as.character(name),new), file = paste0("Data/tree_rings/",name,".csv"), row.names = FALSE)
  assign(as.character(name),new)

}

sites <- unique(sites)
species <- unique(species)

site_species <- unique(crns_long$site_sp)

# Code to produce csv in results/SD_of_each_detrended_chronologies.csv
SD_of_each_detrended_chronologies_bert <- crns_long %>%
  group_by(site_sp) %>%
  summarize(SD = sd(ring_width)) %>%
  mutate(SD = round(SD, 2))



## Run analysis to compare BERT ####
all.dcc.output <- NULL#
corr.dcc.output <- NULL#
#site_species <- c("SIPSEY_WILDERNESS_CAGL")
for(f in site_species) {
#f <- site_species[1]
    print(f)
  end.year <- crns_long_start_end %>%
    filter(site_sp == f) %>%
    pull(end)
  start.year <- crns_long_start_end %>%
    filter(site_sp == f) %>%
    pull(start)


  # load species chronology data ####
  core <- crns_long %>%
    filter(site_sp == f) %>%
    rename(res = ring_width) %>%
    as.data.frame()
  rownames(core) <- core$Year

  # load climate data for corresponding site (not necessary since you have only one site, but renaming to clim so that the rest works)  ####
  site <- substr(f,1, nchar(as.character(f))-5)

  clim <- all_Clim[all_Clim$sites.sitename %in% site,]
  clim <- clim[,c(-1)]
  clim <- clim[,c(4,1,3,2,5)]
if(nrow(clim) == 0){
  next
}
  clim$year <- year(clim$Date)
  ### crop last year to full.time.frame.end.year
  clim <- clim[clim$year <= end.year, ]
clim <- clim[!duplicated(clim$Date),]
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


    corr.dcc.output <- my.dcc(chrono = core["res"], clim = clim[, c("year", "month", v)], method = "correlation", start = -1, end =  10, timespan = c(start.year, end.year), ci = 0.05, ci2 = 0.002)
    all.dcc.output <- rbind(all.dcc.output, data.frame(cbind(Site = site, Species = substr(f, nchar(f)-3, nchar(f)), corr.dcc.output)))#

  }

}
#unique(all_Clim$sites.sitename)
all.dcc.output$variable <- substr(paste(row.names(all.dcc.output)), 1, 3)#get variable from row name
all.dcc.output$month <- substr(paste(row.names(all.dcc.output)), 5, 12)#get month from row name

write.csv(all.dcc.output, file = "results/tree_cores/quiltplots/plot_data/Other/Other_core_corr_EXTENDED.csv", row.names = FALSE)

# all.dcc.output2 <- all.dcc.output %>%
#   mutate(Species = str_c(Site, Species, sep = "_")) %>%
#   select(-Site)
#   ### plot ####
#   #############################################
#   ##Copy/Paste this section from other script##
#   #############################################
#   save.plots = TRUE
# all.dcc.output$site <- all.dcc.output$Species
# all.dcc.output$Species <- substr(all.dcc.output$Species, nchar(all.dcc.output$Species)-3, nchar(all.dcc.output$Species))
#
# #write.csv(all.dcc.output, file = "all.dcc.output_other.csv", row.names = FALSE)
# library(readr)
# #Merge dcc outputs to plot on the same quilt plot
# all_dcc_output_hf <- read_csv("results/all.dcc.output_hf.csv")
# all_dcc_output_hf$site <- paste0("HF_", all_dcc_output_hf$Species)
# all_dcc_output_other <- read_csv("results/all.dcc.output_other.csv")
# all_dcc_output_scbi <- read_csv("results/scbi_core_corr.csv")
# all_dcc_output_scbi$site <- paste0("SCBI_", all_dcc_output_scbi$Species)
#
# all.dcc.output_all <- rbind(all_dcc_output_other,all_dcc_output_scbi,all_dcc_output_hf)
#
# #Load in clime means
# clim_means <- read_csv("clim_means_all.csv")
# #Create porosity lists
# RP <- c("CAGL","CAOV","CATO","CACO","QURU", "QUST", "QUAL","QUPR","QUMO", "FRAM", "QUVE", "FRNI","QUMA", "QUPA")
# SP <- c( "JUNI", "SAAL")
# DP <- c("FAGR", "LITU", "MAAC", "ACSA","ACRU", "NYSY","BELE","BEAL", "POGR")
# #creat wood_type column for subsetting in the forloop
# all.dcc.output_all$wood_type <-  ifelse(all.dcc.output_all$Species %in% RP, "RP",
#        ifelse(all.dcc.output_all$Species %in% DP, "DP",
#               ifelse(all.dcc.output_all$Species %in% SP, "SP", NA)))
# wood_types <- c("RP","SP", "DP")
#
# v <- "tmn"
# climate_variables <- c("tmn","tmx")
# WT <- "RP"
# for(WT in wood_types){
#   all.dcc.output <- all.dcc.output_all[all.dcc.output_all$wood_type %in% WT,]
#   for(v in climate_variables) {
#     print(v)
#
# #TRW_coord$Location
#     TRW_coord <- TRW_coord[!(duplicated(TRW_coord$Location)),]
#     X <- all.dcc.output[all.dcc.output$variable %in% v, ]
#     X$Location <- ifelse(X$site =="SCBI", "SCBI",
#                          ifelse(X$site == "HF", "HF",
#                                 substr(X$site, 1, nchar(X$site)-5)))
#     #X$numid <- seq(1,8,1)
#      X <- X %>%
#       mutate(
#         month_new = case_when(
#           month == "curr.jan" ~ 1,
#           month == "curr.feb" ~ 2,
#           month == "curr.mar" ~ 3,
#           month == "curr.apr" ~ 4,
#           month == "curr.may" ~ 5,
#           month == "curr.jun" ~ 6,
#           month == "curr.jul" ~ 7,
#           month == "curr.aug" ~ 8,
#           TRUE ~ 0
#         )
#       )
#     X <- X[X$month_new != 0,]
#
#     #ctrl shift c
#
#     #SORT BY LATITUDE
#     # X <- X %>%
#     #   left_join(TRW_coord, by = "Location")
#     #
#     # X <- X %>%
#     #   arrange(desc(Latitude), Species, numid)
#     #
#
#     #SORT BY APRIL TEMP
#     X <- X %>%
#       left_join(clim_means, by = "Location") %>%
#       group_by(site)
#
#     X <- X %>%
#       ungroup()%>%
#       #arrange(Species,Location, desc(v),month_new)
#       arrange(tmn, site, month_new)#, .by_group = TRUE)
#
#     X$site <- as.factor(X$site)
#     X$month <- as.factor(X$month)
#     #X <- merge(X,TRW_coord$Latitude, all.x = TRUE, all.y = FALSE)
#     #X <- X[order(as.numeric(X$Latitude), X$numid, X$Species),]
#
#
#
#     x <- X[, c("month", "site", "coef")]
#
#     x <- x %>%
#       pivot_wider(names_from = site,
#               id_cols = month,
#               values_from = coef)%>%
#       as.data.frame()
# #x <- data.frame(reshape(data = X[, c("month","site", "coef")], idvar = "month", timevar = "site",v.names = "coef", direction = "wide"))
#
#     rownames(x) <- ifelse(grepl("curr",  x$month), toupper(x$month), tolower( x$month))
#     rownames(x) <- gsub(".*curr.|.*prev.", "",   rownames(x), ignore.case = T)
#
#     x.sig <- reshape(X[, c("month", "site", "significant")], idvar = "month", timevar = "site", direction = "wide")
#     x.sig2 <- reshape(X[, c("month", "site", "significant2")], idvar = "month", timevar = "site", direction = "wide")
#
#     colnames(x) <- gsub("coef.", "", colnames(x))#Here is naming issue. Fixed by multiple column?
#     colnames(x.sig) <- gsub("significant.", "", colnames(x.sig))
#     colnames(x.sig2) <- gsub("significant2.", "", colnames(x.sig2))
#
#     x <- x[, -1] #Remove column since only looking at curr yr
#     x.sig <- x.sig[, -1]
#     x.sig2 <- x.sig2[, -1]
#
#     # x <- x[, rev(SPECIES_IN_ORDER[!SPECIES_IN_ORDER %in% gsub("CAOVL", "CAOV", species_to_drop)])]
#     #  x.sig <- x.sig[, rev(SPECIES_IN_ORDER[!SPECIES_IN_ORDER %in% gsub("CAOVL", "CAOV", species_to_drop)])]
#     #  x.sig2 <- x.sig2[, rev(SPECIES_IN_ORDER[!SPECIES_IN_ORDER %in% gsub("CAOVL", "CAOV", species_to_drop)])]
#
#     # if(save.plots)  {
#     #    dir.create(paste0("results/", type.start, "/figures/monthly_", method.to.run), showWarnings = F)
#     #    dir.create(paste0("results/", type.start, "/figures/monthly_", method.to.run, "/", c), showWarnings = F)
#     #    tiff(paste0("results/", type.start, "/figures/monthly_", method.to.run, "/", c, "/", v, ".tif"), res = 150, width = 169, height = 169, units = "mm", pointsize = 10)
#     #  }
#
#     v <-  toupper(v)
#     v <- gsub("PDSI_PREWHITEN" , "PDSI", v)
#     #x <- x[,c(2,1,3)]
#     #x.sig <- x.sig[,c(2,1,3)]
#     #x.sig2 <- x.sig2[,c(2,1,3)]
#     png(paste0("results/", "monthly_", "correlation", "other", v,WT, ".png"), res = 150, width = 169, height = 2*169, units = "mm", pointsize = 10)
#
#     my.dccplot(x = as.data.frame(t(x)), sig = as.data.frame(t(x.sig)), sig2 = as.data.frame(t(x.sig2)),  main = ifelse(v %in% "PETminusPRE", "PET-PRE", v), method = "correlation")
#
#     if(save.plots) dev.off()
#   }
# }
#
#
# all.dcc.output$variable <- substr(paste(row.names(all.dcc.output)), 1, 3)#get variable from row name
# all.dcc.output$month <- substr(paste(row.names(all.dcc.output)), 5, 12)#get month from row name
#
