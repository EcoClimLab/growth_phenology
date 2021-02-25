# clear environment ####
rm(list = ls())

#tightly condensed plot by latitude.
#Two panels, rp/dp

# load libraries ####
library(bootRes)
library(dplR) # for read.rwl
library(climwin)
library(tidyverse)

#Prepare csv's
crns <- read.csv("Data/tree_rings/Other/all_crns_res_1901.csv")
TRW_coord <- read_excel("Data/tree_rings/Other/TRW_coord.xlsx")

# Bert approach
crns <- read_csv("Data/tree_rings/Other/all_crns_res_1901.csv") %>%
  # clean up
  select(-c(BearIs, OH_Gol_QUAL_1)) %>%
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
for(f in site_species) {
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
  clim <- all_Clim

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

write.csv(all.dcc.output, file = "results/Harvard_Forest_core_corr.csv", row.names = FALSE)
