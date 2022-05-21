#Script to create qulit plot for growth phenology project#
##########################################################
rm(list = ls())

set.seed(42)

library(readr)
library(readxl)
library(tidyverse)
#Valentine's functions
source("Rscripts/0-My_dplR_functions.R")

#Load in lat lon for plotting sort
TRW_coord <- read_excel("data/tree_rings/TRW_coord2.xlsx")
TRW_coord <- TRW_coord[,c(1,3)]
#Add original two locations to include in final quilt plot
originals <- data.frame(42.5388, "HF") #Lat for HF = 42.5388
originals <- rbind(originals, c(38.8935, "SCBI")) #Lat for scbi = 38.8935
names(originals) <- c("Latitude", "Location")
TRW_coord <- rbind(TRW_coord, originals)

#Merge dcc outputs to plot on the same quilt plot
all_dcc_output_hf <- read_csv("Results/tree_cores/quiltplots/plot_data/HF/all.dcc.output_hf.csv")#created in HF_quiltplot.R
all_dcc_output_hf$site <- paste0("HF_", all_dcc_output_hf$Species)
all_dcc_output_other <- read_csv("Results/tree_cores/quiltplots/plot_data/Other/all.dcc.output_other.csv")#created in Other_quiltplot.R
all_dcc_output_other$site <- paste0(all_dcc_output_other$Site,"_", all_dcc_output_other$Species)
all_dcc_output_other <- all_dcc_output_other[,-1]
all_dcc_output_other <- all_dcc_output_other[!(all_dcc_output_other$site %in% "MO_Flu_CAOV"),]
all_dcc_output_scbi <- read_csv("Results/tree_cores/quiltplots/plot_data/SCBI/scbi_core_corr.csv")#Created in scbi_quiltplot.R
all_dcc_output_scbi$site <- paste0("SCBI_", all_dcc_output_scbi$Species)

all.dcc.output_all <- rbind(all_dcc_output_other,all_dcc_output_scbi,all_dcc_output_hf)

#Load in clim means
clim_means <- read_csv("Results/tree_cores/quiltplots/plot_data/clim_means_all.csv")#Created in other_quiltplot.R
clim_means$tmx <- ifelse(clim_means$Location == "HF_LyfordPlots", clim_means$tmx+0.01, clim_means$tmx)#added to allow HF_lyford plots to be plotted separately from HF, even though spring means were the same. Does not affect analyses
#Create porosity lists
RP <- c("CAGL","CAOV","CATO","CACO","QURU", "QUST", "QUAL","QUPR","QUMO", "FRAM", "QUVE", "FRNI","QUMA", "QUPA")
SP <- c( "JUNI", "SAAL")
DP <- c("FAGR", "LITU", "MAAC", "ACSA","ACRU", "NYSY","BELE","BEAL", "POGR")
#create wood_type column for subsetting in the forloop
all.dcc.output_all$wood_type <-  ifelse(all.dcc.output_all$Species %in% RP, "RP",
                                        ifelse(all.dcc.output_all$Species %in% DP, "DP",
                                               ifelse(all.dcc.output_all$Species %in% SP, "SP", NA)))
#Objects for for loops
wood_types <- c("RP","SP", "DP")
climate_variables <- c("tmn","tmx")
save.plots <- TRUE

# all.dcc.output_all_plt1 <- subset(all.dcc.output_all, all.dcc.output_all$wood_type == "RP" & all.dcc.output_all$variable == "tmx")
#
# all.dcc.output_all_plt1 <- all.dcc.output_all_plt1 %>% #number each month for sorting
#   mutate(
#     month_new = case_when(
#       month == "curr.jan" ~ 1,
#       month == "curr.feb" ~ 2,
#       month == "curr.mar" ~ 3,
#       month == "curr.apr" ~ 4,
#       month == "curr.may" ~ 5,
#       month == "curr.jun" ~ 6,
#       month == "curr.jul" ~ 7,
#       month == "curr.aug" ~ 8,
#       TRUE ~ 0
#     )
#   )
# all.dcc.output_all_plt1 <- all.dcc.output_all_plt1[all.dcc.output_all_plt1$month_new != 0,]#remove months outside of desired Jan-Aug range. SCBI dcc extended to september, so need to remove the september rows
#
# all.dcc.output_all_plt1 <- all.dcc.output_all_plt1 %>%
#   ungroup()%>%
#   arrange(tmx, site, month_new)
#
# data2 <- data2 %>%                              # Create numbering variable
#   group_by(group) %>%
#   mutate(numbering = row_number())
#Forloop to create plots. Cycles through wood types (RP, DP, SP) & clim variables (tmx, tmn)
#Sorts by average April temp across all years of core data at each site
#First, create numbering scheme and prepare data


#TMIN LOOP
for(v in climate_variables) {
  v <- "tmn"
  print(v)
  TRW_coord <- TRW_coord[!(duplicated(TRW_coord$Location)),]#removes duplicate locations added by original author of TRW_coord excel sheet
  X <- all.dcc.output_all[all.dcc.output_all$variable %in% "tmn", ]#subset core by clim variable
  X$Location <- ifelse(X$site =="SCBI", "SCBI",#assigns value of location
                       ifelse(X$site == "HF", "HF",
                              substr(X$site, 1, nchar(X$site)-5)))

  X <- X %>% #number each month for sorting
    mutate(
      month_new = case_when(
        month == "curr.jan" ~ 1,
        month == "curr.feb" ~ 2,
        month == "curr.mar" ~ 3,
        month == "curr.apr" ~ 4,
        month == "curr.may" ~ 5,
        month == "curr.jun" ~ 6,
        month == "curr.jul" ~ 7,
        month == "curr.aug" ~ 8,
        TRUE ~ 0
      )
    )
  X <- X[X$month_new != 0,]#remove months outside of desired Jan-Aug range. SCBI dcc extended to september, so need to remove the september rows

  #SORT BY LATITUDE
  # X <- X %>%
  #   left_join(TRW_coord, by = "Location")
  #
  # X <- X %>%
  #   arrange(desc(Latitude), Species, numid)
  #

  #SORT BY APRIL TEMP
  X <- X %>%
    left_join(clim_means, by = "Location") %>%
    group_by(site)
X <- X[X$wood_type != "SP",]

  X <- X %>%
    ungroup()%>%
    arrange( desc(wood_type), tmn, site, month_new)

  X$site <- as.factor(X$site)
  X$month <- as.factor(X$month)

  X <- X[!(is.na(X$wood_type)),]

  #levels(X$site)
  X <- X %>%
  #group_by(site) %>%
  #mutate(id = as.integer(site) )
    #mutate(site = as.character(site))%>%
    mutate(site = as.character(site)) %>%
  mutate(group = match(site, unique(site)))
# SI_table <- X[,c(19,1,14,15,17,18)]
# write.csv(SI_table, file = paste0("doc/manuscript/tables_figures/", "chronology_table.csv"), row.names = FALSE)
  # X$entry_number <- seq(1, nrow(X)/8, 1)
  #
  write.csv(X, file = paste0("results/tree_cores/quiltplots/plot_data/all/", v, "_quilt_plot_data.csv"), row.names = FALSE)
}

#TMAX LOOP
for(v in climate_variables) {
  v <- "tmx"
  print(v)
  TRW_coord <- TRW_coord[!(duplicated(TRW_coord$Location)),]#removes duplicate locations added by original author of TRW_coord excel sheet
  X <- all.dcc.output_all[all.dcc.output_all$variable %in% "tmx", ]#subset core by clim variable
  X$Location <- ifelse(X$site =="SCBI", "SCBI",#assigns value of location
                       ifelse(X$site == "HF", "HF",
                              substr(X$site, 1, nchar(X$site)-5)))

  X <- X %>% #number each month for sorting
    mutate(
      month_new = case_when(
        month == "curr.jan" ~ 1,
        month == "curr.feb" ~ 2,
        month == "curr.mar" ~ 3,
        month == "curr.apr" ~ 4,
        month == "curr.may" ~ 5,
        month == "curr.jun" ~ 6,
        month == "curr.jul" ~ 7,
        month == "curr.aug" ~ 8,
        TRUE ~ 0
      )
    )
  X <- X[X$month_new != 0,]#remove months outside of desired Jan-Aug range. SCBI dcc extended to september, so need to remove the september rows

  #SORT BY LATITUDE
  # X <- X %>%
  #   left_join(TRW_coord, by = "Location")
  #
  # X <- X %>%
  #   arrange(desc(Latitude), Species, numid)
  #

  #SORT BY APRIL TEMP
  X <- X %>%
    left_join(clim_means, by = "Location") %>%
    group_by(site)
  X <- X[X$wood_type != "SP",]

  X <- X %>%
    ungroup()%>%
    arrange( desc(wood_type), tmx, site, month_new)

  X$site <- as.factor(X$site)
  X$month <- as.factor(X$month)

  X <- X[!(is.na(X$wood_type)),]

  #levels(X$site)
  X <- X %>%
    #group_by(site) %>%
    #mutate(id = as.integer(site) )
    #mutate(site = as.character(site))%>%
    mutate(site = as.character(site)) %>%
    mutate(group = match(site, unique(site)))
  # SI_table <- X[,c(19,1,14,15,17,18)]
  # write.csv(SI_table, file = paste0("doc/manuscript/tables_figures/", "chronology_table.csv"), row.names = FALSE)
  # X$entry_number <- seq(1, nrow(X)/8, 1)
  #
  write.csv(X, file = paste0("results/tree_cores/quiltplots/plot_data/all/", v, "_quilt_plot_data.csv"), row.names = FALSE)
}
#}

TRW_coord <- read_excel("data/tree_rings/TRW_coord2.xlsx")
TRW_coord <- TRW_coord[,c(1,2,3)]
#Add original two locations to include in final quilt plot
originals <- data.frame(42.5388,-72.17550, "HF") #Lat for HF = 42.5388
originals <- rbind(originals, c(38.8935,-78.14540, "SCBI")) #Lat for scbi = 38.8935
names(originals) <- c("Latitude","Longitude", "Location")
TRW_coord <- rbind(TRW_coord, originals)

tmx_data <- read.csv("results/tree_cores/quiltplots/plot_data/all/tmx_quilt_plot_data.csv")
tmx_data
tmx_data_table <- tmx_data[,c(19,15,1,14,17,18)]
tmx_data_table <- tmx_data_table[!(duplicated(tmx_data_table$group)),]
tmx_data_table$Location <- ifelse(tmx_data_table$Location == "Fiddler<U+393C><U+3E32>s_Green,_VA_CAOV_CAOV", "Fiddler's_Green,_VA_CAOV",
                         ifelse(tmx_data_table$Location == "Fiddler<U+393C><U+3E32>s_Green,_VA_LITU_LITU", "Fiddler's_Green,_VA_LITU",
                                ifelse(tmx_data_table$Location == "Fiddler<U+393C><U+3E32>s_Green,_VA_MAAC_MAAC", "Fiddler's_Green,_VA_MAAC",
                                       ifelse(tmx_data_table$Location == "Fiddler<U+393C><U+3E32>s_Green_QURU_QURU", "Fiddler's_Green_QURU", tmx_data_table$Location))))

tmx_data_table <- left_join(tmx_data_table, TRW_coord, by = "Location")
tmx_data_table <- tmx_data_table[!duplicated(tmx_data_table$group),]
#write.csv(tmx_data_table, "doc/manuscript/tables_figures/chronology_table.csv", row.names = FALSE)

tmn_data <- read.csv("results/tree_cores/quiltplots/plot_data/all/tmn_quilt_plot_data.csv")
tmn_data <- tmn_data[!(tmn_data$site %in% "MO_Flu_CAOV"), ]
tmn_data_table <- tmn_data[,c(19,15,1,14,17,18)]
tmn_data_table <- tmn_data_table[!(duplicated(tmn_data_table$group)),]
tmn_data_table$Location <- ifelse(tmn_data_table$Location == "Fiddler<U+393C><U+3E32>s_Green,_VA_CAOV_CAOV", "Fiddler's_Green,_VA_CAOV",
                                  ifelse(tmn_data_table$Location == "Fiddler<U+393C><U+3E32>s_Green,_VA_LITU_LITU", "Fiddler's_Green,_VA_LITU",
                                         ifelse(tmn_data_table$Location == "Fiddler<U+393C><U+3E32>s_Green,_VA_MAAC_MAAC", "Fiddler's_Green,_VA_MAAC",
                                                ifelse(tmn_data_table$Location == "Fiddler<U+393C><U+3E32>s_Green_QURU_QURU", "Fiddler's_Green_QURU", tmn_data_table$Location))))

tmn_data_table <- left_join(tmn_data_table, TRW_coord, by = "Location")
tmn_data_table <- tmn_data_table[!(duplicated(tmn_data_table$group)),]
#write.csv(tmn_data_table, "doc/manuscript/tables_figures/tmn_chronology_table.csv", row.names = FALSE)


#RP TMAXc----
#for(WT in wood_types){#Have to manually change WT and run through the contents of the loop. Not sure whats going wrong
  v <- "tmx"

  WT <- "RP"
  #all.dcc.output <- all.dcc.output_all[all.dcc.output_all$wood_type %in% WT,]#subset by wood type
  X <- tmx_data[tmx_data$wood_type %in% WT,]#subset by wood type.CHANGE!!!!!

  # for(v in climate_variables) {
  #   print(v)

    TRW_coord <- TRW_coord[!(duplicated(TRW_coord$Location)),]#removes duplicate locations added by original author of TRW_coord excel sheet
    # X <- all.dcc.output[all.dcc.output$variable %in% v, ]#subset core by clim variable
    # X$Location <- ifelse(X$site =="SCBI", "SCBI",#assigns value of location
    #                      ifelse(X$site == "HF", "HF",
    #                             substr(X$site, 1, nchar(X$site)-5)))

    # X <- X %>% #number each month for sorting
    #   mutate(
    #     month_new = case_when(
    #       month == "curr.jan" ~ 1,
    #       month == "curr.feb" ~ 2,
    #       month == "curr.mar" ~ 3,
    #       month == "curr.apr" ~ 4,
    #       month == "curr.may" ~ 5,
    #       month == "curr.jun" ~ 6,
    #       month == "curr.jul" ~ 7,
    #       month == "curr.aug" ~ 8,
    #       TRUE ~ 0
    #     )
    #   )
    X <- X[X$month_new != 0,]#remove months outside of desired Jan-Aug range. SCBI dcc extended to september, so need to remove the september rows

    #SORT BY LATITUDE
    # X <- X %>%
    #   left_join(TRW_coord, by = "Location")
    #
    # X <- X %>%
    #   arrange(desc(Latitude), Species, numid)
    #

    #SORT BY APRIL TEMP
    # X <- X %>%
    #   left_join(clim_means, by = "Location") %>%
    #   group_by(site)

    X <- X %>%
      ungroup()%>%
      arrange(tmx, site, month_new) #CHANGE!!!!!!!!!

    X$site <- as.factor(X$site)
    X$month <- as.factor(X$month)


    #COnvert from long to wide
    x <- X[, c("month", "group", "coef")]
    x <- x %>%
      pivot_wider(names_from = group,
                  id_cols = month,
                  values_from = coef)%>%
                  as.data.frame()
    #x <- data.frame(reshape(data = X[, c("month","site", "coef")], idvar = "month", timevar = "site",v.names = "coef", direction = "wide"))

    rownames(x) <- ifelse(grepl("curr",  x$month), toupper(x$month), tolower( x$month))
    rownames(x) <- gsub(".*curr.|.*prev.", "",   rownames(x), ignore.case = T)

    x.sig <- X[, c("month", "group", "significant")]
    x.sig <- x.sig %>%
      pivot_wider(names_from = group,
                  id_cols = month,
                  values_from = significant)%>%
                  as.data.frame()   #reshape(X[, c("month", "site", "significant")], idvar = "month", timevar = "site", direction = "wide")

    x.sig2 <- X[, c("month", "group", "significant2")]
    x.sig2 <- x.sig2 %>%
      pivot_wider(names_from = group,
                  id_cols = month,
                  values_from = significant2)%>%
                  as.data.frame()#reshape(X[, c("month", "site", "significant2")], idvar = "month", timevar = "site", direction = "wide")

    colnames(x) <- gsub("coef.", "", colnames(x))
    colnames(x.sig) <- gsub("significant.", "", colnames(x.sig))
    colnames(x.sig2) <- gsub("significant2.", "", colnames(x.sig2))

    x <- x[, -1] #Remove column since only looking at curr yr
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
#colnames(x) <- seq(1, ncol(x), 1)
    v <-  toupper(v)
    v <- gsub("PDSI_PREWHITEN" , "PDSI", v)
    #x <- x[,c(2,1,3)]
    #x.sig <- x.sig[,c(2,1,3)]
    #x.sig2 <- x.sig2[,c(2,1,3)]
    png(paste0("results/tree_cores/quiltplots/plot_images/", "monthly_", "correlation", "other", v,WT, ".png"), res = 150, width = 18, height = 17, units = "cm", pointsize = 10)

    my.dccplot(x = as.data.frame(t(x)), sig = as.data.frame(t(x.sig)), sig2 = as.data.frame(t(x.sig2)),  main = ifelse(v %in% "PETminusPRE", "PET-PRE", v), method = "correlation")

    if(save.plots) dev.off()
#  }

#DP TMAX ----
v <- "tmx"
WT <- "DP"
#all.dcc.output <- all.dcc.output_all[all.dcc.output_all$wood_type %in% WT,]#subset by wood type
X <- tmx_data[tmx_data$wood_type %in% WT,]#subset by wood type.CHANGE!!!!!

TRW_coord <- TRW_coord[!(duplicated(TRW_coord$Location)),]#removes duplicate locations added by original author of TRW_coord excel sheet

X <- X[X$month_new != 0,]#remove months outside of desired Jan-Aug range. SCBI dcc extended to september, so need to remove the september rows

X <- X %>%
  ungroup()%>%
  arrange(tmx, site, month_new) #CHANGE!!!!!!!!!

X$site <- as.factor(X$site)
X$month <- as.factor(X$month)


#COnvert from long to wide
x <- X[, c("month", "group", "coef")]
x <- x %>%
  pivot_wider(names_from = group,
              id_cols = month,
              values_from = coef)%>%
  as.data.frame()

rownames(x) <- ifelse(grepl("curr",  x$month), toupper(x$month), tolower( x$month))
rownames(x) <- gsub(".*curr.|.*prev.", "",   rownames(x), ignore.case = T)

x.sig <- X[, c("month", "group", "significant")]
x.sig <- x.sig %>%
  pivot_wider(names_from = group,
              id_cols = month,
              values_from = significant)%>%
  as.data.frame()   #reshape(X[, c("month", "site", "significant")], idvar = "month", timevar = "site", direction = "wide")

x.sig2 <- X[, c("month", "group", "significant2")]
x.sig2 <- x.sig2 %>%
  pivot_wider(names_from = group,
              id_cols = month,
              values_from = significant2)%>%
  as.data.frame()#reshape(X[, c("month", "site", "significant2")], idvar = "month", timevar = "site", direction = "wide")

colnames(x) <- gsub("coef.", "", colnames(x))
colnames(x.sig) <- gsub("significant.", "", colnames(x.sig))
colnames(x.sig2) <- gsub("significant2.", "", colnames(x.sig2))

x <- x[, -1] #Remove column since only looking at curr yr
x.sig <- x.sig[, -1]
x.sig2 <- x.sig2[, -1]

v <-  toupper(v)
v <- gsub("PDSI_PREWHITEN" , "PDSI", v)

png(paste0("results/tree_cores/quiltplots/plot_images/", "monthly_", "correlation", "other", v,WT, ".png"), res = 150, width = 8.45, height = 2*8.45, units = "cm", pointsize = 10)

my.dccplot(x = as.data.frame(t(x)), sig = as.data.frame(t(x.sig)), sig2 = as.data.frame(t(x.sig2)),  main = ifelse(v %in% "PETminusPRE", "PET-PRE", v), method = "correlation")

if(save.plots) dev.off()

#RP TMIN ----
v <- "tmn"
WT <- "RP"
#all.dcc.output <- all.dcc.output_all[all.dcc.output_all$wood_type %in% WT,]#subset by wood type
X <- tmn_data[tmn_data$wood_type %in% WT,]#subset by wood type.CHANGE!!!!!

# for(v in climate_variables) {
#   print(v)

TRW_coord <- TRW_coord[!(duplicated(TRW_coord$Location)),]#removes duplicate locations added by original author of TRW_coord excel sheet

X <- X[X$month_new != 0,]#remove months outside of desired Jan-Aug range. SCBI dcc extended to september, so need to remove the september rows

X <- X %>%
  ungroup()%>%
  arrange(tmn, site, month_new) #CHANGE!!!!!!!!!

X$site <- as.factor(X$site)
X$month <- as.factor(X$month)


#COnvert from long to wide
x <- X[, c("month", "group", "coef")]
x <- x %>%
  pivot_wider(names_from = group,
              id_cols = month,
              values_from = coef)%>%
  as.data.frame()

rownames(x) <- ifelse(grepl("curr",  x$month), toupper(x$month), tolower( x$month))
rownames(x) <- gsub(".*curr.|.*prev.", "",   rownames(x), ignore.case = T)

x.sig <- X[, c("month", "group", "significant")]
x.sig <- x.sig %>%
  pivot_wider(names_from = group,
              id_cols = month,
              values_from = significant)%>%
  as.data.frame()   #reshape(X[, c("month", "site", "significant")], idvar = "month", timevar = "site", direction = "wide")

x.sig2 <- X[, c("month", "group", "significant2")]
x.sig2 <- x.sig2 %>%
  pivot_wider(names_from = group,
              id_cols = month,
              values_from = significant2)%>%
  as.data.frame()#reshape(X[, c("month", "site", "significant2")], idvar = "month", timevar = "site", direction = "wide")

colnames(x) <- gsub("coef.", "", colnames(x))
colnames(x.sig) <- gsub("significant.", "", colnames(x.sig))
colnames(x.sig2) <- gsub("significant2.", "", colnames(x.sig2))

x <- x[, -1] #Remove column since only looking at curr yr
x.sig <- x.sig[, -1]
x.sig2 <- x.sig2[, -1]

v <-  toupper(v)
v <- gsub("PDSI_PREWHITEN" , "PDSI", v)

png(paste0("results/tree_cores/quiltplots/plot_images/", "monthly_", "correlation", "other", v,WT, ".png"), res = 150, width = 8.45, height = 2*8.45, units = "cm", pointsize = 10)

my.dccplot(x = as.data.frame(t(x)), sig = as.data.frame(t(x.sig)), sig2 = as.data.frame(t(x.sig2)),  main = ifelse(v %in% "PETminusPRE", "PET-PRE", v), method = "correlation")

if(save.plots) dev.off()

#DP TMIN ----
v <- "tmn"
WT <- "DP"
#all.dcc.output <- all.dcc.output_all[all.dcc.output_all$wood_type %in% WT,]#subset by wood type
X <- tmn_data[tmn_data$wood_type %in% WT,]#subset by wood type.CHANGE!!!!!

TRW_coord <- TRW_coord[!(duplicated(TRW_coord$Location)),]#removes duplicate locations added by original author of TRW_coord excel sheet

X <- X[X$month_new != 0,]#remove months outside of desired Jan-Aug range. SCBI dcc extended to september, so need to remove the september rows

X <- X %>%
  ungroup()%>%
  arrange(tmn, site, month_new)

X$site <- as.factor(X$site)
X$month <- as.factor(X$month)


#COnvert from long to wide
x <- X[, c("month", "group", "coef")]
x <- x %>%
  pivot_wider(names_from = group,
              id_cols = month,
              values_from = coef)%>%
  as.data.frame()
#x <- data.frame(reshape(data = X[, c("month","site", "coef")], idvar = "month", timevar = "site",v.names = "coef", direction = "wide"))

rownames(x) <- ifelse(grepl("curr",  x$month), toupper(x$month), tolower( x$month))
rownames(x) <- gsub(".*curr.|.*prev.", "",   rownames(x), ignore.case = T)

x.sig <- X[, c("month", "group", "significant")]
x.sig <- x.sig %>%
  pivot_wider(names_from = group,
              id_cols = month,
              values_from = significant)%>%
  as.data.frame()   #reshape(X[, c("month", "site", "significant")], idvar = "month", timevar = "site", direction = "wide")

x.sig2 <- X[, c("month", "group", "significant2")]
x.sig2 <- x.sig2 %>%
  pivot_wider(names_from = group,
              id_cols = month,
              values_from = significant2)%>%
  as.data.frame()#reshape(X[, c("month", "site", "significant2")], idvar = "month", timevar = "site", direction = "wide")

colnames(x) <- gsub("coef.", "", colnames(x))
colnames(x.sig) <- gsub("significant.", "", colnames(x.sig))
colnames(x.sig2) <- gsub("significant2.", "", colnames(x.sig2))

x <- x[, -1] #Remove column since only looking at curr yr
x.sig <- x.sig[, -1]
x.sig2 <- x.sig2[, -1]
v <-  toupper(v)
v <- gsub("PDSI_PREWHITEN" , "PDSI", v)

png(paste0("results/tree_cores/quiltplots/plot_images/", "monthly_", "correlation", "other", v,WT, ".png"), res = 150, width = 8.45, height = 2*8.45, units = "cm", pointsize = 10)

my.dccplot(x = as.data.frame(t(x)), sig = as.data.frame(t(x.sig)), sig2 = as.data.frame(t(x.sig2)),  main = ifelse(v %in% "PETminusPRE", "PET-PRE", v), method = "correlation")

if(save.plots) dev.off()
#----
