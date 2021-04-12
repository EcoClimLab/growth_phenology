#Script to create qulit plot for growth phenology project#
##########################################################

library(readr)
library(readxl)
library(tidyverse)
#Valentine's functions
source("Rscripts/0-My_dplR_functions.R")

#Load in lat lon for plotting sort
TRW_coord <- read_excel("Data/tree_rings/Other/TRW_coord2.xlsx")
TRW_coord <- TRW_coord[,c(1,3)]
#Add original two locations to include in final quilt plot
originals <- data.frame(42.5388, "HF") #Lat for HF = 42.5388
originals <- rbind(originals, c(38.8935, "SCBI")) #Lat for scbi = 38.8935
names(originals) <- c("Latitude", "Location")
TRW_coord <- rbind(TRW_coord, originals)

#Merge dcc outputs to plot on the same quilt plot
all_dcc_output_hf <- read_csv("Results/all.dcc.output_hf.csv")#created in HF_quiltplot.R
all_dcc_output_hf$site <- paste0("HF_", all_dcc_output_hf$Species)
all_dcc_output_other <- read_csv("Results/all.dcc.output_other.csv")#created in Other_quiltplot.R
all_dcc_output_scbi <- read_csv("Results/scbi_core_corr.csv")#Created in scbi_quiltplot.R
all_dcc_output_scbi$site <- paste0("SCBI_", all_dcc_output_scbi$Species)

all.dcc.output_all <- rbind(all_dcc_output_other,all_dcc_output_scbi,all_dcc_output_hf)

#Load in clim means
clim_means <- read_csv("Results/clim_means_all.csv")#Created in other_quiltplot.R
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

#For loop to create plots. Cycles through wood types (RP, DP, SP) & clim variables (tmx, tmn)
#Sorts by average April temp across all years of core data at each site
for(WT in wood_types){
  all.dcc.output <- all.dcc.output_all[all.dcc.output_all$wood_type %in% WT,]#subset by wood type
  for(v in climate_variables) {
    print(v)

    TRW_coord <- TRW_coord[!(duplicated(TRW_coord$Location)),]#removes duplicate locations added by original author of TRW_coord excel sheet
    X <- all.dcc.output[all.dcc.output$variable %in% v, ]#subset core by clim variable
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

    X <- X %>%
      ungroup()%>%
      arrange(tmn, site, month_new)

    X$site <- as.factor(X$site)
    X$month <- as.factor(X$month)

    #COnvert from long to wide
    x <- X[, c("month", "site", "coef")]
    x <- x %>%
      pivot_wider(names_from = site,
                  id_cols = month,
                  values_from = coef)%>%
                  as.data.frame()
    #x <- data.frame(reshape(data = X[, c("month","site", "coef")], idvar = "month", timevar = "site",v.names = "coef", direction = "wide"))

    rownames(x) <- ifelse(grepl("curr",  x$month), toupper(x$month), tolower( x$month))
    rownames(x) <- gsub(".*curr.|.*prev.", "",   rownames(x), ignore.case = T)

    x.sig <- reshape(X[, c("month", "site", "significant")], idvar = "month", timevar = "site", direction = "wide")
    x.sig2 <- reshape(X[, c("month", "site", "significant2")], idvar = "month", timevar = "site", direction = "wide")

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

    v <-  toupper(v)
    v <- gsub("PDSI_PREWHITEN" , "PDSI", v)
    #x <- x[,c(2,1,3)]
    #x.sig <- x.sig[,c(2,1,3)]
    #x.sig2 <- x.sig2[,c(2,1,3)]
    png(paste0("results/", "monthly_", "correlation", "other", v,WT, ".png"), res = 150, width = 169, height = 2*169, units = "mm", pointsize = 10)

    my.dccplot(x = as.data.frame(t(x)), sig = as.data.frame(t(x.sig)), sig2 = as.data.frame(t(x.sig2)),  main = ifelse(v %in% "PETminusPRE", "PET-PRE", v), method = "correlation")

    if(save.plots) dev.off()
  }
}
