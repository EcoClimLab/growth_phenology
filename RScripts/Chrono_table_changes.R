#Script to clean up chronology table after creation in Create_final_quiltplot.r

library(readr)
chronology_table <- read_csv("doc/manuscript/tables_figures/chronology_table.csv")
#RP <- c("CAGL","CAOV","CATO","CACO","QURU", "QUST", "QUAL","QUPR","QUMO", "FRAM", "QUVE", "FRNI","QUMA", "QUPA")
#SP <- c( "JUNI", "SAAL")
#DP <- c("FAGR", "LITU", "MAAC", "ACSA","ACRU", "NYSY","BELE","BEAL", "POGR")

chronology_table$Species <- ifelse(chronology_table$Species == "CAGL","Carya glabra",
                                   ifelse(chronology_table$Species == "CATO","Carya tomentosa",
                                   ifelse(chronology_table$Species == "CAOV","Carya ovata",
                                          ifelse(chronology_table$Species == "CACO","Carya cordiformis",
                                                 ifelse(chronology_table$Species == "QURU","Quercus rubra",
                                                        ifelse(chronology_table$Species == "QUST","Quercus stellata",
                                                               ifelse(chronology_table$Species == "QUAL","Quercus alba",
                                                                      ifelse(chronology_table$Species == "QUPR","Quercus prinus",
                                                                             ifelse(chronology_table$Species == "QUMO","Quercus mongolica",
                                                                                    ifelse(chronology_table$Species == "FRAM","Fraxinus americana",
                                                                                           ifelse(chronology_table$Species == "QUVE","Quercus velutina",
                                                                                                  ifelse(chronology_table$Species == "FRNI","Fraxinus nigra",
                                                                                                         ifelse(chronology_table$Species == "QUMA","Quercus macrocarpa",
                                                                                                                ifelse(chronology_table$Species == "QUPA","Quercus Pagoda",
                                                                                                                       ifelse(chronology_table$Species == "FAGR","Fagus grandifolia",
                                                                                                                              ifelse(chronology_table$Species == "LITU","Liriodendron tulipifera",
                                                                                                                                     ifelse(chronology_table$Species == "MAAC","Magnolia acuminata",
                                                                                                                                            ifelse(chronology_table$Species == "ACSA","Acer saccharum",
                                                                                                                                                   ifelse(chronology_table$Species == "ACRU","Acer rubrum",
                                                                                                                                                          ifelse(chronology_table$Species == "NYSY","Nyssa sylvatica",
                                                                                                                                                                 ifelse(chronology_table$Species == "BELE","Betula lenta",
                                                                                                                                                                        ifelse(chronology_table$Species == "BEAL","Betula alleghaniensis",
                                                                                                                                                                               ifelse(chronology_table$Species == "POGR","Populus grandidentata",NA)))))))))))))))))))))))
chronology_table <- chronology_table[!(duplicated(chronology_table$group)),]
write.csv(chronology_table, file = "Doc/manuscript/tables_figures/chronology_table.csv", row.names = FALSE)

#Add correlation coefficients from quilt plot
library(readr)
library(readxl)
library(dplyr)
library(tidyverse)
chronology_table <- read_csv("doc/manuscript/tables_figures/chronology_table.csv")
tmx_quilt_plot_data <- read_csv("Data/tmx_quilt_plot_data.csv")
tmx_quilt_plot_data$sig <- ifelse(tmx_quilt_plot_data$significant == "FALSE", "NS",
                                  ifelse(tmx_quilt_plot_data$significant == "TRUE" & tmx_quilt_plot_data$significant2 == "FALSE", "<0.05",
                                         ifelse(tmx_quilt_plot_data$significant2 == "TRUE", "<0.002", NA)))

tmx_quilt_plot_data <- tmx_quilt_plot_data[,c(19,12,2,20)]

tmx_quilt_plot_data <- tmx_quilt_plot_data %>%
  pivot_wider(id_cols = group,
              names_from = month,
              values_from = c(coef, sig)) #%>%
 # rename(Number = group)

chronology_table <- left_join(chronology_table, tmx_quilt_plot_data, by = "group")

chronology_table <- chronology_table %>%
  rename(January = coef_curr.jan,
         February = coef_curr.feb,
         March = coef_curr.mar,
         April = coef_curr.apr,
         May = coef_curr.may,
         June = coef_curr.jun,
         July = coef_curr.jul,
         August = coef_curr.aug,
         "January Sig" = sig_curr.jan,
         "February Sig" = sig_curr.feb,
         "March Sig" = sig_curr.mar,
         "April Sig" = sig_curr.apr,
         "May Sig" = sig_curr.may,
         "June Sig" = sig_curr.jun,
         "July Sig" = sig_curr.jul,
         "August Sig" = sig_curr.aug)
chronology_table <- chronology_table[, c(1:8, 9,17,10,18,11,19,12,20,13,21,14,22,15,23,16,24)]
write.csv(chronology_table, file = "Doc/manuscript/tables_figures/chronology_table.csv", row.names = FALSE)
