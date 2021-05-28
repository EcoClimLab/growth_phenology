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
