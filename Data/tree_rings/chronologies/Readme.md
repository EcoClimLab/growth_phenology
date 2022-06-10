# Chronologies data

This folder contains final RWI chronologies, as analyzed in this study.

## Chronology development

For each species-site combination, we converted tree-ring records into the dimensionless RWI to emphasize interannual variability associated with climate. A 2/3rds n spline was applied to each core using ARSTAN V49_1b to produce standardized ring-width series; n is the number of years in each series. An adaptive power transformation, a process that also stabilizes the variance over time, was used to minimize the influence of outliers in all series. Low series replication, often in the earliest portions of a chronology collection, can also inflate the variance of tree-ring records. The 1/3rds spline method was chosen when replication in the inner portion of each chronology (the earliest ca. 30–50 yr of each record depending on full chronology length) was less than three trees. When replication was greater than n = 3 trees, we used the average correlation between raw ring-width series (rbar) method. The robust biweight mean chronology (RWI) for each species-site combination was calculated from the ring-width indices following variance stabilization. We defined chronology start year as the year where subsample signal strength (SSS) passed a threshold of SSS = 0.8, or where ≥80% of the population signal was captured in the chronology.

## Explanation of files
Chronologies are organized into folders by site name. 
Within each folder, each species is represented by a separate .csv file named with a 4-letter species code (see [Species.csv](https://github.com/EcoClimLab/growth_phenology/blob/master/Data/tree_rings/Species.csv)) appended to the site name.

all_crns_res_1901.csv contains all chronologies in the "wide" format. 

crns_long.csv contains all chronologies in the "long" format. 
