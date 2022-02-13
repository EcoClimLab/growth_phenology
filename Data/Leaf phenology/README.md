# Description of Files
Raw data
- `scbiEVI2.csv` = EVI2 values for SCBI and Harvard Forest calculated from Bands 1 and 2 of MCD43A4 via Google Earth Engine (see `scbiPhenData.js`).
- `scbiPhen.csv` = Dates of key phenometrics (phenology thresholds) for SCBI and Harvard Forest obained from MCD12Q2 v006 via Google Earth Engine.

Filtered data
- `scbiPhenFiltered.csv` & `scbiEVI2Filtered.csv` = same as their respective versions, but the values have been filtered to only include those that had a QA flag of "good" or "best". For details, please see the script `scbiPhenQA.R` [here](https://github.com/EcoClimLab/growth_phenology/tree/master/RScripts).

Google Earth Engine
- `scbiPhenData.js` is a record of the GEE script used to generate the raw data. Alternatively, please see the script [here](https://code.earthengine.google.com/0b14ff8145db942d54f4ceb1f2bcddc0), which needs [this asset](https://code.earthengine.google.com/?asset=users/imcgreg/forestGEOsites) to run.
