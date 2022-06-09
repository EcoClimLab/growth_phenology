# R Scripts

All R code used for analysis is here:

## Main analysis

Run in the following order:

1. Run both `(HF/SCBI)_wood_phenology.R`: Fit logistic growth model to raw dendroband measurements to create  `Data/Wood_pheno_table_(HF/SCBI)_RAW.csv`and `LG5_parameter_values_(HarvardForest/SCBI)_RAW`
1. Run `data_cleaning.R`: Remove poorly fit models from `Data/Wood_pheno_table_(HF/SCBI)_RAW.csv`and `LG5_parameter_values_(HarvardForest/SCBI)_RAW` to create `Data/Wood_pheno_table_(HF/SCBI)_CLEAN.csv` and `LG5_parameter_values_(HarvardForest/SCBI)_CLEAN`
1. Run both `(HF/SCBI)_climwin_Cameron.R`: Run the climwin analysis to determine critical temperature windows for tmax and tmin
1. Run both `Pheno_tsensitivity_figure_tmax.R` and `Pheno_tsensitivity_figure_april_april_tmax.R`: Run Bayesian analysis of cleaned dendroband models to create outputs found in `results/bayesian_outputs`and `doc/manuscript/tables_figures/pheno_Tsensitivity_combo_patchwork_(AIC/april).png`


## Other analyses

HEY CAM I SPLIT OFF THE 4 POINTS BELOW INTO A NEW "OTHER ANALYSES" SECTION. PLEASE REVERT IF THIS DOESN'T MAKE SENSE.

1. `(HF/SCBI/Other)_quiltplot.R`: Create separte quiltplot data found in `growth_phenology/results`
1. `create_final_quiltplot`: Create the final quiltplot using quilplot generated in previous 3 scripts
1. `tree_core_drought_(no_)interactions.R`: Run basic linear models and extracts regression values found in `growth_phenology/Data`
1. `Chrono_table_changes.R`: Create `doc/manuscript/tables_figures/chronology_table.csv` by combining results from quiltplots and `tree_core_drought_(no_)interactions.R`


## Code for figures

1. **Figure 1: Summary of temperate deciduous tree growth responses to warmer spring temperatures**. Run `additional_figures.R` lines 1-368  
1. **Figure 2: Responses of foliage phenology (a,b) and stem growth timing (c,d) to spring temperatures at the Smithsonian Conservation Biology Institute (a,c) and Harvard Forest (b,d)**. Run `DOY_timing_all_years.R`
1. **Figure 3: Sensitivity of annual growth, as derived from tree-rings, to monthly mean maximum temperatures (*T~max~*), for `r n_chronologies` chronologies from `r n_cores_sites` sites across eastern North America**. HEY CAM COULD YOU FILL IN INSTRUCTIONS ON HOW TO CREATE THESE
