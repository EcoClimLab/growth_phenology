# R Scripts

All R code used for analysis is here.

## Main analysis

Run in the following order:

1. Using McMahon & Parker [LG5 logistic dendroband growth model](https://github.com/seanmcm/RDendrom) compute: (1) fitted (DOY25, DOY50, DOY75) values and (2) all fitted LG5 parameters and related values (max rate, max rate DOY).
    1. Run both `(HF/SCBI)_wood_phenology.R` to create `Data/Wood_pheno_table_(HF/SCBI)_RAW.csv`and `LG5_parameter_values_(HarvardForest/SCBI)_RAW`
    1. Note LG5 functions are found in `dendroband_functions.R`
2. Remove poorly fit models from `Data/Wood_pheno_table_(HF/SCBI)_RAW.csv`and `LG5_parameter_values_(HarvardForest/SCBI)_RAW`
    1. Run `data_cleaning.R` to create `Data/Wood_pheno_table_(HF/SCBI)_CLEAN.csv` and `LG5_parameter_values_(HarvardForest/SCBI)_CLEAN`
3. Determine all [`climwin`](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0167980) critical temperature windows for tmax and tmin
    1. Run both `(HF/SCBI)_climwin_Cameron.R`: Run the climwin analysis to determine critical temperature windows for tmax and tmin
4. Run Bayesian analysis of cleaned dendroband models using [`rstanarm`](https://mc-stan.org/users/interfaces/rstanarm)
    1. Run both `Pheno_tsensitivity_figure_tmax.R` and `Pheno_tsensitivity_figure_april_april_tmax.R` to create outputs found in `results/bayesian_outputs` and `doc/manuscript/tables_figures/pheno_Tsensitivity_combo_patchwork_(AIC/april).png`


## Code for figures

1. Figure 1 - Summary of temperate deciduous tree growth responses to warmer spring temperatures
    1. Run `additional_figures.R` lines 1-368  
2. Figure 2 - Responses of foliage phenology (a,b) and stem growth timing (c,d) to spring temperatures at the Smithsonian Conservation Biology Institute (a,c) and Harvard Forest (b,d)
    1. Run `DOY_timing_all_years.R`
3. Figure 3 - Sensitivity of annual growth, as derived from tree-rings, to monthly mean maximum temperatures (*Tmax*), for `r n_chronologies` chronologies from `r n_cores_sites` sites across eastern North America
    1. Run `(HF/SCBI/Other)_quiltplot.R`
    2. Run `create_final_quiltplot.R`	
4. Extended Data Figure 1 - Seasonal patterns of forest canopy greenness (top row) and stem growth of ring- and diffuse-porous trees
    1. Run `additional figures.R` lines 379-762
5. Extended Data Figure 2 - Landscapes of relationships between the day of year on which 25% of annual growth is achieved (DOY25) and temperature in prior weeks for ring- and diffuse-porous trees at the Smithsonian Conservation Biology Institute (SCBI) and Harvard Forest.
    1. Run `(HF/SCBI)_climwin.R` making sure to edit the plotbetas function in climwin: in the function, change scale_fill_gradientn(colours = c("red",        "yellow", "blue"), name = "") to scale_fill_gradient2(high = "blue", mid = "yellow", low = "red") and add text = element_text(size = 20) to theme        call
    2. Run `additional_figrues.R` lines 874-921
6. Extended Data FIgure 3/4 - Response of stem growth timing and rates to mean maximum temperatures (Tmax) during the spring critial temperature window (CTW) for ring- and diffuseporous species at the Smithsonian Conservation Biology Institute (SCBI) and Harvard Forest
    1. Run both `Pheno_tsensitivity_figure_tmax.R` and `Pheno_tsensitivity_figure_april_april_tmax.R` to create outputs found in `results/bayesian_outputs`        and `doc/manuscript/tables_figures/pheno_Tsensitivity_combo_patchwork_(AIC/april).png`
7. Extended Data Figure 5 - Map of sampling locations of tree-ring chronologies analyzed in this study.
    1. Run `chronology_map.R`
8. Extended Data Figure 6 - Sensitivity of annual growth, as derived from tree-rings, to monthly mean minimum temperatures (Tmin)
    1. Run `(HF/SCBI/Other)_quiltplot.R`
    2. Run `create_final_quiltplot.R`
9. Extended Data Figure 7 - Sensitivity of annual growth, as derived from tree-rings, to monthly mean maximum temperatures (Tmax) of the current and past year
    1. Run `(HF/SCBI/Other)_quiltplot_extended.R`
    2. Run `create_final_quiltplot_extended.R`

## Other analyses

1. `tree_core_drought_(no_)interactions.R`: Run basic linear models and extracts regression values found in `growth_phenology/Data`
2. `Chrono_table_changes.R`: Create `doc/manuscript/tables_figures/chronology_table.csv` by combining results from quiltplots and `tree_core_drought_(no_)interactions.R`



