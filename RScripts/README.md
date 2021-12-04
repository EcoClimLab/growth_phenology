# R Scripts - Run in the following order:

* `(HF/SCBI)_wood_phenology.R`: Script that fits logistic growth model to raw dendroband measurements to create  `Data/Wood_pheno_table_(HF/SCBI)_RAW.csv`and `LG5_parameter_values_(HarvardForest/SCBI)_RAW`

*`data_cleaning.R`: Removes poorly fit models from `Data/Wood_pheno_table_(HF/SCBI)_RAW.csv`and `LG5_parameter_values_(HarvardForest/SCBI)_RAW to create `Data/Wood_pheno_table_(HF/SCBI)_CLEAN.csv`and `LG5_parameter_values_(HarvardForest/SCBI)_CLEAN`

*`(HF/SCBI)_climwin_Cameron.R`: Run the climwin analysis to determine critical temperature windows for tmax and tmin

*`Pheno_tsensitivity_figure_tmax.R` and `Pheno_tsensitivity_figure_april_april_tmax.R`: Run bayesian analysis of cleaned dendroband models to create outputs found in `results/bayesian_outputs`and `doc/manuscript/tables_figures/pheno_Tsensitivity_combo_patchwork_(AIC/april).png`

*`(HF/SCBI/Other)_quiltplot.R`: Used to create separte quiltplot data found in `growth_phenology/results`

*`create_final_quiltplot`: Creates the final quiltplot using quilplot generated in previous 3 scripts

*`tree_core_drought_(no_)interactions.R`: Runs basic linear models and extracts regression values found in `growth_phenology/Data`

*`Chrono_table_changes.R`: Creates `doc/manuscript/tables_figures/chronology_table.csv` by combining results from quiltplots and `tree_core_drought_(no_)interactions.R`

*Remaining scripts create various figures, explore aspects of data created in previous scripts, or are functions used for other scripts.

* The `bert/` folder contains bert's versions of this code
