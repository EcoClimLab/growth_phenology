# Growth Phenology

Cameron Dow's growth phenology project

## Steps to replicate the analysis

1. Compute all fitted parameters of McMahon & Parker LG5 dendroband model and all derivative values (max rate, max rate DOY) using the functions in `RScripts/dendroband_functions.R`
    1. SCBI: 
        1. Run `SCBI_wood_phenology.R` to generate `Data/Wood_pheno_table_VXRAW.csv` 
        1. Run `RScripts/data_cleaning.R` to generate `Data/Wood_pheno_table_VXCLEAN.csv` where `X` is the latest version number, which is 13
    1. Harvard Forest:
        1. Run `HF_wood_phenology.R` to generate `Data/Wood_pheno_table_HarvardForest_VXRAW.csv`
        1. Run `RScripts/data_cleaning.R` to generate `Data/Wood_pheno_table_HarvardForest_VXCLEAN.csv` where `X` is the latest version number, which is 9
2. Generate all `climwin` critical temperature windows
    1. Run `RScripts/Climwin_Cameron.R` to generate `results/Climwin_results/Weekly/SCBI/weekly_climwin_results_975perc.csv`
    1. Run `RScripts/climwin_HF.R` to generate `results/Climwin_results/Weekly/Harvard Forest/weekly_climwin_results_all_HF_975.csv`
3. Run `RScripts/Pheno_tsensitivity_figure.R` to
    1. Fit all Bayesian models
    1. Generate plots based on posterior distributions
4. Run `additional_figures.R` to generate additional plots
    1. Manually change years in section header `DOY timing figure` based upon hottest and coldest temp years within climwin windows. See climwinmeans generated in             `Rscripts/Pheno_Tsensitivity.R`
