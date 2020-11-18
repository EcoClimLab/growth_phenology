# Growth Phenology

Cameron Dow's growth phenology project

## Steps to replicate the analysis

1. Compute all fitted parameters of McMahon & Parker LG5 dendroband model and all derivative values (max rate, max rate DOY)
    1. SCBI: 
        1. ?
        1. Run `RScripts/data_cleaning.R` to generate `Data/Wood_pheno_table_VXCLEAN.csv` where `X` is the latest version number, which is a ???
    1. Harvard Forest:
        1. ?
        1. Run `RScripts/data_cleaning.R` to generate `Data/Wood_pheno_table_HarvardForest_VXCLEAN.csv` where `X` is the latest version number, which is a ???
1. Generate all `climwin` critical temperature windows
    1. Run `RScripts/Climwin_Cameron.R` to generate `results/Climwin_results/Weekly/SCBI/weekly_climwin_results_975perc.csv`
    1. Run `???` to generate `results/Climwin_results/Weekly/Harvard Forest/weekly_climwin_results_all_HF_975.csv`
1. Run `RScripts/Pheno_tsensitivity_figure.R` to
    1. Fit all Bayesian models
    1. Generate plots based on posterior distributions
