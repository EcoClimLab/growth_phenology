# Growth Phenology

Cameron Dow's growth phenology project

## Steps to replicate the analysis


1. Using McMahon & Parker [LG5 dendroband model](https://github.com/seanmcm/RDendrom) for all tree-years compute: (1) fitted (DOY25, DOY50, DOY75) values and (2) all fitted LG5 parameters and related values (max rate, max rate DOY). Then do a little cleaning to remove egrigious outliers.
    1. SCBI: 
        1. Run `SCBI_wood_phenology.R` to generate `Data/Wood_pheno_table_VXRAW.csv` 
        1. Run `data_cleaning.R` to generate `Data/Wood_pheno_table_VXCLEAN.csv` where `X` is the latest version number, which is 13
    1. Harvard Forest:
        1. Run `HF_wood_phenology.R` to generate `Data/Wood_pheno_table_HarvardForest_VXRAW.csv`
        1. Run `data_cleaning.R` to generate `Data/Wood_pheno_table_HarvardForest_VXCLEAN.csv` where `X` is the latest version number, which is 9
2. Generate all `climwin` critical temperature windows
    1. Run `SCBI_Climwin_Cameron.R` to generate `results/Climwin_results/Weekly/SCBI/weekly_climwin_results_975perc.csv`
    1. Run `HF_Climwin_Cameron.R` to generate `results/Climwin_results/Weekly/Harvard Forest/weekly_climwin_results_all_HF_975.csv`
3. Run `Pheno_tsensitivity_figure.R` to (1) fit all Bayesian models using [`rstanarm`](https://mc-stan.org/users/interfaces/rstanarm) R package interface to Stan and (2) generate plots based on posterior distributions
4. Run `additional_figures.R` to generate additional plots
    1. Manually change years in section header `DOY timing figure` based upon hottest and coldest temp years within climwin windows. See climwinmeans generated in             `Pheno_Tsensitivity.R`
