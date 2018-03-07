The files contained in this repository will conduct the analysis for the paper on racial identification among young adults in the [NLSY97](https://www.bls.gov/nls/nlsy97.htm) that is currently under review at *Emerging Adulthood*. All analysis is done using [R](https://www.r-project.org/). Below, I briefly describe each file in the order that they should be run. The shell script `run_entire_project.sh` can also be executed to run the entire analysis from scratch. 

- `check_packages.R`: this R script will check for packages that are used in the analysis, and if they are missing, install them.
- `process_raw_data.R`: This R script will read in the raw data from the NLSY97 in the `input` directory, clean it, re-organize it, and write a dataset to the `output` directory. A log of this script's last run is also kept in the `logs` folder. 
- `mimputation.R`: This R script will read the dataset produced by `process_raw_data.R` and perform multiple imputation to produce five different analytical datasets. These will be the datasets used in the analysis.
- `analysis.Rmd`: This R markdown file will perform the actual analysis including all figures, tables, and models. It includes exploratory figures and models that are not included in the final paper. 

The results for models may not exactly match those in the final paper because of multiple imputation. 
