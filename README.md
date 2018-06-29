# The Racial Identification of Young Adults in a Racially Complex Society

This repository contains the data and code for the analysis in the paper entitled "The Racial Identification of Young Adults in a Racially Complex Society" which is forthcoming in a special issue of *Emerging Adulthood*. The main analytical results can be found in the analysis.html file which can be opened with any web browser.

The data here come from the [National Longitudinal Study of Youth 1997](https://www.bls.gov/nls/nlsy97.htm) (NLSY97). The raw data from extracts through the [NLS Investigator](https://www.nlsinfo.org/investigator/pages/login.jsp) are included in the input directory including *.NLSY files that can be used to load the extract information into NLS Investigator. The output directory contains the analytical data used in the analysis after recoding/cleaning of the raw data.

All analysis was done in R. For those on a Mac or Unix machine, the entire analysis can be run from statch with the run_entire_project.sh bash script. Otherwise, here is the order in which scripts should be run to reproduce the analysis.

- `check_packages.R`: This script will check for necessary packages and install them if they are not currently installed.
- `process_raw_data.R`: This script reads in the raw data, performs data manipulation and recoding, and outputs an analytical dataset. A log file of this script is kept in the logs directory.
- `mimputation.R`: This script uses multiple imputation through chained equations to construct five analytical datasets and does some final variable coding for each dataset. 
- `analysis.Rmd`: The actual analysis. This script is an R Markdown file and will output all results to analysis.html.
