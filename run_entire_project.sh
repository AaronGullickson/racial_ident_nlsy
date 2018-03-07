#!/bin/bash

# This shell script will re-run the entire analysis from the raw data. 

# Remove all logs, output, and html reports. 
rm output/*
rm logs/*
rm *.html

# check for necessary R packages
Rscript check_packages.R

# Process and clean the raw data, send output to log directory as HTML
Rscript -e "rmarkdown::render('process_raw_data.R',output_format='pdf_document',output_file='process_raw_data.pdf',output_dir='logs')"

# do multiple imputation and save analytical datasets
Rscript mimputation.R

# Run the main analysis
Rscript -e "rmarkdown::render('analysis.Rmd')"
