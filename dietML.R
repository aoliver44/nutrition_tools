#!/usr/bin/env Rscript 

## SCRIPT: generic_combine.R ===================================================
## AUTHOR: Andrew Oliver
## DATE:   Aug 8, 2022
##
## PURPOSE: To combine files cleaned from running
## the generic_read_in.R script

## docker info =================================================================

## docker command:
#docker run --rm -it -p 8787:8787 
#-e PASSWORD=yourpasswordhere 
#-v /Users/andrew.oliver/Documents/active_projects_github-USDA/nutrition_tools/:/home 
#amr_r_env:3.1.0

## general command:
## /home/scripts/generic_combine.R --cor_level 0.8 --cor_choose TRUE /home/output/ combined_ml_data.csv

## set working dir to /home for the docker container
setwd("/home")


## add commandline options =====================================================

library(docopt)
"Run random forest regression or classification on a dataframe
Usage:
    dietML.R [--label=<label> --cor_level=<cor_level> --train_split=<train_split>] <input> <output>
    
Options:
    -h --help  Show this screen.
    -v --version  Show version.
    --label=<label> name of column that you are prediction [default: diet_feature]
    --cor_level level to group features together [default: 0.85]
    --train_split what percentage of samples should be used in training [default: 0.70]
    
Arguments:
    input  path to input file for ML
    output path where results should be written
" -> doc

opt <- docopt::docopt(doc, version = 'dietML.R v1.0\n\n')

## load libraries ==============================================================


## set random seed if needed
set.seed(1)

## set working dir to /home for the docker container
if (dir.exists(opt$output) == TRUE) {
  setwd(opt$output)
} else {
  dir.create(path = opt$output)
  setwd(opt$output)
}


## helper functions ============================================================

## Negate function ("not in"):
`%!in%` <- Negate(`%in%`)

## check for inputs ============================================================