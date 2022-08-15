#################################################
## SCRIPT: generic_combine.R
## AUTHOR: Andrew Oliver
## DATE:   Aug 8, 2022
##
## PURPOSE: To combine files cleaned from running
## the generic_read_in.R script
#################################################

## docker command:
#docker run --rm -it -p 8787:8787 
#-e PASSWORD=yourpasswordhere 
#-v /Users/andrew.oliver/Documents/active_projects_github-USDA/nutrition_tools/:/home 
#amr_r_env:3.1.0

## add commandline options
# library(optparse)
# option_list = list(
#   make_option(c("-i", "--input"), type="character", default="/home/data/", 
#               help="path to folder with data to import [default= %default]", metavar="character"),
#   make_option(c("-s", "--subject_identifier"), type="character", default="subject_id", 
#               help="subject key (column name) found in all files [default= %default]", metavar="character")
# ); 
# 
# opt_parser = OptionParser(option_list=option_list);
# opt = parse_args(opt_parser);
# 
# # test if there is at least one argument: if not, return an error
# if (length(opt)==0) {
#   stop("At least one argument must be supplied (input file).n", call.=FALSE)
# } else if (length(opt)==1) {
#   # default output file
#   opt[2] = "subject_id"
# }

##########################################################################

## set working dir to /home for the docker container
setwd("/home")

## load libraries as needed!
library(tidyverse)
library(reshape2)


## check and see if clean_files directory exists
print





