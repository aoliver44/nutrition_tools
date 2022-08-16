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
library(optparse)
option_list = list(
  make_option(c("-i", "--input"), type="character", default="outputs_22_08_15_171447/",
              help="path to clean_data directory for import [default= %default]", metavar="character"),
  make_option(c("-s", "--subject_identifier"), type="character", default="subject_id",
              help="subject key (column name) found in all files [default= %default]", metavar="character"),
  make_option(c("-o", "--output_file"), type="character", default="combined_raw_file.csv",
              help="output file to write [default= %default]", metavar="character")
);

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

## test if there is at least one argument: if not, return an error
if (length(opt)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
} else if (length(opt)==1) {
  # default output file
  opt[2] = "subject_id"
}

##########################################################################

## set working dir to /home for the docker container
setwd(paste0("/home/", opt$input))

## load libraries as needed!
library(tidyverse)
library(reshape2)

## set random seed if needed
set.seed(1)

## make some extra helper functions
## Negate function ("not in"):
`%!in%` <- Negate(`%in%`)



## check and see if clean_files directory exists
print("Checking for clean_files directory and summary_dataset_problems.csv")

fils <- list.files(paste0("clean_files/"), full.names = TRUE, recursive = TRUE)
## check and make sure there are files clean_files dir
if (length(fils) >= 1) {
  print(paste0("clean_files directory exists and is not empty: ", opt$input, "clean_files/"))
}
## check and make sure summary_dataset_problems exists
if (file.exists("summary_dataset_problems.csv")) {
  print(paste0("summary_dataset_problems file found: ", opt$input, "summary_dataset_problems.csv"))
  print("Using file to check if clean_files have been fixed...")
  
  ## use summary_problems to check and see if problems still exist in data
  summary_problems <- read.csv("summary_dataset_problems.csv")
  
  print(paste0("Checking all files that were previously identified as problems"))
  
  ## make a tmp directory and copy problem files into
  dir.create(file.path(paste0("problem_check/")))
  
  for (fils in summary_problems$dataset) {
    ## copy into a tmp directory
    file.copy(from = paste0("clean_files/", fils, ".csv"), to = "problem_check/", recursive = F) }
    
    ## run generic_read_in.R on this problem subset and see if any problems occur
  system(paste0("Rscript /home/scripts/generic_read_in.R -i ", opt$input, "problem_check/ ", "-s ", opt$subject_identifier, " -o ", opt$input, "problem_check/output/"))
  
  ## check and see if summary_dataset_problems got written  
  if (file.exists("problem_check/output/summary_dataset_problems.csv")) {
    summary_problems_recheck <- read.csv(paste0("problem_check/output/summary_dataset_problems.csv"))
    problem_check_dataset <- summary_problems_recheck[1,]$dataset
    problem_check_problem <- apply(summary_problems_recheck[1,], 1, function(x) last(colnames(summary_problems_recheck[1,])[x==1]))
    if (NROW(summary_problems_recheck >= 1)) {
      print(paste0("This program still thinks problems exist with the data. For example, did you fix ", problem_check_problem, " in ", problem_check_dataset, " dataset??"))
      print("Since you didnt fix these problems (we worked so hard to tell you about them!!) we will remove these datasets from downstream analyses")
      
    }
  }
}

