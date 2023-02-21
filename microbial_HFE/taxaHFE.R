#!/usr/bin/env Rscript 

## SCRIPT: taxa_HFE.R ===============================================
## AUTHOR: Andrew Oliver
## DATE:   Feb 16, 2022
##
## PURPOSE: To compress feature space of hierarchical organized data

## docker info =================================================================

## docker command:
#docker run --rm -it -p 8787:8787 -e PASSWORD=yourpasswordhere -v /Users/andrew.oliver/Documents/active_projects_github-USDA/nutrition_tools/:/home amr_r_env:3.1.0

## set working dir to /home for the docker container
setwd("/home")

## add commandline options =====================================================

library(docopt)
'Hierarchical feature engineering (HFE) for the reduction of features with respects to a factor or regressor
Usage:
    taxaHFE.R [--subject_identifier=<subject_colname> --label=<label> --feature_type=<feature_type> --format_metaphlan=<format> --cor_level=<correlation_level> --ncores=<ncores>] <input_metadata> <input> <output>
    
Options:
    -h --help  Show this screen.
    -v --version  Show version.
    --subject_identifier name of columns with subject IDs [default: subject_id]
    --label response feature of interest for classification [default: cluster]
    --feature_type of response i.e. numeric or factor [default: factor]
    --format_metaphlan tells program to expect the desired hData style format, otherwise it attempts to coerce into format [default: FALSE]
    --cor_level level of initial correlation filter [default: 0.95]
    --ncores number of cpu cores to use [default: 2]
Arguments:
    input_meta path to metadata input (txt | tsv | csv)
    input path to input file from hierarchical data (i.e. hData data) (txt | tsv | csv)
    output output file name (txt)

' -> doc

opt <- docopt::docopt(doc, version = 'microbial_HFE.R v1.2\n\n')
#print(opt)
## load libraries ==============================================================

library(dplyr, quietly = T, verbose = F, warn.conflicts = F)
library(janitor, quietly = T, verbose = F, warn.conflicts = F)
library(tidyr, quietly = T, verbose = F, warn.conflicts = F)
library(tibble, quietly = T, verbose = F, warn.conflicts = F)
library(caret, quietly = T, verbose = F, warn.conflicts = F)
library(readr, quietly = T, verbose = F, warn.conflicts = F)
library(reshape2, quietly = T, verbose = F, warn.conflicts = F)
library(ggplot2, quietly = T, verbose = F, warn.conflicts = F)
library(ggsci, quietly = T, verbose = F, warn.conflicts = F)
library(vegan, quietly = T, verbose = F, warn.conflicts = F)
library(progress, quietly = T, verbose = F, warn.conflicts = F)
library(stringr, quietly = T, verbose = F, warn.conflicts = F)

## set random seed if needed
set.seed(42)
nperm = 10
## helper functions ============================================================

## Negate function ("not in"):
`%!in%` <- Negate(`%in%`)

## suppress warnings
options(warn=-1)

source("/home/scripts/microbial_HFE/taxaHFE_functions.R")

## arg tests ===================================================================
# opt <- data.frame(subject_identifier=character(),
#                   label=character(),
#                   feature_type=character(),
#                   ncores=numeric(),
#                   format_metaphlan=character(),
#                   cor_level=numeric(),
#                   input_metadata=character(),
#                   input=character(),
#                   output=character())
# opt <- opt %>% tibble::add_row(
#   subject_identifier = "Sample",
#   label= "Study.Group",
#   feature_type = "factor",
#   format_metaphlan = "FALSE",
#   cor_level = 0.70,
#   ncores = 4,
#   input_metadata = "/home/curated_data/data/clean_data/WANG_metadata.tsv",
#   input= "/home/curated_data/data/clean_data/WANG_species.tsv",
#   output = "/home/curated_data/data/clean_data/WANG_correlation_tests/WANG_cor_70.txt"
#   )

## check for inputs ============================================================
cat("\n\n", "###########################\n", "Reading in data...\n", "###########################")

## check and see if clean_files directory exists
cat("\n","Checking for for input_metadata...")
if (file.exists(opt$input_metadata)) {
  cat("\n",paste0("Using ", opt$input_metadata, " as input")) 
} else { stop("Metadata input not found.") }

## check for input file (hierarchical data)
cat("\n","Checking for for input")
if (file.exists(opt$input)) {
  cat("\n",paste0("Using ", opt$input, " as input")) 
} else { stop("Input not found.") }

## read in microbiome ==========================================================
## read in data, should be in tab or comma separated format

hData <- read_in_microbiome(input = opt$input)
original_taxa_count <- NROW(hData)

## read in metadata file =======================================================
## rename the subject_identifier to subject_id and
## rename the label to feature_of_interest
## metadata, should be in tab or comma separated format

metadata <- read_in_metadata(input = opt$input_metadata, subject_identifier = opt$subject_identifier, label = opt$label)

## if not "metaphlan" format, attempt to convert ===============================

if (opt$format_metaphlan == "FALSE") {
  
  convert_to_hData(input = hData)
  hData <- do.call(rbind, lapply(ls(pattern = "hData_L"), get))
  
  
}

## Remove very low prevalent features ==========================================

apply_filters(input = hData)

## write files for old HFE program =============================================

write_old_hfe(input = hData, output = opt$output)


## clean clade name of symbols and spaces so ranger doesnt freak out.
hData$clade_name <- gsub(" ", "_", hData$clade_name)
hData$clade_name <- gsub("\\-", "_", hData$clade_name)

## make the dataframe of features that will compete in parent-child competitions.
## Basically this is just the taxonomic (hierarchical) information of all
## the features left at this step.

make_taxa_split_df(input = hData)

## write summarized files and clean up =========================================
## these are the species only, genus only...etc files
## to check against.

write_summary_files(input = hData, output = opt$output)

cat("\n\n", "##################################\n", "Starting hierarchical competitions\n", "##################################\n")

## compete! ====================================================================

taxaHFE_competition(input = hData, feature_type = opt$feature_type, cores = opt$ncores, output = opt$output)

## super filter ================================================================

super_filter(input = hData, feature_type = opt$feature_type, cores = opt$ncores, output = opt$output)


## Write Figure ================================================================

write_figure(input = hData, output = opt$output)


## Write outputs ===============================================================

cat("\n",paste0("Reduced/compressed taxa set from ", original_taxa_count, " taxa to ", (NROW(taxa_only_split_sf)), " (", NROW(taxa_only_split), " if no super filter)"))

cat("\n","Output written.  ")
