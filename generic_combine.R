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

## add commandline options =====================================================

library(optparse)
option_list = list(
  make_option(c("-i", "--input"), type="character", default="/home/output/",
              help="path to clean_data directory for import [default= %default]", metavar="character"),
  make_option(c("-s", "--subject_identifier"), type="character", default="subject_id",
              help="subject key (column name) found in all files [default= %default]", metavar="numeric"),
  make_option(c("-c", "--cor_level"), type="numeric", default=0.95,
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
setwd(opt$input)
getwd()
## load libraries ==============================================================

library(tidyverse)
library(reshape2)
library(corrr)
library(mikropml)
library(Hmisc)
library(heatmaply)

## 
## set random seed if needed
set.seed(1)

## helper functions ============================================================

## Negate function ("not in"):
`%!in%` <- Negate(`%in%`)

## check and see if clean_files directory exists
print("Checking for clean_files directory and summary_dataset_problems.csv")

fils <- list.files(paste0("clean_files"), full.names = TRUE, recursive = TRUE)
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
}

for (fil in summary_problems$dataset) {
    ## copy into a tmp directory
    file.copy(from = paste0("clean_files/", fil, ".csv"), to = "problem_check/", recursive = F) 
}

## run generic_read_in.R on this problem subset and see if any problems occur
system(paste0("Rscript /home/scripts/generic_read_in.R -i ", opt$input, "problem_check/ ", "-s ", opt$subject_identifier, " -o ", opt$input, "problem_check/output/"))
  
## check and see if summary_dataset_problems got written  
if (file.exists("problem_check/output/summary_dataset_problems.csv")) {
    summary_problems_recheck <- read.csv(paste0("problem_check/output/summary_dataset_problems.csv"))
    problem_check_dataset <- summary_problems_recheck[1,]$dataset
    problem_check_problem <- apply(summary_problems_recheck[1,], 1, function(x) last(colnames(summary_problems_recheck[1,])[x==1]))
}

if (NROW(summary_problems_recheck >= 1)) {
      print(paste0("This program still thinks problems exist with the data. For example, did you fix ", problem_check_problem, " in ", problem_check_dataset, " dataset??"))
      print("Since you didnt fix these problems (we worked so hard to tell you about them!!) we will remove these datasets from downstream analyses")
      
      ## get interactive acknowledgment 
      cat("Press [enter] to continue ")
      x <- readLines(file("stdin"),1)
      print(x)
      
      ## remove summary_problem_datasets from fils list
      fils <- as.data.frame(fils) %>% 
        dplyr::filter(., !grepl(pattern = paste(summary_problems$dataset, collapse = "|"), x = fils)) %>%
        pull(., var = "fils")
}


## read in the files that passed muster
if (length(fils) > 0) {
    myfiles = lapply(fils, read_csv)
}

## merge them all together
full_merge <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = opt$subject_identifier, all.x = TRUE), myfiles)

## check sample size here ======================================================

if (NROW(full_merge) < 200) {
  print("################################################")
  print("WARNING:")
  print("You have VERY few samples. Overfitting is a major concern. Your results might only be applicable to your sample set. This problem is 
        made much worse if you have a multiclass problem (> 2 classes for classification).")
  print("################################################")
  
  ## get interactive acknowledgment 
  cat("Press [enter] to continue ")
  x <- readLines(file("stdin"),1)
  print(x)
  
} else if (NROW(full_merge) > 200 && NROW(full_merge) < 400) {
  print("Note:")
  print("This is a reasonably small ML dataset. But depending on what you are doing, it might work! We will assume you're an expert.")
  
  ## get interactive acknowledgment 
  cat("Press [enter] to continue ")
  x <- readLines(file("stdin"),1)
  print(x)
  
}

## de-duplicate columns, picking one with least NAs
full_merge_dedup <- full_merge %>% 
  tibble::column_to_rownames(., var = opt$subject_identifier) %>%
  t() %>%
  as.data.frame() %>%
  tibble::rownames_to_column(., var = "feature") %>%
  dplyr::mutate(., feature = gsub("\\.x", "", gsub("\\.y", "", gsub("\\.1", "", feature)))) %>% 
  dplyr::arrange(rowSums(is.na(.))) %>%
  dplyr::distinct(feature, .keep_all = TRUE) %>% 
  tibble::column_to_rownames(., var = "feature") %>%
  t() %>%
  as.data.frame() %>%
  readr::type_convert(.)

## for now lets remove columns that are mostly NAs
full_merge_dedup_tmp_col_drop <- full_merge_dedup[ , colSums(is.na(full_merge_dedup)) < (NCOL(full_merge_dedup)*0.01)]
## ok lets remove rows that are mostly NA (addmittedly a much smaller dataset)...but no NAs left!
full_merge_dedup_tmp_row_drop <- full_merge_dedup_tmp_col_drop %>% tidyr::drop_na()

## add in a dummy var for correlation purpose
full_merge_dedup_pre_cor <- full_merge_dedup_tmp_row_drop %>% 
  mutate(., dummy_var = sample(c(0,1), size = NROW(.), replace = T)) %>%
  tibble::rownames_to_column(., var = "subject_id") 

## co-correlate features =======================================================

## check correlation level

if (opt$cor_level < 0.95) {
  print("################################################")
  print("WARNING:")
  cat("Your correlation level is below 0.95. This is a global correlation check, 
  mainly for PURELY redundant features. For feature engineering, PROPER correlation-based 
  feature selection should happen inside a Train-Test split or a cross-validation 
  procedure. Not doing so constitutes DATA LEAKAGE. You risk overfitting and 
  reporting results that look better than they probably are.")
  
  ## get interactive acknowledgment 
  cat("Press [enter] to continue ")
  x <- readLines(file("stdin"),1)
  print(x)
  
}

## correlate to show which features are super redundant
corr_raw_data <- mikropml::preprocess_data(dataset = full_merge_dedup_pre_cor,
                                         method = NULL,
                                         outcome_colname = "dummy_var", 
                                         collapse_corr_feats = F, 
                                         remove_var = "zv")


high_cor <- mikropml:::group_correlated_features(corr_raw_data$dat_transformed, 
                                                corr_thresh = opt$cor_level, group_neg_corr = T)

high_cor <- as.data.frame(high_cor) %>% 
  separate(., col = high_cor, into = c("keep", "co-correlated"), sep = "\\|", extra = "merge") %>%
  dplyr::filter(., keep != "dummy_var")

high_cor_list <- c(high_cor$keep, high_cor$`co-correlated`)
high_cor_list <- gsub(pattern = "\\|", replacement = " ", x = high_cor_list)
high_cor_list <- unlist(strsplit( high_cor_list, " " ))

post_correlate <- corr_raw_data$dat_transformed %>% 
  dplyr::select(., any_of(high_cor_list), -dummy_var) %>%
  correlate(method = "pearson") %>% 
  tibble::column_to_rownames(., var = "term") %>% abs()

v1 <- sample(colnames(post_correlate))
post_correlate[v1] <- lapply(post_correlate[v1], function(x) replace(x,  (x < (opt$cor_level - 0.000001)), 0))

## for now lets remove columns that are mostly NAs
post_correlate <- post_correlate[ , colSums(post_correlate, na.rm = T) > 0]
post_correlate <- post_correlate[ rowSums(post_correlate, na.rm = T) > 0, ]


## write correlation figure ====================================================
## We use Hmisc to calculate a matrix of p-values from correlation tests
post_correlate.rcorr <- Hmisc::rcorr(as.matrix(post_correlate))
p <- post_correlate.rcorr$P

heatmaply_cor(
  as.matrix(post_correlate),
  #node_type = "scatter",
  #point_size_mat = -log10(p), 
  #point_size_name = "-log10(p-value)",
  label_names = c("x", "y", "Correlation"),
  file = "heatmaply.html",
  #colors = viridis(n = 256, alpha = 1, begin = 0, end = 1, option = "plasma"),
  main = paste0("Absolute Pearson Correlations that are above r= ", opt$cor_level, "\nAll other correlations set to 0 for vizualization purposes"),
)



