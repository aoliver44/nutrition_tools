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
"Combine data from read_in step, prior to ML
Usage:
    generic_combine.R [--subject_identifier=<subject_colname> --cor_level=<cor_level> --cor_choose=<cor_choose>] <input> <output_file>
    
Options:
    -h --help  Show this screen.
    -v --version  Show version.
    --subject_identifier=<subject_colname> name of columns with subject IDs [default: subject_id]
    --cor_level level of general feature correlation [default: 0.95]
    --cor_choose choose which features are kept in correlation [default: FALSE]
    
Arguments:
    input  input directory containing files
    output_file  output file name

" -> doc

opt <- docopt::docopt(doc, version = 'generic_combine.R v1.0\n\n')

## load libraries ==============================================================

library(dplyr)
library(tibble)
library(tidyr)
library(readr)
library(janitor)
library(mikropml)
library(Hmisc)
library(heatmaply)
library(corrr)

## set random seed if needed
set.seed(1)

## set working dir to /home for the docker container
setwd(opt$input)
getwd()

## helper functions ============================================================

## Negate function ("not in"):
`%!in%` <- Negate(`%in%`)

## check for inputs ============================================================

## check and see if clean_files directory exists
print("Checking for clean_files directory and summary_dataset_problems.csv")

fils <- list.files(paste0("clean_files"), full.names = TRUE, recursive = TRUE)
## check and make sure there are files clean_files dir
if (length(fils) >= 1) {
  cat(paste0("clean_files directory exists and is not empty: ", opt$input, "clean_files/"))
}
## check and make sure summary_dataset_problems exists
if (file.exists("summary_dataset_problems.csv")) {
  cat(paste0("summary_dataset_problems file found: ", opt$input, "summary_dataset_problems.csv"))
  cat("Using file to check if clean_files have been fixed...")
  
  ## use summary_problems to check and see if problems still exist in data
  summary_problems <- read.csv("summary_dataset_problems.csv")
  
  cat(paste0("Checking all files that were previously identified as problems"))
  
  ## make a tmp directory and copy problem files into
  dir.create(file.path(paste0("problem_check/")))
}

## check if problem files are fixed ============================================

for (fil in summary_problems$dataset) {
    ## copy into a tmp directory
    file.copy(from = paste0("clean_files/", fil, ".csv"), to = "problem_check/", recursive = F) 
}

## run generic_read_in.R on this problem subset and see if any problems occur
system(paste0("/home/scripts/generic_read_in.R --subject_identifier ",opt$subject_identifier," ",opt$input, "problem_check ", opt$input, "problem_check/output/"))

## check and see if summary_dataset_problems got written  
if (file.exists("problem_check/output/summary_dataset_problems.csv")) {
    summary_problems_recheck <- read.csv(paste0(opt$input,"problem_check/output/summary_dataset_problems.csv"))
    problem_check_dataset <- summary_problems_recheck[1,]$dataset
    problem_check_problem <- apply(summary_problems_recheck[1,], 1, function(x) last(colnames(summary_problems_recheck[1,])[x==1]))
}

if (NROW(summary_problems_recheck >= 1)) {
      cat(paste0("This program still thinks problems exist with the data. For example, did you fix ", problem_check_problem, " in ", problem_check_dataset, " dataset??"))
      cat("Since you didnt fix these problems (we worked so hard to tell you about them!!) we will remove these datasets from downstream analyses")
      
      ## get interactive acknowledgment 
      cat("  Press [enter] to continue  ")
      x <- readLines(file("stdin"),1)
      
      ## remove summary_problem_datasets from fils list
      fils <- as.data.frame(fils) %>% 
        dplyr::filter(., !grepl(pattern = paste(summary_problems$dataset, collapse = "|"), x = fils)) %>%
        pull(., var = "fils")
}

## read in & merge clean files =================================================

## read in the files that passed muster
if (length(fils) > 0) {
    myfiles = lapply(fils, read_csv)
}

## merge them all together
full_merge <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = opt$subject_identifier, all.x = TRUE), myfiles)

## check sample size here ======================================================

if (NROW(full_merge) < 200) {
  cat("################################################")
  cat("WARNING:")
  cat("You have VERY few samples. Overfitting is a major concern. Your results might only be applicable to your sample set. This problem is made much worse if you have a multiclass problem (> 2 classes for classification).")
  cat("################################################")
  
  ## get interactive acknowledgment 
  cat("  Press [enter] to continue  ")
  x <- readLines(file("stdin"),1)
  
} else if (NROW(full_merge) > 200 && NROW(full_merge) < 400) {
  cat("Note:")
  cat("This is a reasonably small ML dataset. But depending on what you are doing, it might work! We will assume you're an expert.")
  
  ## get interactive acknowledgment 
  cat("  Press [enter] to continue  ")
  x <- readLines(file("stdin"),1)

}

## de-duplicate based on NA count ==============================================
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

## pre-corr col drop ===========================================================

## drop columns with more than 1% NAs
full_merge_dedup_tmp_col_drop <- full_merge_dedup[ , colSums(is.na(full_merge_dedup)) < (NROW(full_merge_dedup)*0.01)]

## Notify user about what columns were dropped
if (((NCOL(full_merge_dedup)) - (NCOL(full_merge_dedup[ , colSums(is.na(full_merge_dedup)) < (NCOL(full_merge_dedup)*0.01)]))) > 0) {
  cat("For correlation purposes, we drop some NA-replete features prior to 
      row drops. We will next row-drop in order to have a NA-free dataset. 
      This is only for a global correlation check. We will write both a NA and 
      NA-free file, its up to you if you want to use interpolation methods.")
}

## pre-corr row drop ===========================================================

## ok lets remove rows that are mostly NA (admittedly a much smaller dataset)...but no NAs left!
full_merge_dedup_tmp_row_drop <- full_merge_dedup_tmp_col_drop %>% tidyr::drop_na()

## co-correlate features =======================================================

## add in a dummy var for correlation purpose
## this dummy var is meaningless. We are using the next steps for 
## co-correlation of features and for one-hot-encoding.
full_merge_dedup_pre_cor <- full_merge_dedup_tmp_row_drop %>% 
  dplyr::mutate(., dummy_var = sample(c(0,1), size = NROW(.), replace = T)) %>%
  tibble::rownames_to_column(., var = "subject_id")

## check correlation level

if (as.numeric(opt$cor_level) < 0.95) {
  cat("################################################")
  cat("WARNING:")
  cat("Your correlation level is below 0.95. This is a global correlation check, 
  mainly for PURELY redundant features. For feature engineering, PROPER correlation-based 
  feature selection should happen inside a Train-Test split or a cross-validation 
  procedure. Not doing so constitutes DATA LEAKAGE. You risk overfitting and 
  reporting results that look better than they probably are.")
  
  ## get interactive acknowledgment 
  cat("  Press [enter] to continue  ")
  x <- readLines(file("stdin"),1)

}

## prepare the data for correlation (one-hot encode, remove zero-variance)
corr_raw_data <- mikropml::preprocess_data(dataset = full_merge_dedup_pre_cor,
                                         method = NULL,
                                         outcome_colname = "dummy_var", ## meaningless var
                                         collapse_corr_feats = F, 
                                         remove_var = "zv")

## co-correlate features at specified threshold
high_cor <- mikropml:::group_correlated_features(corr_raw_data$dat_transformed, 
                                                corr_thresh = as.numeric(opt$cor_level), group_neg_corr = T)

## make dataframe of what is correlated at specified threshold.
high_cor <- as.data.frame(high_cor) %>% 
  tidyr::separate(., col = high_cor, into = c("keep", "co-correlated"), sep = "\\|", extra = "merge") %>%
  dplyr::filter(., keep != "dummy_var")


## write correlation figure ====================================================

## make a list of uncorrelated features
un_corr_list <- c(high_cor$keep[is.na(high_cor$`co-correlated`)])

## make list of all cor-correlated features
co_corr_list <- c(high_cor$`co-correlated`[!is.na(high_cor$`co-correlated`)])
co_corr_list <- gsub(pattern = "\\|", replacement = " ", x = co_corr_list)
co_corr_list <- unlist(strsplit( co_corr_list, " " ))
co_corr_list_add <- c(high_cor$keep[!is.na(high_cor$`co-correlated`)])
co_corr_list <- c(co_corr_list_add, co_corr_list)

## co-correlate these vars
correlate_figure <- corr_raw_data$dat_transformed %>% 
  dplyr::select(., any_of(co_corr_list), -dummy_var) %>%
  corrr::correlate(method = "pearson") %>% 
  tibble::column_to_rownames(., var = "term") %>% abs()

## set anything below cor threshold to zero
v1 <- sample(colnames(correlate_figure))
correlate_figure[v1] <- lapply(correlate_figure[v1], function(x) replace(x,  (x < (as.numeric(opt$cor_level) - 0.000001)), 0))

## remove cols and rows that are NAs
correlate_figure <- correlate_figure[ , colSums(correlate_figure, na.rm = T) > 0]
correlate_figure <- correlate_figure[ rowSums(correlate_figure, na.rm = T) > 0, ]

## write interactive heatmap of correlation
cor_figure <- heatmaply::heatmaply_cor(
  as.matrix(correlate_figure),
  #node_type = "scatter",
  #point_size_mat = -log10(p), 
  #point_size_name = "-log10(p-value)",
  label_names = c("x", "y", "Correlation"),
  file = "correlation_heatmap.html",
  #colors = viridis(n = 256, alpha = 1, begin = 0, end = 1, option = "plasma"),
  main = paste0("Absolute Pearson Correlations that are above r= ", as.numeric(opt$cor_level), "\nAll other correlations set to 0 for vizualization purposes"),
)

## decide which co-correlated vars to keep =====================================

if (opt$cor_choose == TRUE) {
  corr_decision_list = c()
  
  corr_choices <- high_cor %>%
    tidyr::drop_na() %>%
    dplyr::pull(var = keep)
  for (keep_var in corr_choices) {
    ## get vars that are co-correlated
    corr_choose <- subset(high_cor, high_cor$keep == keep_var)
    
    ## get rid of the | and make list
    corr_choose_list <- c(corr_choose$`co-correlated`)
    corr_choose_list <- gsub(pattern = "\\|", replacement = " ", x = corr_choose_list)
    corr_choose_list <- unlist(strsplit( corr_choose_list, " " ))
    
    ## put into dataframe with the auto-kept var
    #corr_choose_df <- as.data.frame(c(corr_choose$keep, corr_choose_list))
    corr_options <- c(corr_choose$keep, corr_choose_list)
    corr_choose_df <- as.data.frame(sq = seq_along(corr_options), corr_options)
    
    ## interactive decision 
    print(corr_choose_df)
    cat("Which corr_option (select the number) do you want included in the model?")
    corr_decision <- readLines("stdin", n = 1)
    corr_decision_list <- append(x = corr_decision_list, values = corr_choose_df$corr_options[as.numeric(corr_decision)])
    
  }
  
  ## add in the non-co-correlated vars
  full_decision_list <- c(un_corr_list, corr_decision_list)
  
} else {
  
  full_decision_list <- high_cor$keep
  
}


## summarize features kept =====================================================


## write file of ALL features
kept_features_summary <- data.frame(features=character(),
                                    category=character(),
                               stringsAsFactors=FALSE) 
kept_features_summary <- kept_features_summary %>% 
  tibble::add_row(features = colnames(full_merge_dedup)) %>%
  dplyr::mutate(., category = ifelse(is.na(category), "all_features", NA))

kept_features_summary <- kept_features_summary %>% 
  tibble::add_row(features = setdiff(colnames(full_merge_dedup), colnames(full_merge_dedup_tmp_col_drop))) %>% 
  dplyr::mutate(., category = ifelse(is.na(category), "dropped_to_retain_samples", category))

kept_features_summary <- kept_features_summary %>% 
  tibble::add_row(features = setdiff(co_corr_list, full_decision_list)) %>% 
  dplyr::mutate(., category = ifelse(is.na(category), "dropped_in_correlation", category))

kept_features_summary <- kept_features_summary %>% 
  tibble::add_row(features = full_decision_list) %>% 
  dplyr::mutate(., category = ifelse(is.na(category), "final_feature_list", category))

readr::write_delim(x = kept_features_summary, file = paste0(opt$input, "feature_summary.csv"), delim = ",", quote = NULL)

## write final file for ML =====================================================

for_ml <- corr_raw_data$dat_transformed %>% 
  dplyr::select(., any_of(full_decision_list))

readr::write_delim(for_ml, file = paste0(opt$input, opt$output_file), delim = ",", quote = NULL)
