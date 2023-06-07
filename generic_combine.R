#!/usr/bin/env Rscript 

## SCRIPT: generic_combine.R ===================================================
## AUTHOR: Andrew Oliver
## DATE:   Aug 8, 2022
##
## PURPOSE: To combine files cleaned from running
## the generic_read_in.R script

## docker info =================================================================
#docker run --rm -it -v /Users/$USER/Downloads/nutrition_tools/:/home aoliver44/nutrition_tools:1.1 bash

## general command:
## /home/scripts/generic_combine.R --cor_level 0.8 --cor_choose TRUE /home/output/ combined_ml_data.csv

## set working dir to /home for the docker container
setwd("/home")


## add commandline options =====================================================

library(docopt, quietly = T, verbose = F, warn.conflicts = F)
"Combine data from read_in step, prior to ML
Usage:
    generic_combine [--subject_identifier=<subject_colname> --label=<label> --cor_level=<cor_level> --cor_choose=<cor_choose> --preserve_samples=<preserve_samples>] <input> <output_file>
    
Options:
    -h --help  Show this screen.
    -v --version  Show version.
    --subject_identifier name of columns with subject IDs [default: subject_id]
    --label label of column for use in ML [default: label]
    --cor_level level of general feature correlation [default: 0.99]
    --cor_choose choose which features are kept in correlation [default: FALSE]
    --preserve_samples attempt to drop more features to keep samples [default: FALSE]
    
Arguments:
    input  FULL path of input directory containing files
    output_file  output file name

" -> doc

opt <- docopt::docopt(doc, version = 'generic_combine.R v0.3.0a.3\n\n')

## load libraries ==============================================================

library(dplyr, quietly = T, verbose = F, warn.conflicts = F)
library(tibble, quietly = T, verbose = F, warn.conflicts = F)
library(tidyr, quietly = T, verbose = F, warn.conflicts = F)
library(readr, quietly = T, verbose = F, warn.conflicts = F)
library(janitor, quietly = T, verbose = F, warn.conflicts = F)
library(mikropml, quietly = T, verbose = F, warn.conflicts = F)
library(Hmisc, quietly = T, verbose = F, warn.conflicts = F)
library(corrr, quietly = T, verbose = F, warn.conflicts = F)

## set random seed if needed
set.seed(1)

## helper functions ============================================================

## Negate function ("not in"):
`%!in%` <- Negate(`%in%`)

# opt <- data.frame(subject_identifier=character(),
#                                 label=character(),
#                                 cor_level=numeric(),
#                                 cor_choose=logical(),
#                                 preserve_samples=logical(),
#                                 input=character(),
#                                 output_file=character())
# opt <- opt %>% tibble::add_row(subject_identifier = c("subject_id"), label = c("age"), cor_level = 0.99, cor_choose = TRUE, preserve_samples = FALSE, input = c("/home/nutrition_tools/ultra_merge/output/"), output_file="merged_data.csv")

## suppress warnings
options(warn=-1)

## check for inputs ============================================================

## set working dir to /home for the docker container
setwd(opt$input)
full_path <- getwd()

## check and see if clean_files directory exists
cat("Checking for clean_files directory and summary_dataset_problems.csv", "\n\n")

fils <- list.files(paste0("clean_files"), full.names = TRUE, recursive = TRUE)
## check and make sure there are files clean_files dir
if (length(fils) >= 1) {
  cat(paste0("clean_files directory exists and is not empty: ", opt$input, "clean_files/"), "\n\n")
} else {
  stop("No files found.")
}

## check and make sure summary_dataset_problems exists
if (file.exists("summary_dataset_problems.csv")) {
  
  ## use summary_problems to check and see if problems still exist in data
  summary_problems <- read.csv("summary_dataset_problems.csv")
  summary_problems <- summary_problems %>% tidyr::drop_na()

  if (NROW(summary_problems > 1) && rowSums(summary_problems[,2:NCOL(summary_problems)]) > 0) {
  
  ## output messages
  cat(paste0("summary_dataset_problems file found: ", opt$input, "summary_dataset_problems.csv"), "\n\n")
  cat("Using file to check if clean_files have been fixed...", "\n\n")
  cat(paste0("Checking all files that were previously identified as problems"), "\n\n")
    
  ## make a tmp directory and copy problem files into
  dir.create(file.path(paste0("problem_check/")))
  ## check if problem files are fixed ============================================
  
  for (fil in summary_problems$dataset) {
    ## copy into a tmp directory
    if (file.exists(paste0("clean_files/", fil, ".csv"))) {
      file.copy(from = paste0("clean_files/", fil, ".csv"), to = "problem_check/", recursive = F) 
    }
  }
  
  ## run generic_read_in.R on this problem subset and see if any problems occur
  system(paste0("/scripts/generic_read_in --subject_identifier ",opt$subject_identifier," ",opt$input, "problem_check ", opt$input, "problem_check/output/"))
  
  ## check and see if summary_dataset_problems got written  
    if (file.exists("problem_check/output/summary_dataset_problems.csv")) {
      summary_problems_recheck <- read.csv(paste0(opt$input,"problem_check/output/summary_dataset_problems.csv"))
      summary_problems_recheck <- summary_problems_recheck %>% 
        dplyr::filter(., dataset != "Name of dataset") %>%
        dplyr::filter(., dataset != "NA")
      
      if (NROW(summary_problems_recheck >= 1)) {
        
        problem_check_dataset <- summary_problems_recheck[1,]$dataset
        problem_check_problem <- apply(summary_problems_recheck[1,], 1, function(x) last(colnames(summary_problems_recheck[1,])[x==1]))
        
        cat("\n","################################################", "\n")
        cat("WARNING:", "\n")
        cat(paste0("This program still thinks problems exist with the data. For example, did you fix ", problem_check_problem, " in ", problem_check_dataset, " dataset??"), "\n")
        cat("Since you didnt fix these problems (we worked so hard to tell you about them!!) we will remove these datasets from downstream analyses", "\n")
        cat("################################################", "\n\n")
        
        ## get interactive acknowledgment 
        cat("  Press [enter] to continue  ")
        x <- readLines(file("stdin"),1)
        
        ## remove summary_problem_datasets from fils list
        fils <- as.data.frame(fils) %>% 
          dplyr::filter(., !grepl(pattern = paste(summary_problems_recheck$dataset, collapse = "|"), x = fils)) %>%
          pull(., var = "fils")
      }
    }
  }
}

## read in & merge clean files =================================================

## read in the files that passed muster
if (length(fils) > 0) {
    myfiles = lapply(fils, read_csv) %>%
      suppressMessages() 
}

## merge them all together
full_merge <- suppressWarnings(Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = opt$subject_identifier, all = TRUE), myfiles))

## check sample size here ======================================================

if (NROW(full_merge) < 200) {
  cat("\n","################################################", "\n")
  cat("WARNING:", "\n")
  cat("You have VERY few samples. Overfitting is a major concern. Your results might only be applicable to your sample set. This problem is made much worse if you have a multiclass problem (> 2 classes for classification).", "\n")
  cat("################################################", "\n\n")
  
  ## get interactive acknowledgment 
  cat("  Press [enter] to continue  ")
  x <- suppressWarnings(readLines(file("stdin"),1))

} else if (NROW(full_merge) > 200 && NROW(full_merge) < 400) {
  cat("\n","################################################", "\n")
  cat("NOTE:", "\n")
  cat("This is a reasonably small ML dataset (sample-wise). But depending on what you are doing, it might work! We will assume you're an expert.", "\n")
  cat("################################################", "\n\n")
  
  ## get interactive acknowledgment 
  cat("  Press [enter] to continue  ")
  x <- suppressWarnings(readLines(file("stdin"),1))
  
}

## remove duplicated columns across datasets BY CORRELATION ====================

## NUMERIC

## only select the numeric columns
full_merge_numeric <- full_merge %>% dplyr::select((where(is.numeric)), -dplyr::any_of(c(opt$subject_identifier, opt$label)))
## get a list of high NA features to throw out, they dont correlate well
potential_duplicated_high_na <- colnames(full_merge_numeric[, colSums(!is.na(full_merge_numeric)) <= (0.45 * NROW(full_merge_numeric))])
## remove those features from being correlated
full_merge_numeric <- full_merge_numeric[, colSums(!is.na(full_merge_numeric)) >= (0.5 * NROW(full_merge_numeric))]

## run correlations and keep anything with abs(cor) > 0.999
tmp_cor <- cor(full_merge_numeric, use = "complete.obs")
tmp_cor <- reshape2::melt(as.matrix(tmp_cor))
tmp_cor <- tmp_cor %>% filter(., abs(value) > 0.99)

## if things were correlated:
if (NROW(tmp_cor) > 0) {
  potential_duplicated <- unique(tmp_cor$Var1)
  
  ## merge with NA data
  na_count <- full_merge %>% 
    dplyr::summarise_all(~ sum(is.na(.))) %>% t() %>% 
    as.data.frame() %>% tibble::rownames_to_column(., var = "feature") %>% 
    dplyr::rename(., "na_count" = "V1")
  
  cor_na <- merge(tmp_cor, na_count, by.x = "Var1", by.y = "feature") %>% 
    dplyr::rename(., "na_count_Var_1" = "na_count")
  
  cor_na <- merge(cor_na, na_count, by.x = "Var2", by.y = "feature") %>% 
    dplyr::rename(., "na_count_Var_2" = "na_count")
  
  ## create list of things correlated with themselves and ONLY themselves
  singletons <- cor_na %>% 
    dplyr::count(Var1, name= "Count_Var1") %>% 
    dplyr::filter(., Count_Var1 == 1) %>%
    dplyr::pull(., Var1)
  
  ## remove rows which only compare self against self
  cor_na <- subset(cor_na, Var1 != Var2)
  
  ## select only distinct pairwise comparisons (remove reverse comparison)
  cor_na <- cor_na[!duplicated(data.frame(t(apply(cor_na[1:2], 1, sort)), cor_na$value, cor_na$na_count_Var_1, cor_na$na_count_Var_2)),]
  
  ## select best variable based on na_count
  cor_na <- cor_na %>% 
    dplyr::mutate(., keep = ifelse(na_count_Var_1 >= na_count_Var_2, "keep", "toss")) %>%
    dplyr::filter(., keep == "keep") %>%
    dplyr::filter(., Var2 %!in% Var1)
  
  ## get the vars that are likely duplicated but have most data
  best_vars_across_datasets <- unique(cor_na$Var2) 
  best_vars_across_datasets <- c(best_vars_across_datasets, singletons)
  
  ## get the difference between the previous list and what we started with
  drop_duplicated <- potential_duplicated[potential_duplicated %!in% best_vars_across_datasets]
  
  ## remove these from the main dataframe + high NA features
  full_merge_dedup <- full_merge %>% dplyr::select(., -dplyr::any_of(drop_duplicated), -dplyr::any_of(potential_duplicated_high_na))
  
  ## remove features that are all NA...for some crazy reason these sometimes exist
  ## because people are imperfect and gather imperfect data. We still gotta love though
  full_merge_dedup <- full_merge_dedup[, unlist(lapply(full_merge_dedup, function(x) !all(is.na(x))))]
  
} else {
  full_merge_dedup <- full_merge
  full_merge_dedup <- full_merge_dedup[, unlist(lapply(full_merge_dedup, function(x) !all(is.na(x))))]
}

## CHARACTERS
## get the vars that are characters (not numeric like above)
full_merge_dedup_characters <- full_merge_dedup %>% dplyr::select(where(is.character), dplyr::any_of(opt$subject_identifier), -dplyr::any_of(c(opt$label)))

## if there appears to be duplicated features (_2 at end of var)
if (length(grep("_2$", colnames(full_merge_dedup_characters))) > 0) {
  ## warn users about this:
  cat("\n","################################################", "\n")
  cat("NOTE:", "\n")
  cat("We will drop character columns with the same name 
    (keeping the column with fewest NAs). NOTE: If you have
    character columns with the same name but different data,
    firstly, shame. Secondly, we're about to delete one...", "\n")
  cat("################################################", "\n\n")
  
  
  ## gather the data, remove the _[1-9] and keep the one with least NA
  full_merge_char_dedup <- full_merge_dedup_characters %>% 
    tibble::column_to_rownames(., opt$subject_identifier) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column(., var = "feature") %>% 
    dplyr::mutate(., feature_new = gsub("\\.x$", "", gsub("\\.y$", "", gsub("_[1-9]$", "", feature)))) %>% 
    dplyr::arrange(rowSums(is.na(.))) %>% 
    dplyr::distinct(feature_new, .keep_all = TRUE) %>% 
    tibble::column_to_rownames(., var = "feature") %>%
    t() %>%
    as.data.frame() %>%
    readr::type_convert(.) %>%
    tibble::rownames_to_column(., var = opt$subject_identifier) %>%
    suppressMessages() 
  
  ## figure out the ones that get dropped
  drop_char_columns <- colnames(full_merge_dedup_characters)[colnames(full_merge_dedup_characters) %!in% colnames(full_merge_char_dedup)]
  
  full_merge_dedup <- full_merge_dedup %>% dplyr::select(., -dplyr::any_of(drop_char_columns))
}

## drop duplicated label column 

full_merge_label_dedup <- full_merge_dedup %>% 
  tibble::column_to_rownames(., opt$subject_identifier) %>%
  dplyr::select(., tidyr::matches(c(paste0("^", opt$label, "$"), paste0("^", opt$label, "_[1-9]$")), perl = T)) 

if (NCOL(full_merge_label_dedup) > 1) {
  na_labels <- colSums(is.na(full_merge_label_dedup))
  if (max(na_labels) == min(na_labels)) {
    drop_label_impersonator <- setdiff(colnames(full_merge_label_dedup), opt$label)
    full_merge_dedup <- full_merge_dedup %>% dplyr::select(-dplyr::any_of(drop_label_impersonator))
  } else {
    drop_label_impersonator <- rownames((as.data.frame(sort(na_labels, decreasing = F))))[2:length(na_labels)]
    full_merge_dedup <- full_merge_dedup %>% dplyr::select(-dplyr::any_of(drop_label_impersonator))
    opt$label <- rownames((as.data.frame(sort(na_labels, decreasing = F))))[1]
  }
}

## write full_merge_dedup to file ==============================================

## write full_merge_dedup to file (contains NAs)
readr::write_delim(full_merge_dedup, file = paste0(full_path, "/", "merged_data_with_NAs.csv"), delim = ",", quote = NULL)

## check if label is in dataset ================================================

if (opt$label %!in% colnames(full_merge_dedup)) {
  cat(opt$label, " is specified for --label. Checking to make sure it's there...\n\n")
  stop("Cannot find --label in datasets")
}

## keep a dataframe seperately with just the subject id and label
## sometimes people have labels with tons of NAs, which will get dropped

pre_corr_label_df <- full_merge_dedup %>% 
  dplyr::select(., as.character(opt$label), as.character(opt$subject_identifier)) %>%
  tidyr::drop_na()

## drop samples that have NAs in the label...ultimately useless for ML
full_merge_dedup <- full_merge_dedup %>% tibble::column_to_rownames(., var = opt$subject_identifier) %>% dplyr::filter(., as.character(opt$label) != "NA")

## pre-corr col drop ===========================================================

## lets initally drop columns that are mostly NA already (50% or greater)
full_merge_dedup_init_filt <- full_merge_dedup[, colSums(!is.na(full_merge_dedup)) >= (0.5 * NROW(full_merge_dedup))]
## lets initally drop rows that are mostly NA already (75% or greater)
full_merge_dedup_init_filt <- full_merge_dedup_init_filt[rowSums(!is.na(full_merge_dedup_init_filt)) >= (0.75 * NCOL(full_merge_dedup_init_filt)), ]

## calculate cutoffs of features to preserve samples or more features 
## NOTE: preserving features at this point works pretty well because of the initial filters
preserve_features_cutoff<- as.numeric(quantile(colSums(is.na(full_merge_dedup_init_filt)))[2])
preserve_samples_cutoff<- mean(c(as.numeric(quantile(colSums(is.na(full_merge_dedup_init_filt)))[1]), as.numeric(quantile(colSums(is.na(full_merge_dedup_init_filt)))[2])))

## drop columns with a lot of NAs
if (opt$preserve_samples == TRUE) {
  ## notify user of cutoffs
  cat("\n","################################################", "\n")
  cat("NOTE:", "\n")
  cat("(preserve_samples=TRUE) Automatic sample preservation guess: If features have greater than", preserve_samples_cutoff, "NAs they will be dropped...\n")
  cat("################################################", "\n\n")
  full_merge_dedup_tmp_col_drop <- full_merge_dedup_init_filt[ , colSums(is.na(full_merge_dedup_init_filt)) <= (preserve_samples_cutoff)]
  
  ## Notify user about what columns were dropped
  if (((NCOL(full_merge_dedup_init_filt)) - (NCOL(full_merge_dedup_init_filt[ , colSums(is.na(full_merge_dedup_init_filt)) <= (preserve_samples_cutoff)]))) > 0) {
    cat("\n","################################################", "\n")
    cat("NOTE:", "\n")
    cat("In order to have a complete dataset, we dropped some NA-replete features. 
    We will next row-drop (drop samples) in order to have a NA-free dataset. 
      This is only for a global correlation check. We will write both a NA and 
      NA-free file, its up to you if you want to use interpolation methods.", "\n")
    cat("################################################", "\n\n")
  }
} else {
  ## notify user of cutoffs
  cat("\n","################################################", "\n")
  cat("NOTE:", "\n")
  cat("(preserve_samples=FALSE) Automatic feature preservation guess: If features have greater than", preserve_features_cutoff, " NAs they will be dropped...\n")
  cat("################################################", "\n\n")
  full_merge_dedup_tmp_col_drop <- full_merge_dedup_init_filt[ , colSums(is.na(full_merge_dedup_init_filt)) <= (preserve_features_cutoff)]
  
  ## Notify user about what columns were dropped
  if (((NCOL(full_merge_dedup_init_filt)) - (NCOL(full_merge_dedup_init_filt[ , colSums(is.na(full_merge_dedup_init_filt)) <= (preserve_features_cutoff)]))) > 0) {
    cat("\n","################################################", "\n")
    cat("NOTE:", "\n")
    cat("In order to have a complete dataset, we dropped some NA-replete features. 
    We will next row-drop (drop samples) in order to have a NA-free dataset. 
      This is only for a global correlation check. We will write both a NA and 
      NA-free file, its up to you if you want to use interpolation methods.", "\n")
    cat("################################################", "\n\n")
  }
}



## pre-corr row drop ===========================================================

## ok lets remove rows that are mostly NA (admittedly a much smaller dataset)...but no NAs left!
full_merge_dedup_tmp_row_drop <- full_merge_dedup_tmp_col_drop %>% tidyr::drop_na()

## co-correlate features =======================================================

## co-correlation of features and for one-hot-encoding.
full_merge_dedup_pre_cor <- full_merge_dedup_tmp_row_drop %>% 
  #dplyr::mutate(., dummy_var = sample(c(0,1), size = NROW(.), replace = T)) %>%
  tibble::rownames_to_column(., var = as.character(opt$subject_identifier))

## add back in label if dropped ================================================

if (as.character(opt$label) %!in% colnames(full_merge_dedup_pre_cor)) {
  full_merge_dedup_pre_cor <- merge(pre_corr_label_df, full_merge_dedup_pre_cor, by = as.character(opt$subject_identifier))
  full_merge_dedup_pre_cor <- full_merge_dedup_pre_cor %>% tidyr::drop_na()
  }

## check correlation level

if (as.numeric(opt$cor_level) < 0.99) {
  cat("\n","################################################", "\n")
  cat("WARNING:", "\n")
  cat("Your correlation level is below 0.99. This is a global correlation check, 
  mainly for PURELY redundant features. For feature engineering, PROPER correlation-based 
  feature selection should happen inside a Train-Test split or a cross-validation 
  procedure. Not doing so constitutes DATA LEAKAGE. You risk overfitting and 
  reporting results that look better than they probably are.", "\n")
  cat("################################################", "\n\n")
  
  ## get interactive acknowledgment 
  cat("  Press [enter] to continue  ")
  x <- suppressWarnings(readLines(file("stdin"),1))

}

## prepare the data for correlation (one-hot encode, remove zero-variance)
corr_raw_data <- suppressMessages(suppressWarnings(mikropml::preprocess_data(dataset = full_merge_dedup_pre_cor,
                                         method = NULL,
                                         outcome_colname = as.character(opt$label), 
                                         collapse_corr_feats = F, 
                                         remove_var = "zv")))
post_mikrop_data <- corr_raw_data$dat_transformed 

## correlation will not happen if label is non-numeric
if (class(corr_raw_data$dat_transformed[[opt$label]]) != "numeric") {
  label_df <- corr_raw_data$dat_transformed %>% dplyr::select(., opt$label, subject_id)
  corr_raw_data$dat_transformed <- corr_raw_data$dat_transformed %>% dplyr::select(., -opt$label, -subject_id)
}

## co-correlate features at specified threshold
high_cor <- mikropml:::group_correlated_features(corr_raw_data$dat_transformed, 
                                                corr_thresh = as.numeric(opt$cor_level), group_neg_corr = T)

## make dataframe of what is correlated at specified threshold.
high_cor <- as.data.frame(high_cor) %>% 
  tidyr::separate(., col = high_cor, into = c("keep", "co-correlated"), sep = "\\|", extra = "merge")


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
if (length(co_corr_list) > 0) {
  
  correlate_figure <- corr_raw_data$dat_transformed %>% 
    dplyr::select(., any_of(co_corr_list))
  correlate_figure <- corrr::correlate(x = correlate_figure, method = "pearson") %>% 
    tibble::column_to_rownames(., var = "term") %>% abs() %>%
    suppressMessages()
  
  ## set anything below cor threshold to zero
  v1 <- colnames(correlate_figure)
  correlate_figure[v1] <- lapply(correlate_figure[v1], function(x) replace(x,  (x < (as.numeric(opt$cor_level) - 0.000001)), 0))
  
  ## remove cols and rows that are NAs
  correlate_figure <- correlate_figure[ , colSums(correlate_figure, na.rm = T) > 0]
  correlate_figure <- correlate_figure[ rowSums(correlate_figure, na.rm = T) > 0, ]
  
  if (NROW(correlate_figure) > 3) {
    
    pdf("correlation_heatmap.pdf")
    heatmap(as.matrix(correlate_figure), keep.dendro = TRUE)
    dev.off()
    
  } else {
    cat("\n","################################################", "\n")
    cat("WARNING:", "\n")
    cat("You have too few correlated variables for correlation figure to work", "\n")
    cat("We printed the correlation matrix to file, might help!", "\n")
    cat("################################################", "\n\n")
    readr::write_delim(x = correlate_figure, file = paste0(opt$input, "correlated_features_matrix.csv"), delim = ",", quote = NULL)
  }
  
  
  ## decide which co-correlated vars to keep =====================================
  
  if (opt$cor_choose == TRUE) {
    cat("\n","################################################", "\n")
    cat("NOTE:", "\n")
    cat("You have --corr_choose TRUE.", "\n")
    cat("The following features are correlated at --cor_level.", "\n") 
    cat("Please chose which one you want in the final dataset by selecting the corresponding nymber", "\n")
    cat("################################################", "\n\n")
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
    if (class(corr_raw_data$dat_transformed[[opt$label]]) != "numeric") {
      full_decision_list <- c(un_corr_list, corr_decision_list, opt$label)
    } else {
      full_decision_list <- c(un_corr_list, corr_decision_list)
    }
  } else {
    if (class(corr_raw_data$dat_transformed[[opt$label]]) != "numeric") {
      full_decision_list <- c(high_cor$keep, opt$label)
    } else {
      full_decision_list <- high_cor$keep
    }
  }
} else {
  
  if (class(corr_raw_data$dat_transformed[[opt$label]]) != "numeric") {
    full_decision_list <- c(high_cor$keep, opt$label)
  } else {
    full_decision_list <- high_cor$keep
  }
  
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

readr::write_delim(x = kept_features_summary, file = paste0(full_path, "/feature_summary.csv"), delim = ",", quote = NULL)

## write final file for ML =====================================================

for_ml <- post_mikrop_data %>% 
  dplyr::select(., any_of(full_decision_list), opt$subject_identifier)

readr::write_delim(for_ml, file = paste0(full_path, "/", opt$output_file), delim = ",", quote = NULL)
