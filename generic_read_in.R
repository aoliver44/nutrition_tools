#!/usr/bin/env Rscript 

## SCRIPT: generic_read_in.R ===================================================
## AUTHOR: Andrew Oliver
## DATE:   May 31, 2022
##
## PURPOSE: To read in data in a flat file
## format, which may have copies of columns/rows
## and columns may be duplicated across different 
## datasets

## docker info =================================================================

## docker command:
#docker run --rm -it -v /Users/$USER/Downloads/nutrition_tools/:/home aoliver44/nutrition_tools:1.1 bash

## general command:
## /home/scripts/generic_read_in.R --subject_identifier subject_id /home/data/read_in_tests/ /home/output

## set working dir to /home for the docker container
setwd("/home")

## add commandline options =====================================================

library(docopt, quietly = T, verbose = F, warn.conflicts = F)
"Read in a directory of data and make checks prior to ML
Usage:
    generic_read_in [--subject_identifier=<subject_colname>] <input> <outdir>
    
Options:
    -h --help  Show this screen.
    -v --version  Show version.
    --subject_identifier name of columns with subject IDs [default: subject_id]

Arguments:
    input  input directory containing files
    outdir  output directory name

" -> doc

opt <- docopt(doc, version = 'generic_read_in.R v1.0\n\n')

## load libraries ==============================================================

library(tibble, quietly = T, verbose = F, warn.conflicts = F)
library(janitor, quietly = T, verbose = F, warn.conflicts = F)
library(readr, quietly = T, verbose = F, warn.conflicts = F)
library(tidyr, quietly = T, verbose = F, warn.conflicts = F)
library(reshape2, quietly = T, verbose = F, warn.conflicts = F)
library(dplyr, quietly = T, verbose = F, warn.conflicts = F)
library(digest, quietly = T, verbose = F, warn.conflicts = F)
library(ggplot2, quietly = T, verbose = F, warn.conflicts = F)

## helper functions ============================================================

## Negate function ("not in"):
`%!in%` <- Negate(`%in%`)
## Date function:
is.convertible.to.date <- function(x) !is.na(as.Date(as.character(x), 
                                                     tz = 'UTC', 
                                                     format = c('%m/%d/%Y %H:%M', '%m/%d/%Y')))

## ideas from naniar::common_na_strings
na_possibilities <- c("NA", "N A", "N/A", "#N/A", " NA", "NA ", "N / A", 
                      "N /A", "N / A ", "na", "n a", "n/a", " na", "na ", 
                      "n /a", "n / a", "a / a", "n / a ", "NULL", "null", "", 
                      ".", "*", "?", "-", " - ")

## suppress warnings
options(warn=-1)

## practice flags
# opt <- data.frame(subject_identifier=character(),
#                   input=character(),
#                   outdir=character())
# opt <- opt %>% tibble::add_row(subject_identifier = c("subject_id"), input = c("/home/scripts/simulated_data/"), outdir="/home/simulated_output")

## create output directories ===================================================

outdir_name <- opt$outdir

## check to see if outdir already exists, if so break
if (dir.exists(opt$outdir) == TRUE) {
  stop("Output directory already exists, overwrite behavior not available")
}

dir.create(file.path(paste0(outdir_name)))
dir.create(file.path(paste0(outdir_name, "/duplicated_data")))
dir.create(file.path(paste0(outdir_name, "/duplicated_colnames")))
dir.create(file.path(paste0(outdir_name, "/duplicated_rows")))
dir.create(file.path(paste0(outdir_name, "/clean_files")))

## set random seed if needed
set.seed(1)

## check input dir not empty ===================================================

## create list of files to read in based on path
fils <- list.files(opt$input, full.names = TRUE, recursive = TRUE)
## check and make sure there are files in the path
if (length(fils) < 1) {
  cat(paste0("ERROR: We did not detect any files in ", opt$path, " please check your path or folder."), "\n\n")
}

## create NA count file ========================================================

## create empty dataframe that will house the NA count data eventually
na_count_features <- data.frame(dataset=character(),
                                feature=character(), 
                                data_feature_hash=character(),
                                na_count=numeric(),
                                have_data=numeric(),
                                stringsAsFactors=FALSE) 


## create summary problems file  ===============================================

## create empty dataframe that will house summary problem files
summary_problems <- data.frame(dataset=character(),
                               subject_id_not_found=character(), 
                               subject_id_duplicated=character(),
                               date_not_unix=character(),
                               duplicated_data=character(),
                               duplicated_column_names=character(),
                               data_are_symbols=character(),
                               stringsAsFactors=FALSE) 

## read in files  ==============================================================

## loop through files and make checks
for (file in fils) {
  file_name <- janitor::make_clean_names(strsplit(basename(file), split="\\.")[[1]][1])
  cat("\n\n",paste0("Reading in file: ", file_name), "\n")
  ## Read in all the files, do not fix colnames
  if (strsplit(basename(file), split="\\.")[[1]][2] == "csv") {
    f <- readr::read_delim(file, delim = ",", 
                           name_repair = "minimal", na = na_possibilities) %>%
      suppressMessages() 
    
  } else if (strsplit(basename(file), split="\\.")[[1]][2] %in% c("tsv","txt")){
    f <- readr::read_delim(file, delim = "\t",
                           name_repair = "minimal", na = na_possibilities) %>%
      suppressMessages() 
    
  } # else if (strsplit(basename(file), split="\\.")[[1]][2] == "xls") {
  #   f <- suppressMessages(readxl::read_xls(file, .name_repair = "minimal", na = na_possibilities))
  # } else if (strsplit(basename(file), split="\\.")[[1]][2] == "xlsx") {
  #   f <- suppressMessages(readxl::read_xlsx(file, .name_repair = "minimal", na = na_possibilities))
  # }
  ## commented out because alpine doesnt install readxl
  ## and it might be a little dumb to read in xlsx 
  
  ## assign file name to variable/column
  f$dataset <- file_name
  
  ## add _x to end of cols to ensure clean_names add unique ids past that 
  ## (it will coerce to unique, and i want to know where the unique id is)
  ## i.e. fiber_x_2...keeps me sure that its a duplicate
  colnames(f) <- paste0(colnames(f), '_x')
  f <- f %>% janitor::clean_names() 
  
  ## subject id check  ===========================================================
  
  ## check to see if subject_id is in the file
  cat(paste0("Checking to see if ", opt$subject_identifier," is in dataset..."), "\n")
  if (paste0(opt$subject_identifier, "_x") %in% names(f) == FALSE) { 
    
    cat("\n","################################################", "\n")
    cat("WARNING:", paste0(opt$subject_identifier, " not found in ", file_name, " dataset. That data will not be included in the analysis."), "\n\n") 
    cat("################################################", "\n\n")
    cat("Press [enter] to acknowledge ")
    
    ## add to a summary problem file
    summary_problems <- summary_problems %>% tibble::add_row(dataset = file_name, 
                                                             subject_id_not_found = "needs_check")
    
    x <- readLines(file("stdin"),1)

    next 
  } else { 
    f <- f %>% rename(., "subject_id_x" = paste0(opt$subject_identifier, "_x"))
    cat(paste0("Renaming ", opt$subject_identifier, " to subject_id. No change if you are already using subject_id"), "\n")
  }
  
  ## dup subject id check  =======================================================
  
  ## checking to see if there are duplicate subject_ids in the file - Make unique
  if (NROW(f %>% janitor::get_dupes(., c(subject_id_x))) > 1) {
    cat("\n","################################################", "\n")
    cat("WARNING:",paste("We detected rows in", file_name, "with the same subject_id."), "\n\n")
    cat("We will rename the subjects for now", "\n")
    cat("Note: if this is longitudinal data, (i.e patient is measured multiple times), take care in choosing the approrpiate ML method and CV strategy. Lme4 and other models may be more useful, but you have the power to decide!", "\n")
    cat("################################################", "\n\n")
    cat(" Press [enter] to acknowledge  ")
    
    ## add to a summary problem file
    summary_problems <- summary_problems %>% tibble::add_row(dataset = file_name, 
                                                             subject_id_duplicated = "needs_check")
    
    x <- readLines(file("stdin"),1)

    duplicated_rows <- f %>% janitor::get_dupes(., c(subject_id_x))
    f$subject_id_x <- janitor::make_clean_names(f$subject_id_x)
    
  }
  
  ## melting to long form
  g <- f %>% reshape2::melt(., id.vars = c("subject_id_x", "dataset_x")) %>%
    dplyr::rename(., "feature" = "variable")
  
  ## you eventually need features to be be duplicates again, not the unique
  ## names that clean_names did (i.e. *_x_2)
  ## so make a new column of dataset_feature_hash which will be unique to 
  ## each feature--help you keep track of duplicated colnames
  g <- g %>% 
    dplyr::mutate(.,  combined_name = paste(g$dataset, g$feature, sep="_")) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(., data_feature_hash = digest::digest(combined_name, algo="md5", serialize = F)) %>%
    dplyr::select(., -combined_name)
  
  ## getting rid of the clean names artifact now. we have
  ## data_feature_hash! Woohoo
  g$feature <- gsub(pattern = "_[0-9]$", replacement = "", x = g$feature)
  g$feature <- gsub(pattern = "_x$", replacement = "", x = g$feature)
  colnames(g) <- gsub("_x", "", colnames(g))
  
  
  ## check for POSIXct date  =====================================================
  
  ## checking for dates and removing them until the end
  ## to do: find a way to convert them to unix time stamps
  g$date <- is.convertible.to.date(g$value)
  
  if ((g %>% dplyr::filter(., date == "TRUE") %>% nrow()) > 0) {
    
    rm( list = base::Filter( exists, c("date_features", "date_dataframe") ) )
    
    cat("\n","################################################", "\n")
    cat("WARNING: I think you have POSIXct time/date data included. These often mess up downstream analyses. You should try and convert it to a UNIX timestamp.", "\n")
    cat("################################################", "\n\n")
    
    print(g %>% dplyr::filter(., date == "TRUE") %>% dplyr::select(., feature) %>% base::unique() %>% dplyr::pull(.))
    date_features <- g %>% dplyr::filter(., date == "TRUE") %>% dplyr::select(., feature) %>% base::unique() %>% dplyr::pull()
    
    date_dataframe <- f %>% dplyr::select(., subject_id_x, dplyr::contains(date_features))
    
    g <- g %>% 
      dplyr::filter(., date == "FALSE") %>%
      dplyr::select(., -date) 
    
    cat("Press [enter] to acknowledge ")
    
    ## add to a summary problem file
    summary_problems <- summary_problems %>% tibble::add_row(dataset = file_name, 
                                                             date_not_unix = "needs_check")
    
    
    x <- readLines(file("stdin"),1)
    
  }
  
  ## check for non-alphanumeric  =================================================
  
  ## checking to see if values are not alpha_numeric
  g_tmp <- g %>% as.data.frame() %>% dplyr::filter(., !is.na(value))
  g_tmp$alpha_numeric <- grepl("^([A-Z])|([a-z]|[0-9])", g_tmp$value, ignore.case = TRUE)
  g_tmp <- dplyr::filter(g_tmp, alpha_numeric == FALSE)
  if (NROW(g_tmp) > 0) {
    
    cat("\n","################################################", "\n")
    cat("WARNING: You have cells in your data that contain only a symbol. We cant do anything with that.", "\n")
    cat("We check against a large list of possible NAs. Are you using a really weird NA symbol?", "\n")
    cat("################################################", "\n\n")
    
    cat("Press [enter] to acknowledge ")
    
    ## add to a summary problem file
    summary_problems <- summary_problems %>% tibble::add_row(dataset = file_name,
                                                             data_are_symbols = "needs_check")
    
    x <- readLines(file("stdin"),1)
    
  }
  
  
  ## count NAs  ==================================================================
  
  ## what features are missing  a ton of data
  na_count_features_tmp <- g %>%
    dplyr::group_by(., dataset, feature, data_feature_hash) %>%
    dplyr::summarise(., na_count = sum(is.na(value)), have_data = sum(!is.na(value))) %>%
    dplyr::arrange(desc(na_count)) %>%
    suppressMessages() 
  
  na_count_features <- rbind(na_count_features, na_count_features_tmp)
  
  
  ## get duplicated columns  =====================================================
  
  ## what features are duplicated in the na_count_features
  duplicate_check <- na_count_features_tmp %>%
    janitor::get_dupes(., feature) %>% 
    tibble::add_column(duplicated_value = NA, .after = "have_data")
  
  if (NROW(duplicate_check > 1)) {
    g_dt <- as.data.table(g)
    
    ### do dup cols have same data?  ===============================================
    
    ## loop through features in duplicates (by name), 
    ## check if they are the same values
    for (f_duplicated in unique(duplicate_check$feature)) {
      check <- g_dt %>% filter(., feature == f_duplicated)
      check <- as.data.frame(check)
      tmp <- check %>%
        tidyr::pivot_longer(-subject_id) %>%
        tidyr::drop_na %>%
        dplyr::group_by(subject_id, name) %>%
        dplyr::mutate(n = 1:n()) %>%
        dplyr::ungroup %>%
        tidyr::pivot_wider %>%
        dplyr::select(-n) %>%
        readr::type_convert(.) %>%
        tidyr::drop_na() %>%
        suppressMessages() 
      
      ## for each feature, round numerics and convert character
      if (is.numeric(tmp$value) == TRUE) { tmp$value  <- round(tmp$value, 2) 
      } else if (is.character(tmp$value) == TRUE) 
      { tmp$value <- sapply(tmp$value, janitor::make_clean_names, USE.NAMES = F) }
      
      
      if (NROW(tmp %>% janitor::get_dupes(., c(subject_id, value))) > (NROW(tmp)*0.2)) {
        ## recording that there might be a duplicated value
        duplicate_check <- duplicate_check %>%
          dplyr::mutate(., duplicated_value = ifelse(feature == f_duplicated, "duplicated_values", duplicated_value))
        
        ## printing to file for you to manually check if data is duplicated
        cat("\n", paste(f_duplicated, "is a DUPLICATED column name which contains DUPLICATED values across columns EXAMPLE:"), "\n\n")
        
        print(tmp %>% tidyr::pivot_wider(.,names_from = data_feature_hash, values_from = value) %>% dplyr::arrange(., subject_id) %>% head(n = 5))
        
        tmp %>% tidyr::pivot_wider(.,names_from = data_feature_hash, values_from = value) %>% 
          dplyr::arrange(., subject_id) %>% 
          readr::write_delim(., file = paste0(outdir_name, "/duplicated_data/", file_name, "_", f_duplicated, ".csv"), delim = ",") %>%
          suppressMessages() 
        
        cat(paste0("Printing ", f_duplicated, " data to file for manual checks. See outputs/duplicated_data/"), "\n\n")
        ## keeping a superset of the distinct values and dropping from g
        ## anything not in that superset
        tmp_de_dup <- tmp %>%
          dplyr::arrange(., value) %>% 
          dplyr::distinct(., subject_id, .keep_all = T)
        drop_features <- tmp[tmp$data_feature_hash %!in% tmp_de_dup$data_feature_hash, ]
        g <- g %>% dplyr::filter(., data_feature_hash %!in% drop_features$data_feature_hash)
        
        cat("Press [enter] to continue ")
        
        ## add to a summary problem file
        summary_problems <- summary_problems %>% tibble::add_row(dataset = file_name, 
                                                                 duplicated_data = "needs_check")
        
        
        x <- readLines(file("stdin"),1)
      

      } 
      
      ## do dup cols have different data?  ===========================================
      
      ## or if the data is truly unique, but had the same col names, start
      ## the process of renaming the features. Also write to duplicated_colnames/
      else  {
        ## showing you why we think it is a duplicated value
        cat(paste(f_duplicated, "is a DUPLICATED column name which contains UNIQUE values across columns EXAMPLE:"), "\n\n")
        
        print(tmp %>% tidyr::pivot_wider(.,names_from = data_feature_hash, values_from = value) %>% dplyr::arrange(., subject_id) %>% head(n = 5))
        
        cat("Renaming feature to feature + dataset to keep it unique (if it is the same it will get dropped in correlation)", "\n\n")
        
        ## renaming feature in g
        g <- g %>%
          dplyr::mutate(., feature = ifelse(feature == f_duplicated, paste0(feature,"_",data_feature_hash), feature))
        ## writing to file for you to check later
        cat("We wrote this to file for you to manually check, see outputs/duplicated_colnames")
        tmp %>% tidyr::pivot_wider(.,names_from = data_feature_hash, values_from = value) %>% 
          dplyr::arrange(., subject_id) %>% 
          readr::write_delim(., file = paste0(outdir_name, "/duplicated_colnames/", file_name, "_", f_duplicated, ".csv"), delim = ",") %>%
          suppressMessages() 
        
        cat("Press [enter] to continue ")
        
        ## add to a summary problem file
        summary_problems <- summary_problems %>% tibble::add_row(dataset = file_name, 
                                                                 duplicated_column_names = "needs_check")
        
        x <- readLines(file("stdin"),1)

      }
      
    }
    
  } 
  
  ## add back in POSIX date data  ================================================
  
  ## Add back in the date data
  tmp <- g %>% tidyr::pivot_wider(.,names_from = feature, values_from = value, id_cols = subject_id) %>%
    readr::type_convert(., na = c("na", "nan", "NA")) %>%
    suppressMessages() 
  
  if (exists("date_dataframe")) {
    colnames(date_dataframe) <- gsub(pattern = "_x", "", colnames(date_dataframe))
    tmp <- merge(tmp, date_dataframe, by = "subject_id")
  }
  
  ## clean up date data intermediate files
  rm( list = base::Filter( exists, c("date_features", "date_dataframe") ) )
  
  ## remove unique subject ID bandaid  ===========================================
  
  ## remove unique identifiers for repeated subject_ids
  tmp$subject_id <- gsub("_[0-9]{1,2}$", "", perl = T, x = tmp$subject_id) 
  readr::write_delim(tmp, file = paste0(outdir_name, "/clean_files/", file_name,".csv"), delim = ",") %>%
    suppressMessages() 
  
}

## write figures and files  ====================================================

## print na figure and data to file
if (length(fils) == 1) {
  na_figure <- na_count_features %>%
    dplyr::mutate(., dataset = gsub(pattern = "_", replacement = "\n", x = dataset, perl = T)) %>%
    dplyr::filter(., na_count > 0) %>%
    ggplot2::ggplot(aes(x = reorder(feature, na_count), weight = as.numeric(na_count))) +
    geom_bar() +
    labs(y = "NA Count", x = "Features with NAs") +
    theme(axis.text.x=element_text(angle = 45, size = 2), axis.ticks.x=element_blank())
} else { # facet by dataset
  na_figure <- na_count_features %>%
    dplyr::mutate(., dataset = gsub(pattern = "_", replacement = "\n", x = dataset, perl = T)) %>%
    dplyr::filter(., na_count > 0) %>%
    ggplot2::ggplot(aes(x = reorder(feature, na_count), weight = as.numeric(na_count))) +
    geom_bar() +
    labs(y = "NA Count", x = "Features with NAs") +
    facet_grid(~ dataset, scales = "free") +
    theme(axis.text.x=element_text(angle = 45, size = 2), axis.ticks.x=element_blank())
}

cat("\n\n", "Saving NA counts figure to file, see /outputs/na_counts.pdf", "\n\n")
cat("Saving NA counts table to file, see /output/na_counts.csv", "\n\n")
cat("####################################################", "\n")
cat("Saving a summary_problems file", "\n" ,"see /output/summary_dataset_problems.csv", "\n")
cat("####################################################", "\n\n")

readr::write_delim(na_count_features, file = paste0(outdir_name, "/na_counts.csv"), delim = ",") %>%
  suppressMessages() 

suppressWarnings(ggsave(paste0(outdir_name,"/na_counts.pdf"), plot = last_plot(), scale = 1, width = 15, height = 5, units = "in", dpi = 500, limitsize = TRUE, bg = NULL))

## write summary problems dataframe to file 
summary_problems_tmp <- summary_problems[1:NROW(summary_problems), ] %>% dplyr::group_by(dataset) %>% dplyr::summarise(across(everything(), ~n_distinct(., na.rm = T)))
readr::write_delim(summary_problems_tmp, file = paste0(outdir_name, "/summary_dataset_problems.csv"), delim = ",") %>%
  suppressMessages() 
