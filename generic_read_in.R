## SCRIPT: generic_read_in.R ===================================================
## AUTHOR: Andrew Oliver
## DATE:   May 31, 2022
##
## PURPOSE: To read in data in a flat file
## format, which may have copies of columns/rows
## and columns may be duplicated across different 
## datasets


## docker command:
#docker run --rm -it -p 8787:8787 
#-e PASSWORD=yourpasswordhere 
#-v /Users/andrew.oliver/Documents/active_projects_github-USDA/nutrition_tools/:/home 
#amr_r_env:3.1.0

## add commandline options =====================================================

library(optparse)
option_list = list(
  make_option(c("-i", "--input"), type="character", default="/home/data/read_in_tests/", 
              help="path to folder with data to import [default= %default]", metavar="character"),
  make_option(c("-s", "--subject_identifier"), type="character", default="subject_id", 
              help="subject key (column name) found in all files [default= %default]", metavar="character"),
  make_option(c("-o", "--outdir"), type="character", default=paste0("outputs_", format(Sys.time(), format = "%y_%m_%d_%H%M%S")),
              help="output directory to write [default= %default]", metavar="character")
); 

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

# test if there is at least one argument: if not, return an error
if (length(opt)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
} else if (length(opt)==1) {
  # default output file
  opt[2] = "subject_id"
}

## set working dir to /home for the docker container
setwd("/home")

## load libraries ==============================================================

library(tibble)
library(janitor)
library(readr)
library(readxl)
library(tidyr)
library(reshape2)
library(dplyr)
library(digest)
library(tidyr)
library(ggplot2)

## create output directories ===================================================

outdir_name <- opt$outdir

dir.create(file.path(paste0(outdir_name)))
dir.create(file.path(paste0(outdir_name, "/duplicated_data")))
dir.create(file.path(paste0(outdir_name, "/duplicated_colnames")))
dir.create(file.path(paste0(outdir_name, "/duplicated_rows")))
dir.create(file.path(paste0(outdir_name, "/clean_files")))

## set random seed if needed
set.seed(1)


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

## check input dir not empty ===================================================

## create list of files to read in based on path
fils <- list.files(opt$input, full.names = TRUE, recursive = TRUE)
## check and make sure there are files in the path
if (length(fils) < 1) {
  print(paste0("We did not detect any files in ", opt$path, " please check your path or folder."))
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

summary_problems <- summary_problems %>% tibble::add_row(dataset = "Name of dataset", 
                              subject_id_not_found = "This file has no identifible key/subject_id",
                              subject_id_duplicated = "This appears to be longitudinal data Take care in using ML",
                              date_not_unix = "Time formats are messy Please standarize to Unix or drop",
                              duplicated_data = "You have columns with the same data.  Please drop redundant columns", 
                              duplicated_column_names = "You have the DIFFERENT data in columns with the same name. WHAT? This is sloppy.",
                              data_are_symbols = "You have data that appear not to contain alpha_numeric information. We can't use this downstream")

## read in files  ==============================================================

## loop through files and make checks
for (file in fils) {
  file_name <- janitor::make_clean_names(strsplit(basename(file), split="\\.")[[1]][1])
  print(paste0("Reading in file: ", file_name))
  ## Read in all the files, do not fix colnames
  if (strsplit(basename(file), split="\\.")[[1]][2] == "csv") {
    f <- suppressMessages(readr::read_delim(file, delim = ",", 
                                            name_repair = "minimal", na = na_possibilities))
  } else if (strsplit(basename(file), split="\\.")[[1]][2] %in% c("tsv","txt")){
    f <- suppressMessages(readr::read_delim(file, delim = "\t", 
                                            name_repair = "minimal", na = na_possibilities))
  } else if (strsplit(basename(file), split="\\.")[[1]][2] == "xls") {
    f <- suppressMessages(readxl::read_xls(file, .name_repair = "minimal", na = na_possibilities))
  } else if (strsplit(basename(file), split="\\.")[[1]][2] == "xlsx") {
    f <- suppressMessages(readxl::read_xlsx(file, .name_repair = "minimal", na = na_possibilities))
  }
  
    ## assign file name to variable/column
    f$dataset <- file_name
    
    ## add _x to end of cols to ensure clean_names add unique ids past that 
    ## (it will coerce to unique, and i want to know where the unique id is)
    ## i.e. fiber_x_2...keeps me sure that its a duplicate
    colnames(f) <- paste0(colnames(f), '_x')
    f <- f %>% janitor::clean_names() 
    
## subject id check  ===========================================================
    
    ## check to see if subject_id is in the file
    print(paste0("Checking to see if ", opt$subject_identifier," is in dataset..."))
    if (paste0(opt$subject_identifier, "_x") %in% names(f) == FALSE) { 
      
      print(paste0(opt$subject_identifier, " not found in ", file_name, " dataset. That data will not be included in the analysis.")) 
      
      cat("Press [enter] to acknowledge ")
      
      ## add to a summary problem file
      summary_problems <- summary_problems %>% tibble::add_row(dataset = file_name, 
                                    subject_id_not_found = "needs_check")
                                    
      x <- readLines(file("stdin"),1)
      print(x)
      
      next 
    } else { 
        f <- f %>% rename(., "subject_id_x" = paste0(opt$subject_identifier, "_x"))
        print(paste0("Renaming ", opt$subject_identifier, " to subject_id. No change if you are already using subject_id"))
        }
    
## dup subject id check  =======================================================
    
    ## checking to see if there are duplicate subject_ids in the file - Make unique
    if (NROW(f %>% janitor::get_dupes(., c(subject_id_x))) > 1) {
      print(paste("We detected rows in", file_name, "with the same subject_id."))
      print("We will rename the subjects for now")
      print("Note: if this is longitudinal data, (i.e patient is measured multiple times), take care in choosing the approrpiate ML method and CV strategy. Lme4 and other models may be more useful, but you have the power to decide!")
      
      cat("Press [enter] to acknowledge ")
      
      ## add to a summary problem file
      summary_problems <- summary_problems %>% tibble::add_row(dataset = file_name, 
                                    subject_id_duplicated = "needs_check")
      
      x <- readLines(file("stdin"),1)
      print(x)

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
      
      writeLines("I think you have POSIXct time/date data included. \nThese often mess up downstream analyses. \nYou should try and convert it to a UNIX timestamp.")
      
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
      print(x)
    }
   
## check for non-alphanumeric  =================================================
  
  ## checking to see if values are not alpha_numeric
  g_tmp <- g %>% as.data.frame() %>% dplyr::filter(., !is.na(value))
  g_tmp$alpha_numeric <- grepl("^([A-Z])|([a-z]|[0-9])", g_tmp$value, ignore.case = TRUE)
  g_tmp <- dplyr::filter(g_tmp, alpha_numeric == FALSE)
  if (NROW(g_tmp) > 0) {
    writeLines("You have cells in your data that contain only a symbol. We cant do anything with that.")
    writeLines("We check against a large list of possible NAs. Are you using a really weird NA symbol?")
    cat("Press [enter] to acknowledge ")

      ## add to a summary problem file
      summary_problems <- summary_problems %>% tibble::add_row(dataset = file_name,
                                                       data_are_symbols = "needs_check")

      x <- readLines(file("stdin"),1)
      print(x)
  }
      
    
## count NAs  ==================================================================
    
    ## what features are missing  a ton of data
    na_count_features_tmp <- g %>%
      dplyr::group_by(., dataset, feature, data_feature_hash) %>%
      dplyr::summarise(., na_count = sum(is.na(value)), have_data = sum(!is.na(value))) %>%
      dplyr::arrange(desc(na_count))
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
          tidyr::drop_na()
        
        ## for each feature, round numerics and convert character
        if (is.numeric(tmp$value) == TRUE) { tmp$value  <- round(tmp$value, 2) 
        } else if (is.character(tmp$value) == TRUE) 
        { tmp$value <- sapply(tmp$value, janitor::make_clean_names, USE.NAMES = F) }
        
        
        if (NROW(tmp %>% janitor::get_dupes(., c(subject_id, value))) > (NROW(tmp)*0.2)) {
          ## recording that there might be a duplicated value
          duplicate_check <- duplicate_check %>%
            dplyr::mutate(., duplicated_value = ifelse(feature == f_duplicated, "duplicated_values", duplicated_value))
      
          ## printing to file for you to manually check if data is duplicated
          print(paste(f_duplicated, "is a DUPLICATED column name which contains DUPLICATED values across columns EXAMPLE:"))
          
          print(tmp %>% tidyr::pivot_wider(.,names_from = data_feature_hash, values_from = value) %>% dplyr::arrange(., subject_id) %>% head(n = 5))
          
          tmp %>% tidyr::pivot_wider(.,names_from = data_feature_hash, values_from = value) %>% 
            dplyr::arrange(., subject_id) %>% 
            readr::write_delim(., file = paste0(outdir_name, "/duplicated_data/", file_name, "_", f_duplicated, ".csv"), delim = ",")
          print(paste0("Printing ", f_duplicated, " data to file for manual checks. See outputs/duplicated_data/"))
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
          print(x)
          
        } 
        
## do dup cols have different data?  ===========================================
       
        ## or if the data is truly unique, but had the same col names, start
        ## the process of renaming the features. Also write to duplicated_colnames/
        else  {
          ## showing you why we think it is a duplicated value
          print(paste(f_duplicated, "is a DUPLICATED column name which contains UNIQUE values across columns EXAMPLE:"))
          
          print(tmp %>% tidyr::pivot_wider(.,names_from = data_feature_hash, values_from = value) %>% dplyr::arrange(., subject_id) %>% head(n = 5))
          
          print("Renaming feature to feature + dataset to keep it unique (if it is the same it will get dropped in correlation)")
          
          ## renaming feature in g
          g <- g %>%
            dplyr::mutate(., feature = ifelse(feature == f_duplicated, paste0(feature,"_",data_feature_hash), feature))
          ## writing to file for you to check later
          print("We wrote this to file for you to manually check, see outputs/duplicated_colnames")
          tmp %>% tidyr::pivot_wider(.,names_from = data_feature_hash, values_from = value) %>% 
            dplyr::arrange(., subject_id) %>% 
            readr::write_delim(., file = paste0(outdir_name, "/duplicated_colnames/", file_name, "_", f_duplicated, ".csv"), delim = ",")
          
          cat("Press [enter] to continue ")
          
          ## add to a summary problem file
          summary_problems <- summary_problems %>% tibble::add_row(dataset = file_name, 
                                        duplicated_column_names = "needs_check")
          
          x <- readLines(file("stdin"),1)
          print(x)

        }

      }

    } 
    
## add back in POSIX date data  ================================================
    
    ## Add back in the date data
    tmp <- g %>% tidyr::pivot_wider(.,names_from = feature, values_from = value, id_cols = subject_id) %>%
      readr::type_convert(., na = c("na", "nan", "NA"))
    
    if (exists("date_dataframe")) {
      colnames(date_dataframe) <- gsub(pattern = "_x", "", colnames(date_dataframe))
      tmp <- merge(tmp, date_dataframe, by = "subject_id")
    }
    
    ## clean up date data intermediate files
    rm( list = base::Filter( exists, c("date_features", "date_dataframe") ) )
    
## remove unique subject ID bandaid  ===========================================
    
    ## remove unique identifiers for repeated subject_ids
    tmp$subject_id <- gsub("_[0-9]{1,2}$", "", perl = T, x = tmp$subject_id) 
    readr::write_delim(tmp, file = paste0(outdir_name, "/clean_files/", file_name,".csv"), delim = ",")
    
}

## write figures and files  ====================================================

## print na figure and data to file
na_figure <- na_count_features %>% 
  dplyr::mutate(., dataset = gsub(pattern = "_", replacement = "\n", x = dataset, perl = T)) %>%
  ggplot2::ggplot(aes(x = reorder(feature, na_count), weight = as.numeric(na_count))) + 
  geom_bar() + 
  labs(y = "NA Count", x = "Features with NAs") +
  facet_grid(~ dataset, scales = "free") + 
  theme(axis.text.x=element_text(angle = 90), axis.ticks.x=element_blank())
print("Saving NA counts figure to file, see /outputs/na_counts.pdf")
print("Saving NA counts table to file, see /output/na_counts.csv")
print("####################################################")
print("Saving a summary_problems file, see /output/summary_dataset_problems.csv")
print("####################################################")
print("Explainations of the summary_problems columns are as follows:")
print(as.vector(t(summary_problems[1,])))
readr::write_delim(na_count_features, file = paste0(outdir_name, "/na_counts.csv"), delim = ",")
ggsave(paste0(outdir_name,"/na_counts.pdf"), plot = last_plot(), scale = 1, width = 10, height = 5, units = "in", dpi = 500, limitsize = TRUE, bg = NULL)

## write summary problems dataframe to file 
summary_problems_tmp <- summary_problems[2:NROW(summary_problems), ] %>% dplyr::group_by(dataset) %>% dplyr::summarise(across(everything(), ~n_distinct(., na.rm = T)))
readr::write_delim(summary_problems_tmp, file = paste0(outdir_name, "/summary_dataset_problems.csv"), delim = ",")

