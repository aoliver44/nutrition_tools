#################################################
## SCRIPT: generic_read_in.R
## AUTHOR: Andrew Oliver
## DATE:   May 31, 2022
##
## PURPOSE: To read in data in a flat file
## format, which may have copies of columns/rows
## and columns may be duplicated across different 
## datasets
#################################################

## add commandline options
library(optparse)
option_list = list(
  make_option(c("-i", "--input"), type="character", default="/home/data/", 
              help="path to folder with data to import [default= %default]", metavar="character"),
  make_option(c("-s", "--subject_identifier"), type="character", default="subject_id", 
              help="subject key (column name) found in all files [default= %default]", metavar="character")
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

##########################################################################

## set working dir to /home for the docker container
setwd("/home")

## load libraries as needed!
library(tidyverse)
library(reshape2)
library(data.table)

## create output directories
dir.create(file.path("/home/", "outputs"))
dir.create(file.path("/home/outputs", "duplicated_data"))
dir.create(file.path("/home/outputs", "duplicated_colnames"))
dir.create(file.path("/home/outputs", "duplicated_rows"))
dir.create(file.path("/home/outputs", "clean_files"))
## set random seed if needed
set.seed(1)

#readIN <- function(path, linking_identifier, na_amount = 0.95, duplicated_threshold = 0.96, full_check = FALSE) {

## make some extra helper functions
## Negate function ("not in"):
`%!in%` <- Negate(`%in%`)
## Date function:
is.convertible.to.date <- function(x) !is.na(as.Date(as.character(x), 
                                                     tz = 'UTC', 
                                                     format = c('%m/%d/%Y %H:%M', '%m/%d/%Y')))


## create list of files to read in based on path
fils <- list.files(opt$input, full.names = TRUE, recursive = TRUE)
## check and make sure there are files in the path
if (length(fils) < 1) {
  print(paste0("We did not detect any files in ", opt$path, " please check your path or folder."))
}


## create empty dataframe that will house the NA count data eventually
na_count_features <- data.frame(dataset=character(),
                       feature=character(), 
                       data_feature_hash=character(),
                       na_count=numeric(),
                       have_data=numeric(),
                       stringsAsFactors=FALSE) 

## loop through files and make checks
for (file in fils) {
  file_name <- janitor::make_clean_names(strsplit(basename(file), split="\\.")[[1]][1])
  print(paste0("Reading in file: ", file_name))
  ## Read in all the files, do not fix colnames
  if (strsplit(basename(file), split="\\.")[[1]][2] == "csv") {
    f <- suppressMessages(readr::read_delim(file, delim = ",", 
                                            name_repair = "minimal"))
  } else if (strsplit(basename(file), split="\\.")[[1]][2] %in% c("tsv","txt")){
    f <- suppressMessages(readr::read_delim(file, delim = "\t", 
                                            name_repair = "minimal"))
  } else if (strsplit(basename(file), split="\\.")[[1]][2] == "xls") {
    f <- suppressMessages(readxl::read_xls(file, .name_repair = "minimal"))
  } else if (strsplit(basename(file), split="\\.")[[1]][2] == "xlsx") {
    f <- suppressMessages(readxl::read_xlsx(file, .name_repair = "minimal"))
  }
  
    ## assign file name to variable/column
    f$dataset <- file_name
    
    ## add _x to end of cols to ensure clean_names add unique ids past that 
    ## (it will coerce to unique, and i want to know where the unique id is)
    ## i.e. fiber_x_2...keeps me sure that its a duplicate
    colnames(f) <- paste0(colnames(f), '_x')
    f <- f %>% janitor::clean_names() 
    
    ## check to see if subject_id is in the file
    print(paste0("Checking to see if ", opt$subject_identifier," is in dataset..."))
    if (paste0(opt$subject_identifier, "_x") %in% names(f) == FALSE) { 
      
      print(paste0(opt$subject_identifier, " not found in ", file_name, " dataset. That data will not be included in the analysis.")) 
      
      cat("Press [enter] to acknowledge ")
      x <- readLines(file("stdin"),1)
      print(x)
      
      next 
    } else { 
        f <- f %>% rename(., "subject_id_x" = paste0(opt$subject_identifier, "_x"))
        print(paste0("Renaming ", opt$subject_identifier, " to subject_id. No change if you are already using subject_id"))
        }
    
    ## checking to see if there are duplicate subject_ids in the file - Make unique
    if (NROW(f %>% janitor::get_dupes(., c(subject_id_x))) > 1) {
      print(paste("We detected rows in", file_name, "with the same subject_id."))
      print("We will rename the subjects for now")
      
      cat("Press [enter] to acknowledge ")
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
    
    ## checking for dates and removing them until the end
    ## to do: find a way to convert them to unix time stamps
    g$date <- is.convertible.to.date(g$value)

    if ((g %>% filter(., date == "TRUE") %>% nrow()) > 0) {
      
      rm( list = base::Filter( exists, c("date_features", "date_dataframe") ) )
      
      writeLines("I think you have POSIXct time/date data included. \nThese often mess up downstream analyses. \nYou should try and convert it to a UNIX timestamp.")
      
      print(g %>% filter(., date == "TRUE") %>% select(., feature) %>% unique() %>% pull(.))
      date_features <- g %>% filter(., date == "TRUE") %>% select(., feature) %>% unique() %>% pull()
      
      date_dataframe <- f %>% select(., subject_id_x, dplyr::contains(date_features))
      
      g <- g %>% 
        filter(., date == "FALSE") %>%
        select(., -date) 
      
      cat("Press [enter] to acknowledge ")
      x <- readLines(file("stdin"),1)
      print(x)
    }
    
    
    ## what features are missing  a ton of data
    na_count_features_tmp <- g %>%
      dplyr::group_by(., dataset, feature, data_feature_hash) %>%
      dplyr::summarise(., na_count = sum(is.na(value)), have_data = sum(!is.na(value))) %>%
      dplyr::arrange(desc(na_count))
    na_count_features <- rbind(na_count_features, na_count_features_tmp)
    na_count_features <- na_count_features %>% dplyr::filter(., na_count > 0)
    ## what features are duplicated in the na_count_features
    duplicate_check <- na_count_features_tmp %>%
      janitor::get_dupes(., feature) %>% 
      tibble::add_column(duplicated_value = NA, .after = "have_data")
    
    if (NROW(duplicate_check > 1)) {
      g_dt <- as.data.table(g)
      
      ## loop through features in duplicates (by name), 
      ## check if they are the same values
      for (f_duplicated in unique(duplicate_check$feature)) {
        check <- g_dt %>% filter(., feature == f_duplicated)
        check <- as.data.frame(check)
        tmp <- check %>%
          pivot_longer(-subject_id) %>%
          drop_na %>%
          group_by(subject_id, name) %>%
          mutate(n = 1:n()) %>%
          ungroup %>%
          pivot_wider %>%
          select(-n) %>%
          type_convert(.) %>%
          drop_na()
        
        ## for each feature, round numerics and convert character
        if (is.numeric(tmp$value) == TRUE) { tmp$value  <- round(tmp$value, 2) 
        } else if (is.character(tmp$value) == TRUE) 
        { tmp$value <- sapply(tmp$value, janitor::make_clean_names, USE.NAMES = F) }
        
        
        if (NROW(tmp %>% janitor::get_dupes(., c(subject_id, value))) > (NROW(tmp)*0.2)) {
          ## recording that there might be a duplicated value
          duplicate_check <- duplicate_check %>%
            mutate(., duplicated_value = ifelse(feature == f_duplicated, "duplicated_values", duplicated_value))
      
          ## printing to file for you to manually check if data is duplicated
          print(paste(f_duplicated, "is a DUPLICATED column name which contains DUPLICATED values across columns EXAMPLE:"))
          
          print(tmp %>% pivot_wider(.,names_from = data_feature_hash, values_from = value) %>% arrange(., subject_id) %>% head(n = 5))
          
          tmp %>% pivot_wider(.,names_from = data_feature_hash, values_from = value) %>% 
            arrange(., subject_id) %>% 
            write_delim(., file = paste0("/home/outputs/duplicated_data/", file_name, "_", f_duplicated, ".csv"), delim = ",")
          print(paste0("Printing ", f_duplicated, " data to file for manual checks. See outputs/duplicated_data/"))
          ## keeping a superset of the distinct values and dropping from g
          ## anything not in that superset
          tmp_de_dup <- tmp %>%
            dplyr::arrange(., value) %>% 
            dplyr::distinct(., subject_id, .keep_all = T)
          drop_features <- tmp[tmp$data_feature_hash %!in% tmp_de_dup$data_feature_hash, ]
          g <- g %>% filter(., data_feature_hash %!in% drop_features$data_feature_hash)

          cat("Press [enter] to continue ")
          x <- readLines(file("stdin"),1)
          print(x)
          
        } 
        
        ## or if the data is truly unique, but had the same col names, start
        ## the process of renaming the features. Also write to duplicated_colnames/
        else  {
          ## showing you why we think it is a duplicated value
          print(paste(f_duplicated, "is a DUPLICATED column name which contains UNIQUE values across columns EXAMPLE:"))
          
          print(tmp %>% pivot_wider(.,names_from = data_feature_hash, values_from = value) %>% arrange(., subject_id) %>% head(n = 5))
          
          print("Renaming feature to feature + dataset to keep it unique (if it is the same it will get dropped in correlation)")
          
          ## renaming feature in g
          g <- g %>%
            mutate(., feature = ifelse(feature == f_duplicated, paste0(feature,"_",data_feature_hash), feature))
          ## writing to file for you to check later
          print("We wrote this to file for you to manually check, see outputs/duplicated_colnames")
          tmp %>% pivot_wider(.,names_from = data_feature_hash, values_from = value) %>% 
            arrange(., subject_id) %>% 
            write_delim(., file = paste0("/home/outputs/duplicated_colnames/", file_name, "_", f_duplicated, ".csv"), delim = ",")
          
          cat("Press [enter] to continue ")
          x <- readLines(file("stdin"),1)
          print(x)

        }

      }

    } 
    
    tmp <- g %>% pivot_wider(.,names_from = feature, values_from = value, id_cols = subject_id) %>%
      type_convert(., na = c("na", "nan", "NA"))
    
    if (exists("date_dataframe")) {
      colnames(date_dataframe) <- gsub(pattern = "_x", "", colnames(date_dataframe))
      tmp <- merge(tmp, date_dataframe, by = "subject_id")
    }
    
    rm( list = base::Filter( exists, c("date_features", "date_dataframe") ) )
    
    tmp$subject_id <- gsub("_[0-9]{1,2}$", "", perl = T, x = tmp$subject_id) 
    write_delim(tmp, file = paste0("/home/outputs/clean_files/",file_name,".csv"), delim = ",")
    
}

## print na figure to file
na_figure <- na_count_features %>% 
  mutate(., dataset = gsub(pattern = "_", replacement = "\n", x = dataset, perl = T)) %>%
  ggplot(aes(x = reorder(feature, na_count), weight = as.numeric(na_count))) + 
  geom_bar() + 
  labs(y = "NA Count", x = "Features with NAs") +
  facet_grid(~ dataset, scales = "free") + 
  theme(axis.text.x=element_text(angle = 90), axis.ticks.x=element_blank())
print("Saving NA counts figure to file, see /outputs/na_counts.pdf")
print("Saving NA counts table to file, see /output/na_counts.csv")
write_delim(na_count_features, file = "/home/outputs/na_counts.csv", delim = ",")
ggsave("/home/outputs/na_counts.pdf", plot = last_plot(), scale = 1, width = 10, height = 5, units = "in", dpi = 500, limitsize = TRUE, bg = NULL)
