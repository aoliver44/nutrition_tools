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
`%!in%` <- Negate(`%in%`)
is.convertible.to.date <- function(x) !is.na(as.Date(as.character(x), tz = 'UTC', format = c('%m/%d/%Y %H:%M', '%m/%d/%Y')))
is.convertible.to.date2 <- function(x) !is.na(as.Date(as.character(x), tz = 'UTC', format = c('%m/%d/%Y')))

## create list of files to read in
fils <- list.files("data/read_in_Danielle", full.names = TRUE, recursive = TRUE)


all_data <- data.frame(subject_id=numeric(),
                             dataset=character(),
                             feature=character(), 
                             value=character(), 
                             stringsAsFactors=FALSE) 

## create empty dataframe that will house the NA count data eventually
na_count_features <- data.frame(dataset=character(),
                       feature=character(), 
                       data_feature_hash=character(),
                       na_count=numeric(),
                       have_data=numeric(),
                       stringsAsFactors=FALSE) 

for (file in fils) {
  file_name <- janitor::make_clean_names(strsplit(basename(file), split="\\.")[[1]][1])
  print(paste0("Reading in file: ", file_name))
  ## Read in all the files
  if (strsplit(basename(file), split="\\.")[[1]][2] == "csv") {
    ## READ IN csv, do not fix column names
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
  
    ## assign file name to variable
    f$dataset <- file_name
    
    ## add _x to end of cols to ensure clean_names add unique ids past that 
    ## (it will coerce to unique, and i want to know where the unique id is)
    ## i.e. fiber_x_2...keeps me sure that its a duplicate
    colnames(f) <- paste0(colnames(f), '_x')
    f <- f %>% janitor::clean_names() #%>%
      ## get rid of columns that are 99.9999% NAs...kinda useless
      #purrr::discard(~sum(is.na(.x))/length(.x)* 100 >=99.9999)
    
    ## check to see if subject_id is in the file
    print(paste0("Checking to see if 'subject_id' is in dataset..."))
    if ("subject_id_x" %in% names(f) == FALSE) { 
      
      print(paste0("subject_id ", "not found in ", file_name, " dataset. That data will not be included in the analysis.")) 
      
      readline(prompt="Press [enter] to acknowledge ")
      
      next 
      }
    
    ## checking to see if there are duplicate subject_ids in the file - CHOOSE BEST
    if (NROW(f %>% janitor::get_dupes(., c(subject_id_x))) > 1) {
      print(paste("We detected rows in", file_name, "with the same subject_id."))
      print("We will rename the subjects for now")

      readline(prompt="Press [enter] to acknowledge ")

      duplicated_rows <- f %>% janitor::get_dupes(., c(subject_id_x))
      f$subject_id_x <- janitor::make_clean_names(f$subject_id_x)

    }
    
    ## melting to long form
    g <- f %>% reshape2::melt(., id.vars = c("subject_id_x", "dataset_x")) %>%
      dplyr::rename(., "feature" = "variable")
    
    ## you eventually need features to be be duplicates again, not the unique
    ## names that clean_names did (i.e. *_x_2)
    ## so make a new column of dataset_feature_hash which will be unique to 
    ## each feature
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
    g$date <- is.convertible.to.date2(g$value)

    if ((g %>% filter(., date == "TRUE") %>% nrow()) > 0) {
      
      rm( list = base::Filter( exists, c("date_features", "date_dataframe") ) )
      
      writeLines("I think you have POSIXct time/date data included. \nThese often mess up downstream analyses. \nYou should try and convert it to a UNIX timestamp.")
      
      print(g %>% filter(., date == "TRUE") %>% select(., feature) %>% unique() %>% pull(.))
      date_features <- g %>% filter(., date == "TRUE") %>% select(., feature) %>% unique() %>% pull()
      
      date_dataframe <- f %>% select(., subject_id_x, dplyr::contains(date_features))
      
      g <- g %>% 
        filter(., date == "FALSE") %>%
        select(., -date) 
      
      readline(prompt="Press [enter] to acknowledge ")
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

          readline(prompt="Press [enter] to continue ")
          
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
          
          readline(prompt="Press [enter] to continue ")

        }

      }

    } 
    #all_data <- rbind(all_data, g)
    
    tmp <- g %>% pivot_wider(.,names_from = feature, values_from = value, id_cols = subject_id) %>%
      type_convert(., na = c("na", "nan", "NA"))
    
    if (exists("date_dataframe")) {
      colnames(date_dataframe) <- gsub(pattern = "_x", "", colnames(date_dataframe))
      tmp <- merge(tmp, date_dataframe, by.x = "subject_id")
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


## Correlation
# 
# 
# tmp <- g %>% pivot_wider(.,names_from = feature, values_from = value, id_cols = subject_id) %>%
#   type_convert(., na = c("na", "nan", "NA"))
# tmp$subject_id <- gsub("_[0-9]{1,2}$", "", perl = T, x = tmp$subject_id) 
# 
# features_post_process <- preprocess_data(dataset = tmp,
#                                          method = NULL, # commented out to scale continous data for regression
#                                          outcome_colname = "subject_id",
#                                          collapse_corr_feats = F,
#                                          #group_neg_corr = T,
#                                          remove_var = "zv")
# pre_corr_clean <- features_post_process$dat_transformed
# pre_corr_clean <- pre_corr_clean %>% column_to_rownames(., var = "subject_id")
# cor_tmp <- mikropml:::group_correlated_features(pre_corr_clean, corr_thresh = 1.0)
# 
# cor_tmp <- as.data.frame(cor_tmp)
# cor_tmp <- cor_tmp %>% separate(., col = cor_tmp, into = c("keep", "co-correlated"), sep = "\\|", extra = "merge")
# 
# ## filter out only 1 of the co-correlated groups,
# ## results in 146 features (including subject ID)
# corr_clean <- pre_corr_clean %>% select(., cor_tmp$keep)
# 








# all_data_master <- all_data
# 
# ## check and see if the all data file is empty
# if (NROW(all_data) < 1) {stop("No data was imported. Check your path and file extensions.") }
# ## write a check to see if all the files in data were imported
# if ((n_distinct(all_data$dataset)) < (n_distinct(fils))) {
#   stop(paste("You have more files in", path, 
#              "than were imported. Something may have gone wrong. 
#              Please make sure you only have files to import in this directory."))}
# 
# ## make a md5 hash of dataset_feature
# all_data <- all_data %>% 
#   dplyr::mutate(.,  combined_name = paste(all_data$dataset, all_data$feature, sep="_")) %>%
#   dplyr::rowwise() %>%
#   dplyr::mutate(., data_feature_hash = digest::digest(combined_name, algo="md5", serialize = F)) %>%
#   dplyr::select(., -combined_name)
# ## you previously made names unique with numbers at the end. Get rid of them. 
# all_data$feature <- gsub(pattern = "_[0-9]$", replacement = "", x = all_data$feature)
# all_data$feature <- gsub(pattern = "_x$", replacement = "", x = all_data$feature)
# colnames(all_data) <- gsub("_x", "", colnames(all_data))
# 
# ## just delete dates because dates are just ridic
# all_data$date <- is.convertible.to.date(all_data$value)
# all_data$date <- is.convertible.to.date2(all_data$value)
# all_data <- all_data %>% 
#   filter(., date == "FALSE") %>%
#   select(., -date)
# 
# ## Count number of NAs and number of data points per feature, including
# ## duplicated features
# 
# na_count_features <- all_data %>%
#   dplyr::group_by(., dataset, feature,data_feature_hash) %>%
#   dplyr::summarise(., na_count = sum(is.na(value)), have_data = sum(!is.na(value))) %>%
#   dplyr::arrange(desc(na_count)) 
# 
# na_count_features %>% ggplot(aes(x = reorder(feature, na_count), weight = as.numeric(na_count))) + 
#   geom_bar() + facet_grid(~ dataset, scales = "free") + theme(axis.title.x=element_blank(),axis.text.x=element_text(angle = 60), axis.ticks.x=element_blank())
# 
# ## get rid of duplicates by first checking duplicate names and then verifying
# ## duplicate names = duplicated values. If not, rename feature.
# 
# duplicate_check <- na_count_features %>%
#   janitor::get_dupes(., feature) %>% 
#   tibble::add_column(duplicated_value = NA, .after = "have_data")
# 
# all_data1 <- as.data.table(all_data)
# 
# ## SLOW FOR MANY DUPLICATES:
# ## loop through features in duplicates (by name), 
# ## check if they are the same values
# for (f_duplicated in unique(duplicate_check$feature)) {
#   check <- all_data1 %>% filter(., feature == f_duplicated)
#   check <- as.data.frame(check)
#   tmp <- check %>% 
#     #dplyr::distinct(., subject_id,data_feature_hash,value, .keep_all = T) %>%
#     tibble::rowid_to_column(., "index") %>%
#     drop_na() %>%
#     dcast(., subject_id + index ~ data_feature_hash) 
#     #drop_na() %>%
#     #slice_sample(., prop = 0.4) %>%
#     #column_to_rownames(., var = "subject_id") 
#   tmp <- suppressMessages(type_convert(tmp, na = c("", "NA", "na", "NaN", "nan"))) 
#   tmp <- tmp %>% mutate_if(., is.numeric, round, 2) %>%
#     select(., -index) %>%
#     rowwise %>%
#     mutate(same = n_distinct(unlist(cur_data())) == 1) %>%
#     ungroup
#   
#   ## drop duplicated columns
#   if ((length(which(tmp$same == TRUE))) > (nrow(tmp) * 0.95)) {
#     duplicate_check <- duplicate_check %>%
#       mutate(., duplicated_value = ifelse(feature == f_duplicated, "duplicated_values", duplicated_value))
#   }
#   else  {
#     print(paste(f_duplicated, "id a DUPLICATED column name which contains UNIQUE values across datasets. EXAMPLE:"))
#     print(head(subset(tmp, tmp$same == FALSE), n = 2))
#     print("Renaming feature to feature + dataset to keep it unique (if it is the same it will get dropped in correlation)")
#     duplicate_check <- duplicate_check %>%
#       mutate(., duplicated_value = ifelse(feature == f_duplicated, paste0(feature,"_",data_feature_hash), duplicated_value))
#     #readline(prompt="Press [enter] to continue")
#   }
# }
# 
# ## now group by feature, duplicate check, sort by have data, and run distinct.
# best_duplicated <- duplicate_check %>% 
#   dplyr::group_by(., feature, duplicated_value) %>%
#   dplyr::arrange(-have_data) %>%
#   dplyr::distinct(feature, .keep_all = TRUE)
# 
# 
# ## Now drop the less ideal duplicated features from all data
# poor_features <- subset(duplicate_check, duplicate_check$data_feature_hash %!in% best_duplicated$data_feature_hash)
# all_data_de_dup <- all_data %>% dplyr::filter(., data_feature_hash %!in% poor_features$data_feature_hash)
# 
# 
# ## make into a dataframe with colnames as hashes
# all_data_de_dup_df <- reshape2::dcast(all_data_de_dup, formula = subject_id + filename ~ data_feature_hash, value.var = "value")
# ## drop columns that just are missing a ton of data
# all_data_de_dup_df_col_drop <- all_data_de_dup_df[, (colSums(is.na(all_data_de_dup_df)) / NCOL(all_data_de_dup_df)) < .056]
# all_data_de_dup_df_row_drop <- all_data_de_dup_df_col_drop %>% drop_na()
# 
# ## correlate to subject_id
# 
# features_post_process <- preprocess_data(dataset = all_data_de_dup_df_row_drop,
#                                          method = NULL, # commented out to scale continous data for regression
#                                          outcome_colname = "subject_id", 
#                                          collapse_corr_feats = F, 
#                                          #group_neg_corr = T,
#                                          remove_var = "nzv")
# pre_corr_clean <- features_post_process$dat_transformed
# cor_tmp <- mikropml:::group_correlated_features(pre_corr_clean, corr_thresh = 0.9)
# 
# cor_tmp <- as.data.frame(cor_tmp)
# cor_tmp <- cor_tmp %>% separate(., col = cor_tmp, into = c("keep", "co-correlated"), sep = "\\|", extra = "merge")
# 
# ## filter out only 1 of the co-correlated groups, 
# ## results in 146 features (including subject ID)
# corr_clean <- pre_corr_clean %>% select(., cor_tmp$keep)
# 
# 
# #}
