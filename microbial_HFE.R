#!/usr/bin/env Rscript 

## SCRIPT: microbial_HFE.R ===============================================
## AUTHOR: Andrew Oliver
## DATE:   Sept 16, 2022
##
## PURPOSE: To do some gentle feature engineering. This concept
## is too complicated to do much more without knowing more about the
## dataset.

## docker info =================================================================

## docker command:
#docker run --rm -it -p 8787:8787 -e PASSWORD=yourpasswordhere -v /Users/andrew.oliver/Documents/active_projects_github-USDA/nutrition_tools/:/home amr_r_env:3.1.0

## general command:
## ./microbial_HFE.R --subject_identifier subject_id --label cluster --var_control TRUE --feature_type factor /home/data/read_in_tests/abx_cluster_andrew.csv /home/data/synthetic_test_data/merged_metaphlan4.txt /home/output/metaphlan_hfe.txt

## set working dir to /home for the docker container
setwd("/home")

## add commandline options =====================================================

library(docopt)
'Microbial hierarchical feature engineering (HFE) of metaphfor classification
Usage:
    microbial_HFE.R [--subject_identifier=<subject_colname> --label=<label> --feature_type=<feature_type> --var_control=<var>] <input_metadata> <input_metaphlan> <output_file>
    
Options:
    -h --help  Show this screen.
    -v --version  Show version.
    --subject_identifier=<subject_colname> name of columns with subject IDs [default: subject_id]
    --label response feature of interest for classification [default: cluster]
    --feature_type of response i.e. numeric or factor [default: factor]
    --var_control filter features that contain less than this threshold of percentage of unique features [default: 5]
    
Arguments:
    input_meta path to metadata input (CSV)
    input  path to input file from hierarchical data (i.e. metaphlan data) (TSV)
    output_file  output file name

' -> doc

opt <- docopt::docopt(doc, version = 'microbial_HFE.R v1.0\n\n')
print(opt)
## load libraries ==============================================================

library(dplyr)
library(janitor)
library(tidyr)
library(tibble)
library(caret)
library(readr)

## set random seed if needed
set.seed(1)

## helper functions ============================================================

## Negate function ("not in"):
`%!in%` <- Negate(`%in%`)

## arg tests ===================================================================

# tests
#opt <- readr::read_delim(file = "/home/data/synthetic_test_data/opt_data.csv", delim = ",")

## check for inputs ============================================================

## check and see if clean_files directory exists
cat("Checking for for input_meta...")

if (file.exists(opt$input_metadata)) {
  cat(paste0("Using ", opt$input_meta, "as input")) 
} else { stop("Metadata input not found.") }

cat("Checking for for input")

if (file.exists(opt$input_metaphlan)) {
  cat(paste0("Using ", opt$input_metaphlan, "as input")) 
} else { stop("Input not found.") }


metaphlan <- readr::read_delim(file = opt$input_metaphlan, delim = "\t", skip = 1) %>% dplyr::select(., -any_of(c("NCBI_tax_id")))
original_taxa_count <- NROW(metaphlan)
colnames(metaphlan) <- gsub(pattern = ".metaphlan", "", x = colnames(metaphlan))
metaphlan$clade_name <- gsub(pattern = "\\|t__", replacement = "_", x = metaphlan$clade_name)

metadata <- readr::read_delim(file = opt$input_metadata, delim = ",")
metadata <- metadata %>% dplyr::select(., opt$subject_identifier, opt$label)
metadata <- metadata %>% dplyr::rename(., "subject_id" = opt$subject_identifier) %>%
  rename(., "feature_of_interest" = opt$label) 

## Remove low variance features ================================================

random_subsample <- sample(colnames(metaphlan[2:NCOL(metaphlan)]), (NCOL(metaphlan[,2:NCOL(metaphlan)]) * 0.75))
metaphlan_var <- metaphlan %>%
  tibble::column_to_rownames(., var = "clade_name") %>%
  dplyr::select(., all_of(random_subsample)) %>%
  t() %>% 
  as.data.frame()

nzv <- caret::nearZeroVar(metaphlan_var,saveMetrics= TRUE)
non_nzv_taxa <- nzv %>%
  tibble::rownames_to_column(., var = "taxa") %>%
  dplyr::filter(., percentUnique > as.numeric(opt$var_control)) %>%
  dplyr::pull(., "taxa")

metaphlan <- metaphlan %>%
  dplyr::filter(., clade_name %in% non_nzv_taxa)

taxa_only_split <- metaphlan %>% 
  tidyr::separate(., col = clade_name, into = c("kingdom", "phylum", "class", "order", "family", "genus", "species"), sep = "\\|") %>%
  dplyr::select(., 1:7)

taxa_only_split$metaphlan_taxonomy <- metaphlan$clade_name
taxa_only_split$taxa_abundance <- rowSums(metaphlan[,2:NCOL(metaphlan)])
taxa_only_split$na_count <- rowSums(is.na(taxa_only_split))
taxa_only_split_master <- taxa_only_split
#taxa_only_split_history <- taxa_only_split %>% tibble::add_column(., history = NA)

metaphlan <- metaphlan %>%
  dplyr::filter(., clade_name %in% taxa_only_split$metaphlan_taxonomy)

## Genus =======================================================================

genera <- taxa_only_split %>%
  group_by(., genus) %>%
  summarize(., count = n_distinct(species)) %>% tidyr::drop_na()

count = 1

for (parent_trial in genera[genera$count > 1, ]$genus) {

  #print(paste0("Analyzing if species are more important than genus: ",parent_trial))
  metaphlan_parent <- metaphlan %>%
    dplyr::filter(., grepl(pattern = parent_trial, clade_name)) %>%
    tidyr::separate(., col = clade_name, into = c("kingdom", "phylum", "class", "order", "family", "genus", "species"), sep = "\\|") %>%
    dplyr::select(., -kingdom, -phylum, -class, -order, -family) %>%
    dplyr::mutate(species = ifelse(is.na(species), "PARENT", species)) %>%
    dplyr::select(., -genus) %>% 
    dplyr::group_by(., species) %>%
    dplyr::summarise(., across(where(is.numeric), ~ sum(.))) %>% 
    tibble::column_to_rownames(., "species") %>%
    t() %>%
    as.data.frame()
  
  if ("PARENT" %!in% colnames(metaphlan_parent)) { 
    #print("PARENT not found, summing species to produce PARENT")
    metaphlan_parent$PARENT <- rowSums(metaphlan_parent)
  }
  
  metaphlan_parent_merge <- merge(metadata, metaphlan_parent, by.x = "subject_id", by.y = "row.names")
  
  ### CORRELATION #####
  cor_drop <- suppressMessages(corrr::correlate(metaphlan_parent)) %>% corrr::focus(., PARENT) %>% dplyr::filter(., PARENT > 0.85) %>% pull(., term)

  metaphlan_parent_merge_cor_subset <- metaphlan_parent_merge %>% 
    dplyr::select(., -all_of(cor_drop)) %>%
    select(., -subject_id)
  
  if (NCOL(metaphlan_parent_merge_cor_subset) < 3) {
    #print("Parent wins due to correlation")

    taxa_only_split <- taxa_only_split %>% dplyr::mutate(., species = ifelse((genus == parent_trial & !is.na(genus) & na_count == 0), "drop_dis", species))
    taxa_only_split <- taxa_only_split %>% dplyr::filter(., !grepl(pattern = "drop_dis", x = species))
    
    #taxa_only_split_history <- taxa_only_split_history %>% dplyr::mutate(., history = ifelse((genus == parent_trial & !is.na(genus) & na_count == 0), "This Parent [GENUS] wins due to correlation", history))

  } else {
  
    ## RF MODEL #####
    
    if (opt$feature_type == "factor") {
      model <- ranger::ranger(as.factor(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = seed, mtry = round(sqrt(NCOL(microbial_hfe)) * 1.25))
      model_importance <- as.data.frame(model$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
      nperm <- 10
      for (seed in sample(1:1000, nperm)) {
        model_tmp <- ranger::ranger(as.factor(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = seed, mtry = round(sqrt(NCOL(microbial_hfe)) * 1.25))
        model_importance_tmp <- as.data.frame(model_tmp$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
        model_importance <- merge(model_importance, model_importance_tmp, by = "taxa")
        
      }
      colnames(model_importance)[2:(nperm + 2)] <- paste0("permutation_", seq(1,nperm + 1))
      model_importance$average <- rowMeans(model_importance[, 2:(nperm + 2)])
      
    } else {
      model <- ranger::ranger(as.numeric(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = seed, mtry = round(sqrt(NCOL(microbial_hfe)) * 1.25))
      model_importance <- as.data.frame(model$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
      nperm <- 10
      for (seed in sample(1:1000, nperm)) {
        model_tmp <- ranger::ranger(as.numeric(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = seed, mtry = round(sqrt(NCOL(microbial_hfe)) * 1.25))
        model_importance_tmp <- as.data.frame(model_tmp$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
        model_importance <- merge(model_importance, model_importance_tmp, by = "taxa")
        
      }
      colnames(model_importance)[2:(nperm + 2)] <- paste0("permutation_", seq(1,nperm + 1))
      model_importance$average <- rowMeans(model_importance[, 2:(nperm + 2)])
      
    }
    
    ### RF WINNER - PARENT ####
    if (sapply(as.data.frame(model$variable.importance), function(x) head(row.names(as.data.frame(model$variable.importance))[order(x, decreasing = TRUE)], 1)) == "PARENT") {
      #print("Parent is most important feature in model")

      taxa_only_split <- taxa_only_split %>% dplyr::mutate(., species = ifelse((genus == parent_trial & !is.na(genus) & na_count == 0), "drop_dis", species))
      taxa_only_split <- taxa_only_split %>% dplyr::filter(., !grepl(pattern = "drop_dis", x = species))
      
      #taxa_only_split_history <- taxa_only_split_history %>% dplyr::mutate(., history = ifelse((genus == parent_trial & !is.na(genus) & na_count == 0), "This Parent [GENUS] wins in model", history))
      
    } else { 
      ### RF WINNER - CHILD ####
      
      model_importance <- as.data.frame(model$variable.importance) %>% 
        tibble::rownames_to_column(., var = "taxa") 
      
      parent_importance <- model_importance$`model$variable.importance`[model_importance$taxa == "PARENT"]
      
      children_toss <- model_importance %>% dplyr::filter(., `model$variable.importance` < parent_importance) %>% pull(., taxa)
      children_toss <- c(children_toss, cor_drop)
      
      ## drop parent
      taxa_only_split <- taxa_only_split %>% 
        dplyr::mutate(., species = ifelse((genus == parent_trial & !is.na(genus) & is.na(species)), "drop_dis", species))
      taxa_only_split <- taxa_only_split %>% 
        mutate_at(c("kingdom", "phylum", "class", "order", "family", "genus"), ~ 
                    replace(., genus == parent_trial, NA)) %>%
        dplyr::filter(., !grepl(pattern = "drop_dis", x = species))
      
      ## drop children that didnt win against parent
      taxa_only_split <- taxa_only_split %>% 
        dplyr::mutate(., species = ifelse((species %in% children_toss & is.na(genus) & !is.na(species)), "drop_dis", species))
      taxa_only_split <- taxa_only_split %>% 
        dplyr::filter(., !grepl(pattern = "drop_dis", x = species))
      
      #taxa_only_split_history <- taxa_only_split_history %>% dplyr::mutate(., history = ifelse((genus == parent_trial & !is.na(genus) & is.na(species)), "Child [SPECIES] won in model", history))
      #taxa_only_split_history <- taxa_only_split_history %>% dplyr::mutate(., history = ifelse((species %in% children_toss & is.na(genus) & !is.na(species)), "Children [SPECIES] won, but this Child was dropped bc it didnt beat parent in model", history))
      
     }
  }
    ### PROGRESS ####
    svMisc::progress(count, length(genera[genera$count > 1, ]$genus))
    Sys.sleep(0.01)
    if (count == length(genera[genera$count > 1, ]$genus)) message("Done with Genus!")
    count = count + 1
}

metaphlan <- metaphlan %>%
  dplyr::filter(., clade_name %in% taxa_only_split$metaphlan_taxonomy)

## Family ======================================================================

families <- taxa_only_split %>%
  group_by(., family) %>%
  summarize(., count = n_distinct(genus)) %>% tidyr::drop_na()

count = 1

for (parent_trial in families[families$count > 1, ]$family) {
  
  #print(paste0("Analyzing if genus are more important than family: ",parent_trial))
  metaphlan_parent <- metaphlan %>%
    dplyr::filter(., grepl(pattern = parent_trial, clade_name)) %>%
    tidyr::separate(., col = clade_name, into = c("kingdom", "phylum", "class", "order", "family", "genus", "species"), sep = "\\|") %>%
    dplyr::select(., -kingdom, -phylum, -class, -order, -species) %>%
    dplyr::mutate(genus = ifelse(is.na(genus), "PARENT", genus)) %>%
    dplyr::select(., -family) %>% 
    dplyr::group_by(., genus) %>%
    dplyr::summarise(., across(where(is.numeric), ~ sum(.))) %>% 
    tibble::column_to_rownames(., "genus") %>%
    t() %>%
    as.data.frame()
  
  if ("PARENT" %!in% colnames(metaphlan_parent)) { 
    #print("PARENT not found, summing genus to produce PARENT")
    metaphlan_parent$PARENT <- rowSums(metaphlan_parent)
  }
  
  metaphlan_parent_merge <- merge(metadata, metaphlan_parent, by.x = "subject_id", by.y = "row.names")
  
  ### CORRELATION #####
  cor_drop <- suppressMessages(corrr::correlate(metaphlan_parent)) %>% corrr::focus(., PARENT) %>% dplyr::filter(., PARENT > 0.85) %>% pull(., term)
  
  metaphlan_parent_merge_cor_subset <- metaphlan_parent_merge %>% 
    dplyr::select(., -all_of(cor_drop)) %>%
    select(., -subject_id)
  
  if (NCOL(metaphlan_parent_merge_cor_subset) < 3) {
    #print("Parent wins due to correlation")
    
    taxa_only_split <- taxa_only_split %>% dplyr::mutate(., genus = ifelse((family == parent_trial & !is.na(family) & na_count == 1), "drop_dis", genus))
    taxa_only_split <- taxa_only_split %>% dplyr::filter(., !grepl(pattern = "drop_dis", x = genus))
    
    #taxa_only_split_history <- taxa_only_split_history %>% dplyr::mutate(., history = ifelse((family == parent_trial & !is.na(family) & na_count == 1), "This Parent [FAMILY] wins due to correlation", history))
    
    
  } else {
    
    ## RF MODEL #####
    
    if (opt$feature_type == "factor") {
      model <- ranger::ranger(as.factor(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = seed, mtry = round(sqrt(NCOL(microbial_hfe)) * 1.25))
      model_importance <- as.data.frame(model$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
      nperm <- 10
      for (seed in sample(1:1000, nperm)) {
        model_tmp <- ranger::ranger(as.factor(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = seed, mtry = round(sqrt(NCOL(microbial_hfe)) * 1.25))
        model_importance_tmp <- as.data.frame(model_tmp$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
        model_importance <- merge(model_importance, model_importance_tmp, by = "taxa")
        
      }
      colnames(model_importance)[2:(nperm + 2)] <- paste0("permutation_", seq(1,nperm + 1))
      model_importance$average <- rowMeans(model_importance[, 2:(nperm + 2)])
      
    } else {
      model <- ranger::ranger(as.numeric(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = seed, mtry = round(sqrt(NCOL(microbial_hfe)) * 1.25))
      model_importance <- as.data.frame(model$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
      nperm <- 10
      for (seed in sample(1:1000, nperm)) {
        model_tmp <- ranger::ranger(as.numeric(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = seed, mtry = round(sqrt(NCOL(microbial_hfe)) * 1.25))
        model_importance_tmp <- as.data.frame(model_tmp$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
        model_importance <- merge(model_importance, model_importance_tmp, by = "taxa")
        
      }
      colnames(model_importance)[2:(nperm + 2)] <- paste0("permutation_", seq(1,nperm + 1))
      model_importance$average <- rowMeans(model_importance[, 2:(nperm + 2)])
      
    }
    ### RF WINNER - PARENT ####
    if (sapply(as.data.frame(model$variable.importance), function(x) head(row.names(as.data.frame(model$variable.importance))[order(x, decreasing = TRUE)], 1)) == "PARENT") {
      #print("Parent is most important feature in model")
      
      taxa_only_split <- taxa_only_split %>% dplyr::mutate(., genus = ifelse((family == parent_trial & !is.na(family) & na_count == 1), "drop_dis", genus))
      taxa_only_split <- taxa_only_split %>% dplyr::filter(., !grepl(pattern = "drop_dis", x = genus))
      
      #taxa_only_split_history <- taxa_only_split_history %>% dplyr::mutate(., history = ifelse((family == parent_trial & !is.na(family) & na_count == 1), "This Parent [FAMILY] wins in model", history))
      

    } else { 
      ### RF WINNER - CHILD ####
      
      model_importance <- as.data.frame(model$variable.importance) %>% 
        tibble::rownames_to_column(., var = "taxa") 
      
      parent_importance <- model_importance$`model$variable.importance`[model_importance$taxa == "PARENT"]
      
      children_toss <- model_importance %>% dplyr::filter(., `model$variable.importance` < parent_importance) %>% pull(., taxa)
      children_toss <- c(children_toss, cor_drop)
      
      ## drop parent
      taxa_only_split <- taxa_only_split %>% 
        dplyr::mutate(., genus = ifelse((family == parent_trial & !is.na(family) & is.na(genus)), "drop_dis", genus))
      taxa_only_split <- taxa_only_split %>% 
        mutate_at(c("kingdom", "phylum", "class", "order", "family", "species"), ~ 
                    replace(., family == parent_trial, NA)) %>%
        dplyr::filter(., !grepl(pattern = "drop_dis", x = genus))
      
      ## drop children that didnt win against parent
      taxa_only_split <- taxa_only_split %>% 
        dplyr::mutate(., genus = ifelse((genus %in% children_toss & is.na(family) & !is.na(genus)), "drop_dis", genus))
      taxa_only_split <- taxa_only_split %>% 
        dplyr::filter(., !grepl(pattern = "drop_dis", x = genus))
      
      #taxa_only_split_history <- taxa_only_split_history %>% dplyr::mutate(., history = ifelse((family == parent_trial & !is.na(family) & is.na(genus)), "This Child [GENUS] won in model", history))
      #taxa_only_split_history <- taxa_only_split_history %>% dplyr::mutate(., history = ifelse((genus %in% children_toss & is.na(family) & !is.na(genus)), "Children [GENUS] won, but this Child was dropped bc it didnt beat parent in model", history))
      
    }
  }
  ### PROGRESS ####
  svMisc::progress(count, length(families[families$count > 1, ]$family))
  Sys.sleep(0.01)
  if (count == length(families[families$count > 1, ]$family)) message("Done with Family!")
  count = count + 1
}

metaphlan <- metaphlan %>%
  dplyr::filter(., clade_name %in% taxa_only_split$metaphlan_taxonomy)


## Order ======================================================================

orders <- taxa_only_split %>%
  group_by(., order) %>%
  summarize(., count = n_distinct(family)) %>% tidyr::drop_na()

count = 1

for (parent_trial in orders[orders$count > 1, ]$order) {
  
  #print(paste0("Analyzing if family are more important than order: ",parent_trial))
  metaphlan_parent <- metaphlan %>%
    dplyr::filter(., grepl(pattern = parent_trial, clade_name)) %>%
    tidyr::separate(., col = clade_name, into = c("kingdom", "phylum", "class", "order", "family", "genus", "species"), sep = "\\|") %>%
    dplyr::select(., -kingdom, -phylum, -class, -genus, -species) %>%
    dplyr::mutate(family = ifelse(is.na(family), "PARENT", family)) %>%
    dplyr::select(., -order) %>% 
    dplyr::group_by(., family) %>%
    dplyr::summarise(., across(where(is.numeric), ~ sum(.))) %>% 
    tibble::column_to_rownames(., "family") %>%
    t() %>%
    as.data.frame()
  
  if ("PARENT" %!in% colnames(metaphlan_parent)) { 
    #print("PARENT not found, summing genus to produce PARENT")
    metaphlan_parent$PARENT <- rowSums(metaphlan_parent)
  }
  
  metaphlan_parent_merge <- merge(metadata, metaphlan_parent, by.x = "subject_id", by.y = "row.names")
  
  ### CORRELATION #####
  cor_drop <- suppressMessages(corrr::correlate(metaphlan_parent)) %>% corrr::focus(., PARENT) %>% dplyr::filter(., PARENT > 0.85) %>% pull(., term)
  
  metaphlan_parent_merge_cor_subset <- metaphlan_parent_merge %>% 
    dplyr::select(., -all_of(cor_drop)) %>%
    select(., -subject_id)
  
  if (NCOL(metaphlan_parent_merge_cor_subset) < 3) {
    #print("Parent wins due to correlation")
    
    taxa_only_split <- taxa_only_split %>% dplyr::mutate(., family = ifelse((order == parent_trial & !is.na(order) & na_count == 2), "drop_dis", family))
    taxa_only_split <- taxa_only_split %>% dplyr::filter(., !grepl(pattern = "drop_dis", x = family))
    
    #taxa_only_split_history <- taxa_only_split_history %>% dplyr::mutate(., history = ifelse((order == parent_trial & !is.na(order) & na_count == 2), "This Parent [ORDER] wins due to correlation", history))
    
    
  } else {
    
    ## RF MODEL #####
    
    if (opt$feature_type == "factor") {
      model <- ranger::ranger(as.factor(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = seed, mtry = round(sqrt(NCOL(microbial_hfe)) * 1.25))
      model_importance <- as.data.frame(model$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
      nperm <- 10
      for (seed in sample(1:1000, nperm)) {
        model_tmp <- ranger::ranger(as.factor(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = seed, mtry = round(sqrt(NCOL(microbial_hfe)) * 1.25))
        model_importance_tmp <- as.data.frame(model_tmp$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
        model_importance <- merge(model_importance, model_importance_tmp, by = "taxa")
        
      }
      colnames(model_importance)[2:(nperm + 2)] <- paste0("permutation_", seq(1,nperm + 1))
      model_importance$average <- rowMeans(model_importance[, 2:(nperm + 2)])
      
    } else {
      model <- ranger::ranger(as.numeric(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = seed, mtry = round(sqrt(NCOL(microbial_hfe)) * 1.25))
      model_importance <- as.data.frame(model$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
      nperm <- 10
      for (seed in sample(1:1000, nperm)) {
        model_tmp <- ranger::ranger(as.numeric(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = seed, mtry = round(sqrt(NCOL(microbial_hfe)) * 1.25))
        model_importance_tmp <- as.data.frame(model_tmp$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
        model_importance <- merge(model_importance, model_importance_tmp, by = "taxa")
        
      }
      colnames(model_importance)[2:(nperm + 2)] <- paste0("permutation_", seq(1,nperm + 1))
      model_importance$average <- rowMeans(model_importance[, 2:(nperm + 2)])
      
    }
    ### RF WINNER - PARENT ####
    if (sapply(as.data.frame(model$variable.importance), function(x) head(row.names(as.data.frame(model$variable.importance))[order(x, decreasing = TRUE)], 1)) == "PARENT") {
      #print("Parent is most important feature in model")
      
      taxa_only_split <- taxa_only_split %>% dplyr::mutate(., family = ifelse((order == parent_trial & !is.na(order) & na_count == 2), "drop_dis", family))
      taxa_only_split <- taxa_only_split %>% dplyr::filter(., !grepl(pattern = "drop_dis", x = family))
      
      #taxa_only_split_history <- taxa_only_split_history %>% dplyr::mutate(., history = ifelse((order == parent_trial & !is.na(order) & na_count == 2), "This Parent [ORDER] wins in model", history))
      

    } else { 
      ### RF WINNER - CHILD ####
      
      model_importance <- as.data.frame(model$variable.importance) %>% 
        tibble::rownames_to_column(., var = "taxa") 
      
      parent_importance <- model_importance$`model$variable.importance`[model_importance$taxa == "PARENT"]
      
      children_toss <- model_importance %>% dplyr::filter(., `model$variable.importance` < parent_importance) %>% pull(., taxa)
      children_toss <- c(children_toss, cor_drop)
      
      ## drop parent
      taxa_only_split <- taxa_only_split %>% 
        dplyr::mutate(., family = ifelse((order == parent_trial & !is.na(order) & is.na(family)), "drop_dis", family))
      taxa_only_split <- taxa_only_split %>% 
        mutate_at(c("kingdom", "phylum", "class", "order", "genus", "species"), ~ 
                    replace(., order == parent_trial, NA)) %>%
        dplyr::filter(., !grepl(pattern = "drop_dis", x = family))
      
      ## drop children that didnt win against parent
      taxa_only_split <- taxa_only_split %>% 
        dplyr::mutate(., family = ifelse((family %in% children_toss & is.na(order) & !is.na(family)), "drop_dis", family))
      taxa_only_split <- taxa_only_split %>% 
        dplyr::filter(., !grepl(pattern = "drop_dis", x = family))
     
      #taxa_only_split_history <- taxa_only_split_history %>% dplyr::mutate(., history = ifelse((order == parent_trial & !is.na(order) & is.na(family)), "This Child [FAMILY] won in model", history))
      #taxa_only_split_history <- taxa_only_split_history %>% dplyr::mutate(., history = ifelse((family %in% children_toss & is.na(order) & !is.na(family)), "Children [FAMILY] won, but this Child was dropped bc it didnt beat parent in model", history))
      
    }
  }
  ### PROGRESS ####
  svMisc::progress(count, length(orders[orders$count > 1, ]$order))
  Sys.sleep(0.01)
  if (count == length(orders[orders$count > 1, ]$order)) message("Done with Order!")
  count = count + 1
}

metaphlan <- metaphlan %>%
  dplyr::filter(., clade_name %in% taxa_only_split$metaphlan_taxonomy)

## Class ======================================================================

classes <- taxa_only_split %>%
  group_by(., class) %>%
  summarize(., count = n_distinct(order)) %>% tidyr::drop_na()

count = 1

for (parent_trial in classes[classes$count > 1, ]$class) {
  
  #print(paste0("Analyzing if order are more important than class: ",parent_trial))
  metaphlan_parent <- metaphlan %>%
    dplyr::filter(., grepl(pattern = parent_trial, clade_name)) %>%
    tidyr::separate(., col = clade_name, into = c("kingdom", "phylum", "class", "order", "family", "genus", "species"), sep = "\\|") %>%
    dplyr::select(., -kingdom, -phylum, -family, -genus, -species) %>%
    dplyr::mutate(order = ifelse(is.na(order), "PARENT", order)) %>%
    dplyr::select(., -class) %>% 
    dplyr::group_by(., order) %>%
    dplyr::summarise(., across(where(is.numeric), ~ sum(.))) %>% 
    tibble::column_to_rownames(., "order") %>%
    t() %>%
    as.data.frame()
  
  if ("PARENT" %!in% colnames(metaphlan_parent)) { 
    #print("PARENT not found, summing genus to produce PARENT")
    metaphlan_parent$PARENT <- rowSums(metaphlan_parent)
  }
  
  metaphlan_parent_merge <- merge(metadata, metaphlan_parent, by.x = "subject_id", by.y = "row.names")
  
  ### CORRELATION #####
  cor_drop <- suppressMessages(corrr::correlate(metaphlan_parent)) %>% corrr::focus(., PARENT) %>% dplyr::filter(., PARENT > 0.85) %>% pull(., term)
  
  metaphlan_parent_merge_cor_subset <- metaphlan_parent_merge %>% 
    dplyr::select(., -all_of(cor_drop)) %>%
    select(., -subject_id)
  
  if (NCOL(metaphlan_parent_merge_cor_subset) < 3) {
    #print("Parent wins due to correlation")
    
    taxa_only_split <- taxa_only_split %>% dplyr::mutate(., order = ifelse((class == parent_trial & !is.na(class) & na_count == 3), "drop_dis", order))
    taxa_only_split <- taxa_only_split %>% dplyr::filter(., !grepl(pattern = "drop_dis", x = order))
    

    
  } else {
    
    ## RF MODEL #####
    
    if (opt$feature_type == "factor") {
      model <- ranger::ranger(as.factor(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = seed, mtry = round(sqrt(NCOL(microbial_hfe)) * 1.25))
      model_importance <- as.data.frame(model$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
      nperm <- 10
      for (seed in sample(1:1000, nperm)) {
        model_tmp <- ranger::ranger(as.factor(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = seed, mtry = round(sqrt(NCOL(microbial_hfe)) * 1.25))
        model_importance_tmp <- as.data.frame(model_tmp$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
        model_importance <- merge(model_importance, model_importance_tmp, by = "taxa")
        
      }
      colnames(model_importance)[2:(nperm + 2)] <- paste0("permutation_", seq(1,nperm + 1))
      model_importance$average <- rowMeans(model_importance[, 2:(nperm + 2)])
      
    } else {
      model <- ranger::ranger(as.numeric(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = seed, mtry = round(sqrt(NCOL(microbial_hfe)) * 1.25))
      model_importance <- as.data.frame(model$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
      nperm <- 10
      for (seed in sample(1:1000, nperm)) {
        model_tmp <- ranger::ranger(as.numeric(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = seed, mtry = round(sqrt(NCOL(microbial_hfe)) * 1.25))
        model_importance_tmp <- as.data.frame(model_tmp$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
        model_importance <- merge(model_importance, model_importance_tmp, by = "taxa")
        
      }
      colnames(model_importance)[2:(nperm + 2)] <- paste0("permutation_", seq(1,nperm + 1))
      model_importance$average <- rowMeans(model_importance[, 2:(nperm + 2)])
      
    }
    ### RF WINNER - PARENT ####
    if (sapply(as.data.frame(model$variable.importance), function(x) head(row.names(as.data.frame(model$variable.importance))[order(x, decreasing = TRUE)], 1)) == "PARENT") {
      #print("Parent is most important feature in model")
      
      taxa_only_split <- taxa_only_split %>% dplyr::mutate(., order = ifelse((class == parent_trial & !is.na(class) & na_count == 3), "drop_dis", order))
      taxa_only_split <- taxa_only_split %>% dplyr::filter(., !grepl(pattern = "drop_dis", x = order))
      

    } else { 
      ### RF WINNER - CHILD ####
      
      model_importance <- as.data.frame(model$variable.importance) %>% 
        tibble::rownames_to_column(., var = "taxa") 
      
      parent_importance <- model_importance$`model$variable.importance`[model_importance$taxa == "PARENT"]
      
      children_toss <- model_importance %>% dplyr::filter(., `model$variable.importance` < parent_importance) %>% pull(., taxa)
      children_toss <- c(children_toss, cor_drop)
      
      ## drop parent
      taxa_only_split <- taxa_only_split %>% 
        dplyr::mutate(., order = ifelse((class == parent_trial & !is.na(class) & is.na(order)), "drop_dis", order))
      taxa_only_split <- taxa_only_split %>% 
        mutate_at(c("kingdom", "phylum", "class", "family", "genus", "species"), ~ 
                    replace(., class == parent_trial, NA)) %>%
        dplyr::filter(., !grepl(pattern = "drop_dis", x = order))
      
      ## drop children that didnt win against parent
      taxa_only_split <- taxa_only_split %>% 
        dplyr::mutate(., order = ifelse((order %in% children_toss & is.na(class) & !is.na(order)), "drop_dis", order))
      taxa_only_split <- taxa_only_split %>% 
        dplyr::filter(., !grepl(pattern = "drop_dis", x = order))
      
    }
  }
  ### PROGRESS ####
  svMisc::progress(count, length(classes[classes$count > 1, ]$class))
  Sys.sleep(0.01)
  if (count == length(classes[classes$count > 1, ]$class)) message("Done with Class!")
  count = count + 1
}

metaphlan <- metaphlan %>%
  dplyr::filter(., clade_name %in% taxa_only_split$metaphlan_taxonomy)



## Phylum ======================================================================

phyla <- taxa_only_split %>%
  group_by(., phylum) %>%
  summarize(., count = n_distinct(class)) %>% tidyr::drop_na()

count = 1

for (parent_trial in phyla[phyla$count > 1, ]$phylum) {
  
  #print(paste0("Analyzing if class are more important than phylum: ",parent_trial))
  metaphlan_parent <- metaphlan %>%
    dplyr::filter(., grepl(pattern = parent_trial, clade_name)) %>%
    tidyr::separate(., col = clade_name, into = c("kingdom", "phylum", "class", "order", "family", "genus", "species"), sep = "\\|") %>%
    dplyr::select(., -kingdom, -order, -family, -genus, -species) %>%
    dplyr::mutate(class = ifelse(is.na(class), "PARENT", class)) %>%
    dplyr::select(., -phylum) %>% 
    dplyr::group_by(., class) %>%
    dplyr::summarise(., across(where(is.numeric), ~ sum(.))) %>% 
    tibble::column_to_rownames(., "class") %>%
    t() %>%
    as.data.frame()
  
  if ("PARENT" %!in% colnames(metaphlan_parent)) { 
    #print("PARENT not found, summing genus to produce PARENT")
    metaphlan_parent$PARENT <- rowSums(metaphlan_parent)
  }
  
  metaphlan_parent_merge <- merge(metadata, metaphlan_parent, by.x = "subject_id", by.y = "row.names")
  
  ### CORRELATION #####
  cor_drop <- suppressMessages(corrr::correlate(metaphlan_parent)) %>% corrr::focus(., PARENT) %>% dplyr::filter(., PARENT > 0.85) %>% pull(., term)
  
  metaphlan_parent_merge_cor_subset <- metaphlan_parent_merge %>% 
    dplyr::select(., -all_of(cor_drop)) %>%
    select(., -subject_id)
  
  if (NCOL(metaphlan_parent_merge_cor_subset) < 3) {
    #print("Parent wins due to correlation")
    
    taxa_only_split <- taxa_only_split %>% dplyr::mutate(., class = ifelse((phylum == parent_trial & !is.na(phylum) & na_count == 4), "drop_dis", class))
    taxa_only_split <- taxa_only_split %>% dplyr::filter(., !grepl(pattern = "drop_dis", x = class))
    

    
  } else {
    
    ## RF MODEL #####
    
    if (opt$feature_type == "factor") {
      model <- ranger::ranger(as.factor(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = seed, mtry = round(sqrt(NCOL(microbial_hfe)) * 1.25))
      model_importance <- as.data.frame(model$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
      nperm <- 10
      for (seed in sample(1:1000, nperm)) {
        model_tmp <- ranger::ranger(as.factor(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = seed, mtry = round(sqrt(NCOL(microbial_hfe)) * 1.25))
        model_importance_tmp <- as.data.frame(model_tmp$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
        model_importance <- merge(model_importance, model_importance_tmp, by = "taxa")
        
      }
      colnames(model_importance)[2:(nperm + 2)] <- paste0("permutation_", seq(1,nperm + 1))
      model_importance$average <- rowMeans(model_importance[, 2:(nperm + 2)])
      
    } else {
      model <- ranger::ranger(as.numeric(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = seed, mtry = round(sqrt(NCOL(microbial_hfe)) * 1.25))
      model_importance <- as.data.frame(model$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
      nperm <- 10
      for (seed in sample(1:1000, nperm)) {
        model_tmp <- ranger::ranger(as.numeric(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = seed, mtry = round(sqrt(NCOL(microbial_hfe)) * 1.25))
        model_importance_tmp <- as.data.frame(model_tmp$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
        model_importance <- merge(model_importance, model_importance_tmp, by = "taxa")
        
      }
      colnames(model_importance)[2:(nperm + 2)] <- paste0("permutation_", seq(1,nperm + 1))
      model_importance$average <- rowMeans(model_importance[, 2:(nperm + 2)])
      
    }
    ### RF WINNER - PARENT ####
    if (sapply(as.data.frame(model$variable.importance), function(x) head(row.names(as.data.frame(model$variable.importance))[order(x, decreasing = TRUE)], 1)) == "PARENT") {
      #print("Parent is most important feature in model")
      
      taxa_only_split <- taxa_only_split %>% dplyr::mutate(., class = ifelse((phylum == parent_trial & !is.na(phylum) & na_count == 4), "drop_dis", class))
      taxa_only_split <- taxa_only_split %>% dplyr::filter(., !grepl(pattern = "drop_dis", x = class))
      

    } else { 
      ### RF WINNER - CHILD ####
      
      model_importance <- as.data.frame(model$variable.importance) %>% 
        tibble::rownames_to_column(., var = "taxa") 
      
      parent_importance <- model_importance$`model$variable.importance`[model_importance$taxa == "PARENT"]
      
      children_toss <- model_importance %>% dplyr::filter(., `model$variable.importance` < parent_importance) %>% pull(., taxa)
      children_toss <- c(children_toss, cor_drop)
      
      ## drop parent
      taxa_only_split <- taxa_only_split %>% 
        dplyr::mutate(., class = ifelse((phylum == parent_trial & !is.na(phylum) & is.na(class)), "drop_dis", class))
      taxa_only_split <- taxa_only_split %>% 
        mutate_at(c("kingdom", "phylum", "family", "order", "genus", "species"), ~ 
                    replace(., phylum == parent_trial, NA)) %>%
        dplyr::filter(., !grepl(pattern = "drop_dis", x = class))
      
      ## drop children that didnt win against parent
      taxa_only_split <- taxa_only_split %>% 
        dplyr::mutate(., class = ifelse((class %in% children_toss & is.na(phylum) & !is.na(class)), "drop_dis", class))
      taxa_only_split <- taxa_only_split %>% 
        dplyr::filter(., !grepl(pattern = "drop_dis", x = class))
      
    }
  }
  ### PROGRESS ####
  svMisc::progress(count, length(phyla[phyla$count > 1, ]$phylum))
  Sys.sleep(0.01)
  if (count == length(phyla[phyla$count > 1, ]$phylum)) message("Done with Phyla!")
  count = count + 1
}

metaphlan <- metaphlan %>%
  dplyr::filter(., clade_name %in% taxa_only_split$metaphlan_taxonomy)

## Kingdom ======================================================================

kingdoms <- taxa_only_split %>%
  group_by(., kingdom) %>%
  summarize(., count = n_distinct(phylum)) %>% tidyr::drop_na()

count = 1

for (parent_trial in kingdoms[kingdoms$count > 1, ]$kingdom) {
  
  #print(paste0("Analyzing if phyla are more important than kingdom: ",parent_trial))
  metaphlan_parent <- metaphlan %>%
    dplyr::filter(., grepl(pattern = parent_trial, clade_name)) %>%
    tidyr::separate(., col = clade_name, into = c("kingdom", "phylum", "class", "order", "family", "genus", "species"), sep = "\\|") %>%
    dplyr::select(., -class, -order, -family, -genus, -species) %>%
    dplyr::mutate(phylum = ifelse(is.na(phylum), "PARENT", phylum)) %>%
    dplyr::select(., -kingdom) %>% 
    dplyr::group_by(., phylum) %>%
    dplyr::summarise(., across(where(is.numeric), ~ sum(.))) %>% 
    tibble::column_to_rownames(., "phylum") %>%
    t() %>%
    as.data.frame()
  
  if ("PARENT" %!in% colnames(metaphlan_parent)) { 
    #print("PARENT not found, summing genus to produce PARENT")
    metaphlan_parent$PARENT <- rowSums(metaphlan_parent)
  }
  
  metaphlan_parent_merge <- merge(metadata, metaphlan_parent, by.x = "subject_id", by.y = "row.names")
  
  ### CORRELATION #####
  cor_drop <- suppressMessages(corrr::correlate(metaphlan_parent)) %>% corrr::focus(., PARENT) %>% dplyr::filter(., PARENT > 0.85) %>% pull(., term)
  
  metaphlan_parent_merge_cor_subset <- metaphlan_parent_merge %>% 
    dplyr::select(., -all_of(cor_drop)) %>%
    select(., -subject_id)
  
  if (NCOL(metaphlan_parent_merge_cor_subset) < 3) {
    #print("Parent wins due to correlation")
    
    taxa_only_split <- taxa_only_split %>% dplyr::mutate(., phylum = ifelse((kingdom == parent_trial & !is.na(kingdom) & na_count == 5), "drop_dis", phylum))
    taxa_only_split <- taxa_only_split %>% dplyr::filter(., !grepl(pattern = "drop_dis", x = phylum))
    
    
    
  } else {
    
    ## RF MODEL #####
    
    if (opt$feature_type == "factor") {
      model <- ranger::ranger(as.factor(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = seed, mtry = round(sqrt(NCOL(microbial_hfe)) * 1.25))
      model_importance <- as.data.frame(model$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
      nperm <- 10
      for (seed in sample(1:1000, nperm)) {
        model_tmp <- ranger::ranger(as.factor(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = seed, mtry = round(sqrt(NCOL(microbial_hfe)) * 1.25))
        model_importance_tmp <- as.data.frame(model_tmp$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
        model_importance <- merge(model_importance, model_importance_tmp, by = "taxa")
        
      }
      colnames(model_importance)[2:(nperm + 2)] <- paste0("permutation_", seq(1,nperm + 1))
      model_importance$average <- rowMeans(model_importance[, 2:(nperm + 2)])
      
    } else {
      model <- ranger::ranger(as.numeric(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = seed, mtry = round(sqrt(NCOL(microbial_hfe)) * 1.25))
      model_importance <- as.data.frame(model$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
      nperm <- 10
      for (seed in sample(1:1000, nperm)) {
        model_tmp <- ranger::ranger(as.numeric(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = seed, mtry = round(sqrt(NCOL(microbial_hfe)) * 1.25))
        model_importance_tmp <- as.data.frame(model_tmp$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
        model_importance <- merge(model_importance, model_importance_tmp, by = "taxa")
        
      }
      colnames(model_importance)[2:(nperm + 2)] <- paste0("permutation_", seq(1,nperm + 1))
      model_importance$average <- rowMeans(model_importance[, 2:(nperm + 2)])
      
    }
    ### RF WINNER - PARENT ####
    if (sapply(as.data.frame(model$variable.importance), function(x) head(row.names(as.data.frame(model$variable.importance))[order(x, decreasing = TRUE)], 1)) == "PARENT") {
      #print("Parent is most important feature in model")
      
      taxa_only_split <- taxa_only_split %>% dplyr::mutate(., phylum = ifelse((kingdom == parent_trial & !is.na(kingdom) & na_count == 5), "drop_dis", phylum))
      taxa_only_split <- taxa_only_split %>% dplyr::filter(., !grepl(pattern = "drop_dis", x = phylum))
      
    } else { 
      ### RF WINNER - CHILD ####
      
      model_importance <- as.data.frame(model$variable.importance) %>% 
        tibble::rownames_to_column(., var = "taxa") 
      
      parent_importance <- model_importance$`model$variable.importance`[model_importance$taxa == "PARENT"]
      
      children_toss <- model_importance %>% dplyr::filter(., `model$variable.importance` < parent_importance) %>% pull(., taxa)
      children_toss <- c(children_toss, cor_drop)
      
      ## drop parent
      taxa_only_split <- taxa_only_split %>% 
        dplyr::mutate(., phylum = ifelse((kingdom == parent_trial & !is.na(kingdom) & is.na(phylum)), "drop_dis", phylum))
      taxa_only_split <- taxa_only_split %>% 
        mutate_at(c("kingdom", "phylum", "family", "order", "genus", "species"), ~ 
                    replace(., kingdom == parent_trial, NA)) %>%
        dplyr::filter(., !grepl(pattern = "drop_dis", x = phylum))
      
      ## drop children that didnt win against parent
      taxa_only_split <- taxa_only_split %>% 
        dplyr::mutate(., phylum = ifelse((phylum %in% children_toss & is.na(kingdom) & !is.na(phylum)), "drop_dis", phylum))
      taxa_only_split <- taxa_only_split %>% 
        dplyr::filter(., !grepl(pattern = "drop_dis", x = phylum))
      
    }
  }
  ### PROGRESS ####
  svMisc::progress(count, length(kingdoms[kingdoms$count > 1, ]$kingdom))
  Sys.sleep(0.01)
  if (count == length(kingdoms[kingdoms$count > 1, ]$kingdom)) message("Done with Kingdoms!")
  count = count + 1
}

metaphlan <- metaphlan %>%
  dplyr::filter(., clade_name %in% taxa_only_split$metaphlan_taxonomy)


## super filter ================================================================

# if (opt$super_filter == "TRUE") {
#   metaphlan_sf <- metaphlan %>%
#     tibble::column_to_rownames(., var = "clade_name") %>%
#     t() %>%
#     as.data.frame() %>%
#     tibble::rownames_to_column(., var = "subject_id") %>%
#     janitor::clean_names()
#   
#   output_sf <- merge(metadata, metaphlan_sf, by.x = "subject_id", by.y = "subject_id")
#   output_sf <- dplyr::select(., -subject_id) %>%
#     janitor::clean_names() 
#   
#   apples <- sample(x = 1:1000, size = 10, replace = F)
#   for (apple_seed in apples) {
#     
#     if (opt$feature_type == "factor") {
#       model <- ranger::ranger(as.factor(feature_of_interest) ~ . , data = output_sf, importance = "permutation", seed = apple_seed, mtry = round(sqrt(NCOL(microbial_hfe)) * 1.25))
#       model_importance <- as.data.frame(model$variable.importance) %>% tibble::rownames_to_column(., var = "taxa") 


#     } else {
#       model <- ranger::ranger(as.numeric(feature_of_interest) ~ . , data = output_sf, importance = "permutation", seed = apple_seed, mtry = round(sqrt(NCOL(microbial_hfe)) * 1.25))
#       
#     }
#   
#   }
#     
#   
# }



## Write Output ================================================================

metaphlan <- metaphlan %>%
  tibble::column_to_rownames(., var = "clade_name") %>%
  t() %>%
  as.data.frame() %>%
  tibble::rownames_to_column(., var = "subject_id") %>%
  janitor::clean_names()

cat(paste0("Reduced/compressed taxa set from ", original_taxa_count, " taxa to ", (NCOL(metaphlan) -1)))
output <- merge(metadata, metaphlan, by.x = "subject_id", by.y = "subject_id")
readr::write_delim(file = opt$output_file, x = output, delim = "\t")
readr::write_delim(file = "/home/output/taxa_only_split.txt", x = taxa_only_split, delim = "\t")

cat(".  Output written.  ")



