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
## ./microbial_HFE.R --subject_identifier subject_id --feature_type factor --var_control 5 --label cluster --super_filter TRUE --feature_limit 15 /home/data/synthetic_test_data/abx_cluster_andrew_bi.csv /home/data/synthetic_test_data/merged_metaphlan4.txt /home/output_old/abx_cluster_bi_metaphlan4.txt

## set working dir to /home for the docker container
setwd("/home")

## add commandline options =====================================================

library(docopt)
'Microbial hierarchical feature engineering (HFE) of metaphfor classification
Usage:
    microbial_HFE.R [--subject_identifier=<subject_colname> --label=<label> --feature_type=<feature_type> --var_control=<pct> --super_filter=<TRUE/FALSE> --feature_limit=<number_of_features>] <input_metadata> <input> <outpute>
    
Options:
    -h --help  Show this screen.
    -v --version  Show version.
    --subject_identifier name of columns with subject IDs [default: subject_id]
    --label response feature of interest for classification [default: cluster]
    --feature_type of response i.e. numeric or factor [default: factor]
    --var_control filter features that contain less than this threshold of percentage of unique features [default: 5]
    --super_filter to run a final RF and only take positive values [default: FALSE]
    --feature_limit limits output to best N number of features (NOTE: if changed, must set superfilter to TRUE) [default: ALL]
Arguments:
    input_meta path to metadata input (txt | tsv | csv)
    input path to input file from hierarchical data (i.e. metaphlan data) (txt | tsv | csv)
    output output file name (txt)

' -> doc

opt <- docopt::docopt(doc, version = 'microbial_HFE.R v1.1\n\n')
#print(opt)
## load libraries ==============================================================

library(dplyr)
library(janitor)
library(tidyr)
library(tibble)
library(caret)
library(readr)
library(reshape2)
library(ggplot2)
library(ggsci)

## set random seed if needed
set.seed(42)
nperm = 10
## helper functions ============================================================

## Negate function ("not in"):
`%!in%` <- Negate(`%in%`)

## arg tests ===================================================================
# opt <- data.frame(subject_identifier=character(),
#                   label=character(),
#                   feature_type=character(),
#                   var_control=numeric(),
#                   super_filter=character(),
#                   feature_limit=numeric(),
#                   input_metadata=character(),
#                   input=character(),
#                   output=character())
# opt <- opt %>% tibble::add_row(subject_identifier = "subject_id", label= "cluster", feature_type = "factor", var_control = 5, super_filter = "TRUE", feature_limit = 15, input_metadata = "/home/data/synthetic_test_data/abx_cluster_andrew_bi.csv", input= "/home/data/synthetic_test_data/merged_metaphlan4.txt", output = "/home/output_old/abx_cluster_bi_metaphlan4.txt")



## check for inputs ============================================================

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

## read in data, should be in tab or comma separated format
if (strsplit(basename(opt$input), split="\\.")[[1]][2] %in% c("tsv","txt")) {
metaphlan <- readr::read_delim(file = opt$input, delim = "\t", skip = 0) %>% dplyr::select(., -any_of(c("NCBI_tax_id", "clade_taxid")))
} else {
  metaphlan <- readr::read_delim(file = opt$input, delim = ",", skip = 0) %>% dplyr::select(., -any_of(c("NCBI_tax_id", "clade_taxid")))
}

original_taxa_count <- NROW(metaphlan)

## read in metadata file and rename the subject_identifier to subject_id and
## rename the label to feature_of_interest
## metadata, should be in tab or comma separated format
if (strsplit(basename(opt$input_metadata), split="\\.")[[1]][2] %in% c("tsv","txt")) {
metadata <- readr::read_delim(file = opt$input_metadata, delim = "\t")
} else {
  metadata <- readr::read_delim(file = opt$input_metadata, delim = ",")
}

metadata <- metadata %>% dplyr::select(., opt$subject_identifier, opt$label)
metadata <- metadata %>% dplyr::rename(., "subject_id" = opt$subject_identifier) %>%
  rename(., "feature_of_interest" = opt$label) 

## Remove low variance features ================================================

## in feature engineering, youre not ~really~ supposed to know the
## whole dataset prior to ML analysis. But this script could definitely
## be better about that generally...and maybe someday it will be.

## randomly subsample 75% of the entire dataset for variance filtering
random_subsample <- sample(colnames(metaphlan[2:NCOL(metaphlan)]), (NCOL(metaphlan[,2:NCOL(metaphlan)]) * 0.75))
metaphlan_var <- metaphlan %>%
  tibble::column_to_rownames(., var = "clade_name") %>%
  dplyr::select(., all_of(random_subsample)) %>%
  t() %>% 
  as.data.frame()

## filter for taxa for which the percentage of samples with unique values is 
## greater than var_control (greater than var_control means more samples 
## have different values, useful for ML applications). 
## See caret::nearZeroVar for more info.

cat("\n", "Removing features that that have the same values in", (100 - as.numeric(opt$var_control)),"%(+) of the samples...\n")
nzv <- caret::nearZeroVar(metaphlan_var,saveMetrics= TRUE)
non_nzv_taxa <- nzv %>%
  tibble::rownames_to_column(., var = "taxa") %>%
  dplyr::filter(., percentUnique > as.numeric(opt$var_control)) %>%
  dplyr::pull(., "taxa")

## select features that are above the var_control threshold (in the
## non_nzv_taxa vector)
metaphlan <- metaphlan %>%
  dplyr::filter(., clade_name %in% non_nzv_taxa)
metaphlan_master <- metaphlan

## make the dataframe of features that will compete in parent-child competitions.
## Basically this is just the taxonomic (hierarchical) information of all
## the features left at this step.
taxa_only_split <- metaphlan %>% 
  tidyr::separate(., col = clade_name, into = c("kingdom", "phylum", "class", "order", "family", "genus", "species", "type"), sep = "\\|") %>%
  dplyr::select(., 1:8)

## add some columns to this dataframe (full clade name, full taxa abundance, number of NAs)
## NOTE: future scripts could add an abundance threshold filter here
taxa_only_split$metaphlan_taxonomy <- metaphlan$clade_name
taxa_only_split$taxa_abundance <- rowSums(metaphlan[,2:NCOL(metaphlan)])
taxa_only_split$na_count <- rowSums(is.na(taxa_only_split))
taxa_only_split_master <- taxa_only_split

## inform user about how many features were dropped because they had too little
## variability (var_control filter)
cat("\n",paste0((NROW(nzv) - NROW(metaphlan)), " taxa dropped due to variability filter.\n"))
Sys.sleep(0.25)


## Species =====================================================================

## make a dataframe of all the species and how many sub-species each species has
specieses <- taxa_only_split %>%
  dplyr::group_by(., species) %>%
  dplyr::summarize(., count = n_distinct(type)) %>% tidyr::drop_na()

## start a counter to keep track of each for-loop 
count = 1

## loop through all the species with greater than 1 sub-species. If species
## has only itself, it will automatically be kept and used in the genus step.
for (parent_trial in specieses[specieses$count > 1, ]$species) {
  
  ## create a dataframe of the parent and children. In this case the abundance
  ## of a species and the subspecies across samples.
  metaphlan_parent <- metaphlan %>%
    dplyr::filter(., grepl(pattern = parent_trial, clade_name)) %>%
    tidyr::separate(., col = clade_name, into = c("kingdom", "phylum", "class", "order", "family", "genus", "species", "type"), sep = "\\|") %>%
    dplyr::select(., -kingdom, -phylum, -class, -order, -family, -genus) %>%
    dplyr::mutate(type = ifelse(is.na(type), "PARENT", type)) %>%
    dplyr::select(., -species) %>% 
    dplyr::group_by(., type) %>%
    dplyr::summarise(., across(where(is.numeric), ~ sum(.))) %>% 
    tibble::column_to_rownames(., "type") %>%
    t() %>%
    as.data.frame()
  
  ## if no columns named parent exists (species renamed parent in previous step)
  ## create column by summing up children (sub-species)
  if ("PARENT" %!in% colnames(metaphlan_parent)) { 
    metaphlan_parent$PARENT <- rowSums(metaphlan_parent)
  }
  ## merge with metadata
  metaphlan_parent_merge <- merge(metadata, metaphlan_parent, by.x = "subject_id", by.y = "row.names")
  
  ### CORRELATION #####
  
  ## correlate parent with children. If parent (species) is highly (pearson = 0.85) 
  ## correlated with child, drop the highly correlated child...they dont bring
  ## more information to the table that is otherwise carried in the parent (species in this case)
  cor_drop <- suppressMessages(corrr::correlate(metaphlan_parent)) %>% corrr::focus(., PARENT) %>% dplyr::filter(., PARENT > 0.85) %>% dplyr::pull(., term)
  
  metaphlan_parent_merge_cor_subset <- metaphlan_parent_merge %>% 
    dplyr::select(., -all_of(cor_drop)) %>%
    select(., -subject_id)
  
  ## if, after dropping highly correlated children, only species is left,
  ## drop from taxa_only_split (dataframe keeping track of all kept features)
  ## and move on to the next species...ELSE, run a random forest, below
  if (NCOL(metaphlan_parent_merge_cor_subset) < 3) {
    ## so this is selecting the features in taxa_only_split where the for-loop
    ## variable is found in the Parent column (in this case species), and there are no
    ## NAs (which is expected, because the hierarchy is complete if were looking at
    ## species vs subspecies. If we were looking at genus vs species, we'd expect 1 NA, because
    ## after this step, only a species (PARENT) or a subspecies(CHILD) moved forward, and one was 
    ## dropped...thus a NA was created. Just a way to make the dropping more selective.)
    taxa_only_split <- taxa_only_split %>% dplyr::mutate(., type = ifelse((species == parent_trial & !is.na(species) & na_count == 0), "drop_dis", type))
    taxa_only_split <- taxa_only_split %>% dplyr::filter(., !grepl(pattern = "drop_dis", x = type))
    
    ## So some subspecies (CHILDREN) were not correlated with the species (PARENT)...
    ## Do these species bring more information to the table with regards to the
    ## feature_of_interest?
  } else {
    
    ## RF MODEL #####
    
    ## if the feature of interest is a factor, do a RF Classification
    if (opt$feature_type == "factor") {
      ## create an intial model and keep track of the variable.importance
      ## this will help us decide if the PARENT (species) or the CHILD (sub-speceies)
      ## brings more information to the table with regards to feature_of_interest
      model <- ranger::ranger(as.factor(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = 42)
      model_importance <- as.data.frame(model$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
      
      ## welp, the initial model miiight be correct, but lets permute that process
      ## by looping over some random seeds (10, as set in a very early variable nperm at the top)
      ## and keeping track of the variable.importance. We can then average that and have
      ## a more sure guess whether a Parent or Child is more important with regards to 
      ## the feature_of_interest
      for (seed in sample(1:1000, nperm)) {
        model_tmp <- ranger::ranger(as.factor(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = seed)
        model_importance_tmp <- as.data.frame(model_tmp$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
        model_importance <- merge(model_importance, model_importance_tmp, by = "taxa")
        
      }
      colnames(model_importance)[2:(nperm + 2)] <- paste0("permutation_", seq(1,nperm + 1))
      model_importance$average <- rowMeans(model_importance[, 2:(nperm + 2)])
      
      ## this else statement does the sample as the above few lines, just for a continous
      ## feature_of_interest...with RF Regression.
    } else {
      model <- ranger::ranger(as.numeric(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = 42)
      model_importance <- as.data.frame(model$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
      for (seed in sample(1:1000, nperm)) {
        model_tmp <- ranger::ranger(as.numeric(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = seed)
        model_importance_tmp <- as.data.frame(model_tmp$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
        model_importance <- merge(model_importance, model_importance_tmp, by = "taxa")
        
      }
      colnames(model_importance)[2:(nperm + 2)] <- paste0("permutation_", seq(1,nperm + 1))
      model_importance$average <- rowMeans(model_importance[, 2:(nperm + 2)])
      
      
    }
    
    ### RF WINNER - PARENT ####
    ## Ok we have the permuted variable.importance. Do, via the average, any
    ## children (sub-species) beat the parents with regards to discriminatory information
    ## with regards to the feature_of_interest? 
    ## If the largest average variable importance is PARENT:
    if ((model_importance %>% dplyr::arrange(., desc(average)) %>% slice_head(., n = 1) %>% dplyr::pull(., taxa)) == "PARENT") {
      ## drop children from master taxa file taxa_only_split
      taxa_only_split <- taxa_only_split %>% dplyr::mutate(., type = ifelse((species == parent_trial & !is.na(species) & na_count == 0), "drop_dis", type))
      taxa_only_split <- taxa_only_split %>% dplyr::filter(., !grepl(pattern = "drop_dis", x = type))


    } else { 
      ### RF WINNER - CHILD ####
      parent_importance <- model_importance$average[model_importance$taxa == "PARENT"]
      
      children_toss <- model_importance %>% dplyr::filter(., average < parent_importance) %>% dplyr::pull(., taxa)
      children_toss_zero <- model_importance%>% dplyr::filter(., average < 0) %>% dplyr::pull(., taxa) 
      children_toss <- unique(c(children_toss, cor_drop, children_toss_zero))
      
      ## drop parent
      taxa_only_split <- taxa_only_split %>% 
        dplyr::mutate(., type = ifelse((species == parent_trial & !is.na(species) & is.na(type)), "drop_dis", type))
      taxa_only_split <- taxa_only_split %>% 
        mutate_at(c("kingdom", "phylum", "class", "order", "family", "genus", "species"), ~ 
                    replace(., species == parent_trial, NA)) %>%
        dplyr::filter(., !grepl(pattern = "drop_dis", x = type))
      
      ## drop children that didnt win against parent
      taxa_only_split <- taxa_only_split %>% 
        dplyr::mutate(., type = ifelse((type %in% children_toss & is.na(species) & !is.na(type)), "drop_dis", type))
      taxa_only_split <- taxa_only_split %>% 
        dplyr::filter(., !grepl(pattern = "drop_dis", x = type))
      
    }
  }
  ### PROGRESS ####
  svMisc::progress(count, length(specieses[specieses$count > 1, ]$species))
  if (count == length(specieses[specieses$count > 1, ]$species)) message("Done with Species!")
  count = count + 1
}

metaphlan <- metaphlan %>%
  dplyr::filter(., clade_name %in% taxa_only_split$metaphlan_taxonomy)


## Genus =======================================================================

genera <- taxa_only_split %>%
  dplyr::group_by(., genus) %>%
  dplyr::summarize(., count = n_distinct(species)) %>% tidyr::drop_na()

count = 1

for (parent_trial in genera[genera$count > 1, ]$genus) {

  metaphlan_parent <- metaphlan %>%
    dplyr::filter(., grepl(pattern = parent_trial, clade_name)) %>%
    tidyr::separate(., col = clade_name, into = c("kingdom", "phylum", "class", "order", "family", "genus", "species", "type"), sep = "\\|") %>%
    dplyr::select(., -kingdom, -phylum, -class, -order, -family, -type) %>%
    dplyr::mutate(species = ifelse(is.na(species), "PARENT", species)) %>%
    dplyr::select(., -genus) %>% 
    dplyr::group_by(., species) %>%
    dplyr::summarise(., across(where(is.numeric), ~ sum(.))) %>% 
    tibble::column_to_rownames(., "species") %>%
    t() %>%
    as.data.frame()
  
  if ("PARENT" %!in% colnames(metaphlan_parent)) { 
    metaphlan_parent$PARENT <- rowSums(metaphlan_parent)
  }
  
  metaphlan_parent_merge <- merge(metadata, metaphlan_parent, by.x = "subject_id", by.y = "row.names")
  
  ### CORRELATION #####
  cor_drop <- suppressMessages(corrr::correlate(metaphlan_parent)) %>% corrr::focus(., PARENT) %>% dplyr::filter(., PARENT > 0.85) %>% dplyr::pull(., term)

  metaphlan_parent_merge_cor_subset <- metaphlan_parent_merge %>% 
    dplyr::select(., -all_of(cor_drop)) %>%
    select(., -subject_id)
  
  if (NCOL(metaphlan_parent_merge_cor_subset) < 3) {

    taxa_only_split <- taxa_only_split %>% dplyr::mutate(., species = ifelse((genus == parent_trial & !is.na(genus) & na_count == 1), "drop_dis", species))
    taxa_only_split <- taxa_only_split %>% dplyr::filter(., !grepl(pattern = "drop_dis", x = species))
    

  } else {
  
    ## RF MODEL #####
    
    if (opt$feature_type == "factor") {
      model <- ranger::ranger(as.factor(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = 42)
      model_importance <- as.data.frame(model$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
      for (seed in sample(1:1000, nperm)) {
        model_tmp <- ranger::ranger(as.factor(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = seed)
        model_importance_tmp <- as.data.frame(model_tmp$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
        model_importance <- merge(model_importance, model_importance_tmp, by = "taxa")
        
      }
      colnames(model_importance)[2:(nperm + 2)] <- paste0("permutation_", seq(1,nperm + 1))
      model_importance$average <- rowMeans(model_importance[, 2:(nperm + 2)])
      
      
    } else {
      model <- ranger::ranger(as.numeric(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = 42)
      model_importance <- as.data.frame(model$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
      for (seed in sample(1:1000, nperm)) {
        model_tmp <- ranger::ranger(as.numeric(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = seed)
        model_importance_tmp <- as.data.frame(model_tmp$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
        model_importance <- merge(model_importance, model_importance_tmp, by = "taxa")
        
      }
      colnames(model_importance)[2:(nperm + 2)] <- paste0("permutation_", seq(1,nperm + 1))
      model_importance$average <- rowMeans(model_importance[, 2:(nperm + 2)])
      
      
    }
    
    ### RF WINNER - PARENT ####
    if ((model_importance %>% dplyr::arrange(., desc(average)) %>% slice_head(., n = 1) %>% dplyr::pull(., taxa)) == "PARENT") {

      taxa_only_split <- taxa_only_split %>% dplyr::mutate(., species = ifelse((genus == parent_trial & !is.na(genus) & na_count == 1), "drop_dis", species))
      taxa_only_split <- taxa_only_split %>% dplyr::filter(., !grepl(pattern = "drop_dis", x = species))


    } else { 
      ### RF WINNER - CHILD ####
      parent_importance <- model_importance$average[model_importance$taxa == "PARENT"]
      
      children_toss <- model_importance %>% dplyr::filter(., average < parent_importance) %>% dplyr::pull(., taxa)
      children_toss_zero <- model_importance%>% dplyr::filter(., average < 0) %>% dplyr::pull(., taxa) 
      children_toss <- unique(c(children_toss, cor_drop, children_toss_zero))
      
      ## drop parent
      taxa_only_split <- taxa_only_split %>% 
        dplyr::mutate(., species = ifelse((genus == parent_trial & !is.na(genus) & is.na(species)), "drop_dis", species))
      taxa_only_split <- taxa_only_split %>% 
        mutate_at(c("kingdom", "phylum", "class", "order", "family", "genus", "type"), ~ 
                    replace(., genus == parent_trial, NA)) %>%
        dplyr::filter(., !grepl(pattern = "drop_dis", x = species))
      
      ## drop children that didnt win against parent
      taxa_only_split <- taxa_only_split %>% 
        dplyr::mutate(., species = ifelse((species %in% children_toss & is.na(genus) & !is.na(species)), "drop_dis", species))
      taxa_only_split <- taxa_only_split %>% 
        dplyr::filter(., !grepl(pattern = "drop_dis", x = species))
      
     }
  }
    ### PROGRESS ####
    svMisc::progress(count, length(genera[genera$count > 1, ]$genus))
    if (count == length(genera[genera$count > 1, ]$genus)) message("Done with Genus!")
    count = count + 1
}

metaphlan <- metaphlan %>%
  dplyr::filter(., clade_name %in% taxa_only_split$metaphlan_taxonomy)

## save for debugging
metaphlan_genus <- metaphlan
taxa_only_split_genus <- taxa_only_split

## Family ======================================================================

families <- taxa_only_split %>%
  dplyr::group_by(., family) %>%
  dplyr::summarize(., count = n_distinct(genus)) %>% tidyr::drop_na()

count = 1

for (parent_trial in families[families$count > 1, ]$family) {
  
  metaphlan_parent <- metaphlan %>%
    dplyr::filter(., grepl(pattern = parent_trial, clade_name)) %>%
    tidyr::separate(., col = clade_name, into = c("kingdom", "phylum", "class", "order", "family", "genus", "species", "type"), sep = "\\|") %>%
    dplyr::select(., -kingdom, -phylum, -class, -order, -species, -type) %>%
    dplyr::mutate(genus = ifelse(is.na(genus), "PARENT", genus)) %>%
    dplyr::select(., -family) %>% 
    dplyr::group_by(., genus) %>%
    dplyr::summarise(., across(where(is.numeric), ~ sum(.))) %>% 
    tibble::column_to_rownames(., "genus") %>%
    t() %>%
    as.data.frame()
  
  if ("PARENT" %!in% colnames(metaphlan_parent)) { 
    metaphlan_parent$PARENT <- rowSums(metaphlan_parent)
  }
  
  metaphlan_parent_merge <- merge(metadata, metaphlan_parent, by.x = "subject_id", by.y = "row.names")
  
  ### CORRELATION #####
  cor_drop <- suppressMessages(corrr::correlate(metaphlan_parent)) %>% corrr::focus(., PARENT) %>% dplyr::filter(., PARENT > 0.85) %>% dplyr::pull(., term)
  
  metaphlan_parent_merge_cor_subset <- metaphlan_parent_merge %>% 
    dplyr::select(., -all_of(cor_drop)) %>%
    dplyr::select(., -subject_id)
  
  if (NCOL(metaphlan_parent_merge_cor_subset) < 3) {

    taxa_only_split <- taxa_only_split %>% dplyr::mutate(., genus = ifelse((family == parent_trial & !is.na(family) & na_count == 2), "drop_dis", genus))
    taxa_only_split <- taxa_only_split %>% dplyr::filter(., !grepl(pattern = "drop_dis", x = genus))
    
    
  } else {
    
    ## RF MODEL #####
    
    if (opt$feature_type == "factor") {
      model <- ranger::ranger(as.factor(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = 42)
      model_importance <- as.data.frame(model$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
      for (seed in sample(1:1000, nperm)) {
        model_tmp <- ranger::ranger(as.factor(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = seed)
        model_importance_tmp <- as.data.frame(model_tmp$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
        model_importance <- merge(model_importance, model_importance_tmp, by = "taxa")
        
      }
      colnames(model_importance)[2:(nperm + 2)] <- paste0("permutation_", seq(1,nperm + 1))
      model_importance$average <- rowMeans(model_importance[, 2:(nperm + 2)])
      
      
    } else {
      model <- ranger::ranger(as.numeric(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = 42)
      model_importance <- as.data.frame(model$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
      
      for (seed in sample(1:1000, nperm)) {
        model_tmp <- ranger::ranger(as.numeric(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = seed)
        model_importance_tmp <- as.data.frame(model_tmp$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
        model_importance <- merge(model_importance, model_importance_tmp, by = "taxa")
        
      }
      colnames(model_importance)[2:(nperm + 2)] <- paste0("permutation_", seq(1,nperm + 1))
      model_importance$average <- rowMeans(model_importance[, 2:(nperm + 2)])
      
      
    }
    ### RF WINNER - PARENT ####
    if ((model_importance %>% dplyr::arrange(., desc(average)) %>% slice_head(., n = 1) %>% dplyr::pull(., taxa)) == "PARENT") {

      taxa_only_split <- taxa_only_split %>% dplyr::mutate(., genus = ifelse((family == parent_trial & !is.na(family) & na_count == 2), "drop_dis", genus))
      taxa_only_split <- taxa_only_split %>% dplyr::filter(., !grepl(pattern = "drop_dis", x = genus))


    } else { 
      ### RF WINNER - CHILD ####
      parent_importance <- model_importance$average[model_importance$taxa == "PARENT"]
      
      children_toss <- model_importance %>% dplyr::filter(., average < parent_importance) %>% dplyr::pull(., taxa)
      children_toss_zero <- model_importance%>% dplyr::filter(., average < 0) %>% dplyr::pull(., taxa) 
      children_toss <- unique(c(children_toss, cor_drop, children_toss_zero))
      
      ## drop parent
      taxa_only_split <- taxa_only_split %>% 
        dplyr::mutate(., genus = ifelse((family == parent_trial & !is.na(family) & is.na(genus)), "drop_dis", genus))
      taxa_only_split <- taxa_only_split %>% 
        mutate_at(c("kingdom", "phylum", "class", "order", "family", "species", "type"), ~ 
                    replace(., family == parent_trial, NA)) %>%
        dplyr::filter(., !grepl(pattern = "drop_dis", x = genus))
      
      ## drop children that didnt win against parent
      taxa_only_split <- taxa_only_split %>% 
        dplyr::mutate(., genus = ifelse((genus %in% children_toss & is.na(family) & !is.na(genus)), "drop_dis", genus))
      taxa_only_split <- taxa_only_split %>% 
        dplyr::filter(., !grepl(pattern = "drop_dis", x = genus))
      
    }
  }
  ### PROGRESS ####
  svMisc::progress(count, length(families[families$count > 1, ]$family))
  if (count == length(families[families$count > 1, ]$family)) message("Done with Family!")
  count = count + 1
}

metaphlan <- metaphlan %>%
  dplyr::filter(., clade_name %in% taxa_only_split$metaphlan_taxonomy)

## save for debugging
metaphlan_family <- metaphlan
taxa_only_split_family <- taxa_only_split

## Order ======================================================================

orders <- taxa_only_split %>%
  dplyr::group_by(., order) %>%
  dplyr::summarize(., count = n_distinct(family)) %>% tidyr::drop_na()

count = 1

for (parent_trial in orders[orders$count > 1, ]$order) {
  
  metaphlan_parent <- metaphlan %>%
    dplyr::filter(., grepl(pattern = parent_trial, clade_name)) %>%
    tidyr::separate(., col = clade_name, into = c("kingdom", "phylum", "class", "order", "family", "genus", "species", "type"), sep = "\\|") %>%
    dplyr::select(., -kingdom, -phylum, -class, -genus, -species, -type) %>%
    dplyr::mutate(family = ifelse(is.na(family), "PARENT", family)) %>%
    dplyr::select(., -order) %>% 
    dplyr::group_by(., family) %>%
    dplyr::summarise(., across(where(is.numeric), ~ sum(.))) %>% 
    tibble::column_to_rownames(., "family") %>%
    t() %>%
    as.data.frame()
  
  if ("PARENT" %!in% colnames(metaphlan_parent)) { 
    metaphlan_parent$PARENT <- rowSums(metaphlan_parent)
  }
  
  metaphlan_parent_merge <- merge(metadata, metaphlan_parent, by.x = "subject_id", by.y = "row.names")
  
  ### CORRELATION #####
  cor_drop <- suppressMessages(corrr::correlate(metaphlan_parent)) %>% corrr::focus(., PARENT) %>% dplyr::filter(., PARENT > 0.85) %>% dplyr::pull(., term)
  
  metaphlan_parent_merge_cor_subset <- metaphlan_parent_merge %>% 
    dplyr::select(., -all_of(cor_drop)) %>%
    select(., -subject_id)
  
  if (NCOL(metaphlan_parent_merge_cor_subset) < 3) {

    taxa_only_split <- taxa_only_split %>% dplyr::mutate(., family = ifelse((order == parent_trial & !is.na(order) & na_count == 3), "drop_dis", family))
    taxa_only_split <- taxa_only_split %>% dplyr::filter(., !grepl(pattern = "drop_dis", x = family))
    
    
  } else {
    
    ## RF MODEL #####
    
    if (opt$feature_type == "factor") {
      model <- ranger::ranger(as.factor(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = 42)
      model_importance <- as.data.frame(model$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
      for (seed in sample(1:1000, nperm)) {
        model_tmp <- ranger::ranger(as.factor(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = seed)
        model_importance_tmp <- as.data.frame(model_tmp$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
        model_importance <- merge(model_importance, model_importance_tmp, by = "taxa")
        
      }
      colnames(model_importance)[2:(nperm + 2)] <- paste0("permutation_", seq(1,nperm + 1))
      model_importance$average <- rowMeans(model_importance[, 2:(nperm + 2)])
      
      
    } else {
      model <- ranger::ranger(as.numeric(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = 42)
      model_importance <- as.data.frame(model$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
      
      for (seed in sample(1:1000, nperm)) {
        model_tmp <- ranger::ranger(as.numeric(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = seed)
        model_importance_tmp <- as.data.frame(model_tmp$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
        model_importance <- merge(model_importance, model_importance_tmp, by = "taxa")
        
      }
      colnames(model_importance)[2:(nperm + 2)] <- paste0("permutation_", seq(1,nperm + 1))
      model_importance$average <- rowMeans(model_importance[, 2:(nperm + 2)])
      
      
    }
    ### RF WINNER - PARENT ####
    if ((model_importance %>% dplyr::arrange(., desc(average)) %>% slice_head(., n = 1) %>% dplyr::pull(., taxa)) == "PARENT") {

      taxa_only_split <- taxa_only_split %>% dplyr::mutate(., family = ifelse((order == parent_trial & !is.na(order) & na_count == 3), "drop_dis", family))
      taxa_only_split <- taxa_only_split %>% dplyr::filter(., !grepl(pattern = "drop_dis", x = family))


    } else { 
      ### RF WINNER - CHILD ####
      parent_importance <- model_importance$average[model_importance$taxa == "PARENT"]
      
      children_toss <- model_importance %>% dplyr::filter(., average < parent_importance) %>% dplyr::pull(., taxa)
      children_toss_zero <- model_importance%>% dplyr::filter(., average < 0) %>% dplyr::pull(., taxa) 
      children_toss <- unique(c(children_toss, cor_drop, children_toss_zero))
      
      ## drop parent
      taxa_only_split <- taxa_only_split %>% 
        dplyr::mutate(., family = ifelse((order == parent_trial & !is.na(order) & is.na(family)), "drop_dis", family))
      taxa_only_split <- taxa_only_split %>% 
        mutate_at(c("kingdom", "phylum", "class", "order", "genus", "species", "type"), ~ 
                    replace(., order == parent_trial, NA)) %>%
        dplyr::filter(., !grepl(pattern = "drop_dis", x = family))
      
      ## drop children that didnt win against parent
      taxa_only_split <- taxa_only_split %>% 
        dplyr::mutate(., family = ifelse((family %in% children_toss & is.na(order) & !is.na(family)), "drop_dis", family))
      taxa_only_split <- taxa_only_split %>% 
        dplyr::filter(., !grepl(pattern = "drop_dis", x = family))

    }
  }
  ### PROGRESS ####
  svMisc::progress(count, length(orders[orders$count > 1, ]$order))
  if (count == length(orders[orders$count > 1, ]$order)) message("Done with Order!")
  count = count + 1
}

metaphlan <- metaphlan %>%
  dplyr::filter(., clade_name %in% taxa_only_split$metaphlan_taxonomy)

## save for debugging
metaphlan_order <- metaphlan
taxa_only_split_order <- taxa_only_split

## Class ======================================================================

classes <- taxa_only_split %>%
  dplyr::group_by(., class) %>%
  dplyr::summarize(., count = n_distinct(order)) %>% tidyr::drop_na()

count = 1

for (parent_trial in classes[classes$count > 1, ]$class) {
  
  metaphlan_parent <- metaphlan %>%
    dplyr::filter(., grepl(pattern = parent_trial, clade_name)) %>%
    tidyr::separate(., col = clade_name, into = c("kingdom", "phylum", "class", "order", "family", "genus", "species", "type"), sep = "\\|") %>%
    dplyr::select(., -kingdom, -phylum, -family, -genus, -species, -type) %>%
    dplyr::mutate(order = ifelse(is.na(order), "PARENT", order)) %>%
    dplyr::select(., -class) %>% 
    dplyr::group_by(., order) %>%
    dplyr::summarise(., across(where(is.numeric), ~ sum(.))) %>% 
    tibble::column_to_rownames(., "order") %>%
    t() %>%
    as.data.frame()
  
  if ("PARENT" %!in% colnames(metaphlan_parent)) { 
    metaphlan_parent$PARENT <- rowSums(metaphlan_parent)
  }
  
  metaphlan_parent_merge <- merge(metadata, metaphlan_parent, by.x = "subject_id", by.y = "row.names")
  
  ### CORRELATION #####
  cor_drop <- suppressMessages(corrr::correlate(metaphlan_parent)) %>% corrr::focus(., PARENT) %>% dplyr::filter(., PARENT > 0.85) %>% dplyr::pull(., term)
  
  metaphlan_parent_merge_cor_subset <- metaphlan_parent_merge %>% 
    dplyr::select(., -all_of(cor_drop)) %>%
    select(., -subject_id)
  
  if (NCOL(metaphlan_parent_merge_cor_subset) < 3) {

    taxa_only_split <- taxa_only_split %>% dplyr::mutate(., order = ifelse((class == parent_trial & !is.na(class) & na_count == 4), "drop_dis", order))
    taxa_only_split <- taxa_only_split %>% dplyr::filter(., !grepl(pattern = "drop_dis", x = order))
    

    
  } else {
    
    ## RF MODEL #####
    
    if (opt$feature_type == "factor") {
      model <- ranger::ranger(as.factor(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = 42)
      model_importance <- as.data.frame(model$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
      for (seed in sample(1:1000, nperm)) {
        model_tmp <- ranger::ranger(as.factor(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = seed)
        model_importance_tmp <- as.data.frame(model_tmp$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
        model_importance <- merge(model_importance, model_importance_tmp, by = "taxa")
        
      }
      colnames(model_importance)[2:(nperm + 2)] <- paste0("permutation_", seq(1,nperm + 1))
      model_importance$average <- rowMeans(model_importance[, 2:(nperm + 2)])
      
      
    } else {
      model <- ranger::ranger(as.numeric(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = 42)
      model_importance <- as.data.frame(model$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
      
      for (seed in sample(1:1000, nperm)) {
        model_tmp <- ranger::ranger(as.numeric(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = seed)
        model_importance_tmp <- as.data.frame(model_tmp$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
        model_importance <- merge(model_importance, model_importance_tmp, by = "taxa")
        
      }
      colnames(model_importance)[2:(nperm + 2)] <- paste0("permutation_", seq(1,nperm + 1))
      model_importance$average <- rowMeans(model_importance[, 2:(nperm + 2)])
      
      
    }
    ### RF WINNER - PARENT ####
    if ((model_importance %>% dplyr::arrange(., desc(average)) %>% slice_head(., n = 1) %>% dplyr::pull(., taxa)) == "PARENT") {

      taxa_only_split <- taxa_only_split %>% dplyr::mutate(., order = ifelse((class == parent_trial & !is.na(class) & na_count == 4), "drop_dis", order))
      taxa_only_split <- taxa_only_split %>% dplyr::filter(., !grepl(pattern = "drop_dis", x = order))


    } else { 
      ### RF WINNER - CHILD ####

      parent_importance <- model_importance$average[model_importance$taxa == "PARENT"]
      
      children_toss <- model_importance %>% dplyr::filter(., average < parent_importance) %>% dplyr::pull(., taxa)
      children_toss_zero <- model_importance%>% dplyr::filter(., average < 0) %>% dplyr::pull(., taxa) 
      children_toss <- unique(c(children_toss, cor_drop, children_toss_zero))
      
      ## drop parent
      taxa_only_split <- taxa_only_split %>% 
        dplyr::mutate(., order = ifelse((class == parent_trial & !is.na(class) & is.na(order)), "drop_dis", order))
      taxa_only_split <- taxa_only_split %>% 
        mutate_at(c("kingdom", "phylum", "class", "family", "genus", "species", "type"), ~ 
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
  if (count == length(classes[classes$count > 1, ]$class)) message("Done with Class!")
  count = count + 1
}

metaphlan <- metaphlan %>%
  dplyr::filter(., clade_name %in% taxa_only_split$metaphlan_taxonomy)

## save for debugging
metaphlan_class <- metaphlan
taxa_only_split_class <- taxa_only_split

## Phylum ======================================================================

phyla <- taxa_only_split %>%
  dplyr::group_by(., phylum) %>%
  dplyr::summarize(., count = n_distinct(class)) %>% tidyr::drop_na()

count = 1

for (parent_trial in phyla[phyla$count > 1, ]$phylum) {
  
  metaphlan_parent <- metaphlan %>%
    dplyr::filter(., grepl(pattern = parent_trial, clade_name)) %>%
    tidyr::separate(., col = clade_name, into = c("kingdom", "phylum", "class", "order", "family", "genus", "species", "type"), sep = "\\|") %>%
    dplyr::select(., -kingdom, -order, -family, -genus, -species, -type) %>%
    dplyr::mutate(class = ifelse(is.na(class), "PARENT", class)) %>%
    dplyr::select(., -phylum) %>% 
    dplyr::group_by(., class) %>%
    dplyr::summarise(., across(where(is.numeric), ~ sum(.))) %>% 
    tibble::column_to_rownames(., "class") %>%
    t() %>%
    as.data.frame()
  
  if ("PARENT" %!in% colnames(metaphlan_parent)) { 
    metaphlan_parent$PARENT <- rowSums(metaphlan_parent)
  }
  
  metaphlan_parent_merge <- merge(metadata, metaphlan_parent, by.x = "subject_id", by.y = "row.names")
  
  ### CORRELATION #####
  cor_drop <- suppressMessages(corrr::correlate(metaphlan_parent)) %>% corrr::focus(., PARENT) %>% dplyr::filter(., PARENT > 0.85) %>% dplyr::pull(., term)
  
  metaphlan_parent_merge_cor_subset <- metaphlan_parent_merge %>% 
    dplyr::select(., -all_of(cor_drop)) %>%
    select(., -subject_id)
  
  if (NCOL(metaphlan_parent_merge_cor_subset) < 3) {

    taxa_only_split <- taxa_only_split %>% dplyr::mutate(., class = ifelse((phylum == parent_trial & !is.na(phylum) & na_count == 5), "drop_dis", class))
    taxa_only_split <- taxa_only_split %>% dplyr::filter(., !grepl(pattern = "drop_dis", x = class))
    

    
  } else {
    
    ## RF MODEL #####
    
    if (opt$feature_type == "factor") {
      model <- ranger::ranger(as.factor(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = 42)
      model_importance <- as.data.frame(model$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
      for (seed in sample(1:1000, nperm)) {
        model_tmp <- ranger::ranger(as.factor(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = seed)
        model_importance_tmp <- as.data.frame(model_tmp$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
        model_importance <- merge(model_importance, model_importance_tmp, by = "taxa")
        
      }
      colnames(model_importance)[2:(nperm + 2)] <- paste0("permutation_", seq(1,nperm + 1))
      model_importance$average <- rowMeans(model_importance[, 2:(nperm + 2)])
      
      
    } else {
      model <- ranger::ranger(as.numeric(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = 42)
      model_importance <- as.data.frame(model$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
      
      for (seed in sample(1:1000, nperm)) {
        model_tmp <- ranger::ranger(as.numeric(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = seed)
        model_importance_tmp <- as.data.frame(model_tmp$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
        model_importance <- merge(model_importance, model_importance_tmp, by = "taxa")
        
      }
      colnames(model_importance)[2:(nperm + 2)] <- paste0("permutation_", seq(1,nperm + 1))
      model_importance$average <- rowMeans(model_importance[, 2:(nperm + 2)])
      
      
    }
    ### RF WINNER - PARENT ####
    if ((model_importance %>% dplyr::arrange(., desc(average)) %>% slice_head(., n = 1) %>% dplyr::pull(., taxa)) == "PARENT") {

      taxa_only_split <- taxa_only_split %>% dplyr::mutate(., class = ifelse((phylum == parent_trial & !is.na(phylum) & na_count == 5), "drop_dis", class))
      taxa_only_split <- taxa_only_split %>% dplyr::filter(., !grepl(pattern = "drop_dis", x = class))


    } else { 
      ### RF WINNER - CHILD ####

      parent_importance <- model_importance$average[model_importance$taxa == "PARENT"]
      
      children_toss <- model_importance %>% dplyr::filter(., average < parent_importance) %>% dplyr::pull(., taxa)
      children_toss_zero <- model_importance%>% dplyr::filter(., average < 0) %>% dplyr::pull(., taxa) 
      children_toss <- unique(c(children_toss, cor_drop, children_toss_zero))
      
      ## drop parent
      taxa_only_split <- taxa_only_split %>% 
        dplyr::mutate(., class = ifelse((phylum == parent_trial & !is.na(phylum) & is.na(class)), "drop_dis", class))
      taxa_only_split <- taxa_only_split %>% 
        mutate_at(c("kingdom", "phylum", "family", "order", "genus", "species", "type"), ~ 
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
  if (count == length(phyla[phyla$count > 1, ]$phylum)) message("Done with Phyla!")
  count = count + 1
}

metaphlan <- metaphlan %>%
  dplyr::filter(., clade_name %in% taxa_only_split$metaphlan_taxonomy)

## save for debugging
metaphlan_phylum <- metaphlan
taxa_only_split_phylum <- taxa_only_split

## Kingdom ======================================================================

kingdoms <- taxa_only_split %>%
  dplyr::group_by(., kingdom) %>%
  dplyr::summarize(., count = n_distinct(phylum)) %>% tidyr::drop_na()

count = 1

for (parent_trial in kingdoms[kingdoms$count > 1, ]$kingdom) {
  
  metaphlan_parent <- metaphlan %>%
    dplyr::filter(., grepl(pattern = parent_trial, clade_name)) %>%
    tidyr::separate(., col = clade_name, into = c("kingdom", "phylum", "class", "order", "family", "genus", "species", "type"), sep = "\\|") %>%
    dplyr::select(., -class, -order, -family, -genus, -species, -type) %>%
    dplyr::mutate(phylum = ifelse(is.na(phylum), "PARENT", phylum)) %>%
    dplyr::select(., -kingdom) %>% 
    dplyr::group_by(., phylum) %>%
    dplyr::summarise(., across(where(is.numeric), ~ sum(.))) %>% 
    tibble::column_to_rownames(., "phylum") %>%
    t() %>%
    as.data.frame()
  
  if ("PARENT" %!in% colnames(metaphlan_parent)) { 
    metaphlan_parent$PARENT <- rowSums(metaphlan_parent)
  }
  
  metaphlan_parent_merge <- merge(metadata, metaphlan_parent, by.x = "subject_id", by.y = "row.names")
  
  ### CORRELATION #####
  cor_drop <- suppressMessages(corrr::correlate(metaphlan_parent)) %>% corrr::focus(., PARENT) %>% dplyr::filter(., PARENT > 0.85) %>% dplyr::pull(., term)
  
  metaphlan_parent_merge_cor_subset <- metaphlan_parent_merge %>% 
    dplyr::select(., -all_of(cor_drop)) %>%
    select(., -subject_id)
  
  if (NCOL(metaphlan_parent_merge_cor_subset) < 3) {

    taxa_only_split <- taxa_only_split %>% dplyr::mutate(., phylum = ifelse((kingdom == parent_trial & !is.na(kingdom) & na_count == 6), "drop_dis", phylum))
    taxa_only_split <- taxa_only_split %>% dplyr::filter(., !grepl(pattern = "drop_dis", x = phylum))
    
    
    
  } else {
    
    ## RF MODEL #####
    
    if (opt$feature_type == "factor") {
      model <- ranger::ranger(as.factor(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = 42)
      model_importance <- as.data.frame(model$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
      for (seed in sample(1:1000, nperm)) {
        model_tmp <- ranger::ranger(as.factor(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = seed)
        model_importance_tmp <- as.data.frame(model_tmp$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
        model_importance <- merge(model_importance, model_importance_tmp, by = "taxa")
        
      }
      colnames(model_importance)[2:(nperm + 2)] <- paste0("permutation_", seq(1,nperm + 1))
      model_importance$average <- rowMeans(model_importance[, 2:(nperm + 2)])
      
      
    } else {
      model <- ranger::ranger(as.numeric(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = 42)
      model_importance <- as.data.frame(model$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
      
      for (seed in sample(1:1000, nperm)) {
        model_tmp <- ranger::ranger(as.numeric(feature_of_interest) ~ . , data = metaphlan_parent_merge_cor_subset, importance = "permutation", seed = seed)
        model_importance_tmp <- as.data.frame(model_tmp$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
        model_importance <- merge(model_importance, model_importance_tmp, by = "taxa")
        
      }
      colnames(model_importance)[2:(nperm + 2)] <- paste0("permutation_", seq(1,nperm + 1))
      model_importance$average <- rowMeans(model_importance[, 2:(nperm + 2)])
      
      
    }
    ### RF WINNER - PARENT ####
    if ((model_importance %>% dplyr::arrange(., desc(average)) %>% slice_head(., n = 1) %>% dplyr::pull(., taxa)) == "PARENT") {

      taxa_only_split <- taxa_only_split %>% dplyr::mutate(., phylum = ifelse((kingdom == parent_trial & !is.na(kingdom) & na_count == 6), "drop_dis", phylum))
      taxa_only_split <- taxa_only_split %>% dplyr::filter(., !grepl(pattern = "drop_dis", x = phylum))

    } else { 
      ### RF WINNER - CHILD ####
      
      parent_importance <- model_importance$average[model_importance$taxa == "PARENT"]
      
      children_toss <- model_importance %>% dplyr::filter(., average < parent_importance) %>% dplyr::pull(., taxa)
      children_toss_zero <- model_importance%>% dplyr::filter(., average < 0) %>% dplyr::pull(., taxa) 
      children_toss <- unique(c(children_toss, cor_drop, children_toss_zero))
      
      ## drop parent
      taxa_only_split <- taxa_only_split %>% 
        dplyr::mutate(., phylum = ifelse((kingdom == parent_trial & !is.na(kingdom) & is.na(phylum)), "drop_dis", phylum))
      taxa_only_split <- taxa_only_split %>% 
        mutate_at(c("kingdom", "class", "family", "order", "genus", "species", "type"), ~ 
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
  if (count == length(kingdoms[kingdoms$count > 1, ]$kingdom)) message("Done with Kingdoms!")
  count = count + 1
}

metaphlan <- metaphlan %>%
  dplyr::filter(., clade_name %in% taxa_only_split$metaphlan_taxonomy)

## super filter ================================================================

if (opt$super_filter == "TRUE") {
 metaphlan_sf <- metaphlan %>%
   tibble::column_to_rownames(., var = "clade_name") %>%
   t() %>%
   as.data.frame() %>%
   tibble::rownames_to_column(., var = "subject_id") %>%
   janitor::clean_names()
 
 metaphlan_sf <- merge(metadata, metaphlan_sf, by.x = "subject_id", by.y = "subject_id")
 output_sf <- metaphlan_sf %>% dplyr::select(., -subject_id) 
 nperm = nperm + 190
 
 if (opt$feature_type == "factor") {  
   model <- ranger::ranger(as.factor(feature_of_interest) ~ . , data = output_sf, importance = "permutation", seed = 42)
   model_importance <- as.data.frame(model$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
   for (seed in sample(1:1000, nperm)) {
     model_tmp <- ranger::ranger(as.factor(feature_of_interest) ~ . , data = output_sf, importance = "permutation", seed = seed)
     model_importance_tmp <- as.data.frame(model_tmp$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
     model_importance <- merge(model_importance, model_importance_tmp, by = "taxa")
     
   }
   colnames(model_importance)[2:(nperm + 2)] <- paste0("permutation_", seq(1,nperm + 1))
   model_importance$average <- rowMeans(model_importance[, 2:(nperm + 2)])
   model_importance <- model_importance %>% dplyr::relocate(., average)
  } else {
    model <- ranger::ranger(as.numeric(feature_of_interest) ~ . , data = output_sf, importance = "permutation", seed = 42)
    model_importance <- as.data.frame(model$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
    for (seed in sample(1:1000, nperm)) {
      model_tmp <- ranger::ranger(as.numeric(feature_of_interest) ~ . , data = output_sf, importance = "permutation", seed = seed)
      model_importance_tmp <- as.data.frame(model_tmp$variable.importance) %>% tibble::rownames_to_column(., var = "taxa")
      model_importance <- merge(model_importance, model_importance_tmp, by = "taxa")
   
    }
    colnames(model_importance)[2:(nperm + 2)] <- paste0("permutation_", seq(1,nperm + 1))
    model_importance$average <- rowMeans(model_importance[, 2:(nperm + 2)])
    model_importance <- model_importance %>% dplyr::relocate(., average)
  }
 
 if (opt$feature_limit != "ALL") {
   
   model_importance_list <- model_importance %>% dplyr::filter(., average > mean(average)) %>% dplyr::filter(., average > 0) %>% dplyr::arrange(dplyr::desc(average)) %>% dplyr::slice_head(n = as.numeric(opt$feature_limit)) %>% dplyr::pull(., taxa)
   output <- metaphlan_sf %>% dplyr::select(., subject_id, feature_of_interest, all_of(model_importance_list))
   taxa_only_split$metaphlan_taxonomy <- janitor::make_clean_names(taxa_only_split$metaphlan_taxonomy)
   taxa_only_split <- taxa_only_split %>% dplyr::filter(., metaphlan_taxonomy %in% model_importance_list)
   
 } else{
 
 model_importance_list <- model_importance %>% dplyr::filter(., average > mean(average)) %>% dplyr::filter(., average > 0) %>% dplyr::pull(., taxa)
 output <- metaphlan_sf %>% dplyr::select(., subject_id, feature_of_interest, all_of(model_importance_list))
 taxa_only_split$metaphlan_taxonomy <- janitor::make_clean_names(taxa_only_split$metaphlan_taxonomy)
 taxa_only_split <- taxa_only_split %>% dplyr::filter(., metaphlan_taxonomy %in% model_importance_list)

 }
 
}


## No super filter =============================================================

if (opt$super_filter == "FALSE") {
  metaphlan_output <- metaphlan %>%
    tibble::column_to_rownames(., var = "clade_name") %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column(., var = "subject_id") %>%
    janitor::clean_names()
  output <- merge(metadata, metaphlan_output, by.x = "subject_id", by.y = "subject_id")
}

## Write Figure ================================================================

if (opt$super_filter == "TRUE") {
  
  top_features <- model_importance %>% dplyr::filter(., average > mean(average)) %>% dplyr::filter(., average > 0) %>% dplyr::arrange(., desc(average)) %>% dplyr::pull(., taxa)
  top_features <- top_features[1:pmin(10, length(top_features))]
  figure_data <- output %>% dplyr::select(., feature_of_interest, any_of(top_features)) %>%
    reshape2::melt(id.vars = "feature_of_interest")
  
  if (opt$feature_type == "factor") {
    ggplot(data = figure_data) +
      aes(x = as.factor(feature_of_interest), y = log(value)) +
      geom_boxplot(aes(fill = as.factor(feature_of_interest)), outlier.alpha = 0) +
      geom_point(position = position_jitter(width = 0.2), alpha = 0.4) +
      facet_wrap( ~ variable, scales = "free_y") +
      theme_bw() + theme(strip.text.x = element_text(size = 2.5), legend.position = "none") + 
      ggsci::scale_fill_jama()
    
    ggsave(filename = paste0(tools::file_path_sans_ext(opt$output), "_plot.pdf"), device = "pdf", dpi = "retina", width = 11, height = 8, units = "in")
  
  } else {
    ggplot(data = figure_data) +
      aes(x = feature_of_interest, y = log(value)) +
      geom_point() +
      geom_smooth(method = "lm") +
      facet_wrap( ~ variable, scales = "free_y") +
      theme_bw() + theme(strip.text.x = element_text(size = 2.5))
    
    ggsave(filename = paste0(tools::file_path_sans_ext(opt$output), "_plot.pdf"), device = "pdf", dpi = "retina", width = 11, height = 8, units = "in")

  }
 }


## Write outputs ===============================================================

filename <- basename(tools::file_path_sans_ext(opt$output))
cat("\n",paste0("Reduced/compressed taxa set from ", original_taxa_count, " taxa to ", (NROW(taxa_only_split))))
readr::write_delim(file = opt$output, x = output, delim = "\t")
readr::write_delim(file = paste0(tools::file_path_sans_ext(opt$output), "_taxa_list.txt"), x = taxa_only_split, delim = "\t")
save.image(file = paste0(tools::file_path_sans_ext(opt$output), ".RData"))
cat("\n","Output written.  ")

