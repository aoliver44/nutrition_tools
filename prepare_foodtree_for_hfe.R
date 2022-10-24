## prepare food tree file

food_tree_raw <- read_delim(file = "/home/data/food_tree_data/fl100_otu_abundance.txt", delim = "\t")

food_tree <- food_tree_raw %>% rename(., "clade_name" = "taxonomy") %>% select(., -`#FoodCode`) %>% relocate(., clade_name)
food_tree$clade_name <- gsub(pattern = ";", replacement = "\\|", x = food_tree$clade_name)

food_tree <- food_tree %>% 
  tidyr::separate(., col = clade_name, into = c("kingdom", "phylum", "class", "order", "family", "genus"), sep = "\\|")

food_tree$kingdom <- stringr::str_split(string = food_tree$kingdom, pattern = "_", n = 2, simplify = T)[,2]
food_tree$phylum <- stringr::str_split(string = food_tree$phylum, pattern = "_", n = 2, simplify = T)[,2]
food_tree$class <- stringr::str_split(string = food_tree$class, pattern = "_", n = 2, simplify = T)[,2]
food_tree$order <- stringr::str_split(string = food_tree$order, pattern = "_", n = 2, simplify = T)[,2]
food_tree$family <- stringr::str_split(string = food_tree$family, pattern = "_", n = 2, simplify = T)[,2]

## bring in last known taxonomic group
food_tree <- food_tree %>% 
  mutate(., phylum = ifelse(phylum == "", paste0(kingdom, "_unclassified"), phylum)) %>%
  mutate(., class = ifelse((class == "" & grepl("_unclassified", .data$phylum) == TRUE), phylum, 
                           ifelse((class == "" & grepl("_unclassified", .data$phylum) == FALSE), paste0(phylum, "_unclassified"), class))) %>%
  mutate(., order = ifelse((order == "" & grepl("_unclassified", .data$class) == TRUE), class, 
                           ifelse((order == "" & grepl("_unclassified", .data$class) == FALSE), paste0(class, "_unclassified"), order))) %>%
  mutate(., family = ifelse((family == "" & grepl("_unclassified", .data$order) == TRUE), order, 
                           ifelse((family == "" & grepl("_unclassified", .data$order) == FALSE), paste0(order, "_unclassified"), family)))

## identify the taxa group using L1-6
food_tree$kingdom <- paste0("L1_", food_tree$kingdom)
food_tree$phylum <- paste0(food_tree$kingdom, "|L2_", food_tree$phylum)
food_tree$class <- paste0(food_tree$phylum, "|L3_", food_tree$class)
food_tree$order <- paste0(food_tree$class, "|L4_", food_tree$order)
food_tree$family <- paste0(food_tree$order, "|L5_", food_tree$family)
food_tree$genus <- paste0(food_tree$family, "|L6_", food_tree$genus)


food_tree_1 <- food_tree %>% group_by(., kingdom) %>% dplyr::summarise(., across(where(is.numeric), ~ sum(.))) %>% dplyr::rename(., "clade_name" = "kingdom")
food_tree_2 <- food_tree %>% group_by(., phylum) %>% dplyr::summarise(., across(where(is.numeric), ~ sum(.))) %>% dplyr::rename(., "clade_name" = "phylum")
food_tree_3 <- food_tree %>% group_by(., class) %>% dplyr::summarise(., across(where(is.numeric), ~ sum(.))) %>% dplyr::rename(., "clade_name" = "class")
food_tree_4 <- food_tree %>% group_by(., order) %>% dplyr::summarise(., across(where(is.numeric), ~ sum(.))) %>% dplyr::rename(., "clade_name" = "order")
food_tree_5 <- food_tree %>% group_by(., family) %>% dplyr::summarise(., across(where(is.numeric), ~ sum(.))) %>% dplyr::rename(., "clade_name" = "family")
food_tree_6 <- food_tree %>% group_by(., genus) %>% dplyr::summarise(., across(where(is.numeric), ~ sum(.))) %>% dplyr::rename(., "clade_name" = "genus")

food_tree <- rbind(food_tree_1, food_tree_2, food_tree_3, food_tree_4, food_tree_5, food_tree_6)

write_delim(x = food_tree, file = "/home/data/synthetic_test_data/foodtree_for_hfe.txt", delim = "\t")


## Nutrient tree ====================

food_codes <- read_delim(file = "/home/data/food_tree_data/fl100_newick_taxonomy_nowater.txt", delim = "\t")
food_tree <- read_delim(file = "/home/data/food_tree_data/fl100_otu_abundance.txt", delim = "\t")
fndds <- readxl::read_xlsx(path = "/home/data/food_tree_data/2015-2016 FNDDS At A Glance - FNDDS Nutrient Values.xlsx", skip = 1)

## merge food otu (original) and food codes
food_tree_code <- merge(food_codes, food_tree, by.x = "Main.food.description", by.y = "#FoodCode")
food_tree_code <- food_tree_code %>% dplyr::select(., FoodID, taxonomy.y) %>% rename(., "taxonomy" = "taxonomy.y")
food_tree_code <- food_tree_code %>% separate(., col = "taxonomy", into = c("L1", "L2", "L3", "L4", "L5", "L6"), extra = "merge", sep = ";")

## merge with nutritional data
food_nutrition <- merge(food_tree_code, fndds, by.x = "FoodID", by.y = "Food code")
food_nutrition <- food_nutrition %>% dplyr::select(., 1, 11:last_col())
food_nutrition$FoodID <- paste0("fc_", food_nutrition$FoodID)

food_nutrition <- food_nutrition %>% tibble::column_to_rownames(., "FoodID")

## scale data
food_nutrition_scaled <- scale(food_nutrition)

## vis of MDS of the data
set.seed(seed = 999)
beta.mds <- vegan::metaMDS(food_nutrition_scaled, distance = 'euclidean')
vegan::stressplot(beta.mds)

sites <- as.data.frame(scores(beta.mds, display = "sites"))
food_tree_code_data <- food_tree_code
food_tree_code_data$FoodID <- paste0("fc_", food_tree_code_data$FoodID)
nmds.sites <- merge(sites, food_tree_code_data, by.x = "row.names", by.y = "FoodID")

ggplot() + 
  geom_point(data = nmds.sites, aes(NMDS1, NMDS2, color = L1), size = 3, alpha = 0.8) + 
  theme_bw() + theme(panel.border = element_rect(colour = "black", size = 1.5), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank()) + scale_color_d3()

beta.mds$stress

## tsne ======================

library("Rtsne")
tSNE_fit <- food_nutrition_scaled %>%
  Rtsne(check_duplicates = FALSE, exaggeration_factor = 12)

tSNE_df <- tSNE_fit$Y %>% 
  as.data.frame() %>%
  rename(tSNE1="V1",
         tSNE2="V2") %>%
  mutate(ID=row_number())

food_tree_code_data <- food_tree_code_data[food_tree_code_data$FoodID %in% rownames(food_nutrition_scaled), ]
food_tree_code_data <- food_tree_code_data[order(food_tree_code_data$FoodID, rownames(food_nutrition_scaled)), ]
food_tree_code_data <- food_tree_code_data %>% mutate(ID=row_number())

tSNE_df <- tSNE_df %>%
  inner_join(food_tree_code_data, by="ID")

tSNE_df %>%
  dplyr::filter(., L1 == "L1_Sugars_Sweets_and_Beverages") %>%
  ggplot(aes(x = tSNE1, 
             y = tSNE2,
             color = L3))+
  geom_point()+
  theme_bw() + 
  #theme(legend.position="none") + 
  scale_color_d3()

food_tree_code_data %>% group_by(., L1) %>% tally

