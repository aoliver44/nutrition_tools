## prepare food tree file

food_tree <- read_delim(file = "/home/data/synthetic_test_data/fl100_otu_abundance.txt", delim = "\t")

food_tree <- food_tree %>% rename(., "clade_name" = "taxonomy") %>% select(., -`#FoodCode`) %>% relocate(., clade_name)
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
food_tree$phylum <- paste0("L2_", food_tree$phylum)
food_tree$class <- paste0("L3_", food_tree$class)
food_tree$order <- paste0("L4_", food_tree$order)
food_tree$family <- paste0("L5_", food_tree$family)
food_tree$genus <- paste0("L6_", food_tree$genus)

## paste it all together
food_tree$taxonomy <- paste0(food_tree$kingdom, "|", food_tree$phylum, "|", food_tree$class, "|", food_tree$order, "|", food_tree$family, "|", food_tree$genus)


food_tree <- food_tree %>% dplyr::select(., 7:last_col()) %>% relocate(., taxonomy) %>% rename(., "clade_name" = "taxonomy")

write_delim(x = food_tree, file = "/home/data/synthetic_test_data/foodtree_for_hfe.txt", delim = "\t")
