#!/usr/bin/env Rscript
## v0.3.0a.7

## SCRIPT: dietML_null_tidy.R ===================================================
## AUTHOR: Andrew Oliver
## DATE:   Mar, 20 2023
##
## PURPOSE: NULL model for tidymodels

## helper functions and vars ===================================================

## suppress warnings
options(warn=-1)

## Negate function ("not in"):
`%!in%` <- Negate(`%in%`)

## set seed
set.seed(as.numeric(opt$seed))

## load libraries ==============================================================

library(tidyr, quietly = T, verbose = F, warn.conflicts = F)
library(ggplot2, quietly = T, verbose = F, warn.conflicts = F)
suppressPackageStartupMessages(library(tidymodels, quietly = T, verbose = F, warn.conflicts = F))

## create results df ===========================================================

if (opt$type == "classification") {
  results_df <- data.frame(seed = as.numeric(), bal_accuracy = as.numeric(), f_meas = as.numeric(), accuracy = as.numeric(), stringsAsFactors = F)
} else if (opt$type == "regression") {
  results_df <- data.frame(seed = as.numeric(), mae = as.numeric(), rmse = as.numeric(), ccc = as.numeric(), stringsAsFactors = F)
}

## interate over null model ====================================================
seeds <- sample(x = 1:99999, size = 20, replace = F)

for (seed in seeds) {
  
  if (opt$type == "classification") {
    df_loop_results <- data.frame(truth = as.character(), estimate = as.character(), stringsAsFactors = F)
  } else if (opt$type == "regression") {
    df_loop_results <- data.frame(truth = as.numeric(), estimate = as.numeric(), stringsAsFactors = F)
  }
  
  ## set initial test-train split
  tr_te_split <- rsample::initial_split(input, prop = as.numeric(opt$train_split), strata = label)
  train <- rsample::training(tr_te_split)
  test  <- rsample::testing(tr_te_split)
  
  ## recipe ======================================================================
  
  ## specify recipe (this is like the pre-process work)
  dietML_recipe <- 
    recipes::recipe(label ~ ., data = train) %>% 
    recipes::update_role(tidyr::any_of(opt$subject_identifier), new_role = "ID") %>% 
    recipes::step_corr(all_numeric_predictors(), threshold = as.numeric(opt$cor_level)) %>%
    recipes::step_zv(all_predictors())
  
  
  ## ML engine ===================================================================
  
  ## specify ML model and engine 
  initial_mod <- null_model() %>% 
    set_engine("parsnip") %>% 
    set_mode(opt$type) %>% 
    translate()
  
  ## workflow ====================================================================
  
  ## define workflow
  dietML_wflow <- 
    workflows::workflow() %>% 
    workflows::add_model(initial_mod) %>% 
    workflows::add_recipe(dietML_recipe)  
  
  
  ## fit model ==============================================================
  
  ## fit to test data
  final_res <- parsnip::fit(dietML_wflow, test)
  
  df_loop_results <- add_row(df_loop_results, truth = test$label)
  df_loop_results$estimate <- final_res$fit$fit$fit$value
  
  if (opt$type == "classification") {
    df_loop_results$estimate <- factor(x = df_loop_results$estimate, levels = levels(as.factor(df_loop_results$truth)))
    results_df <- results_df %>% 
      tibble::add_row(., bal_accuracy = 
                        yardstick::bal_accuracy(truth = as.factor(df_loop_results$truth), 
                                       estimate = as.factor(df_loop_results$estimate), 
                                       data = df_loop_results)$.estimate, 
                      accuracy = 
                        yardstick::accuracy(truth = as.factor(df_loop_results$truth), 
                                                estimate = as.factor(df_loop_results$estimate), 
                                                data = df_loop_results)$.estimate, 
                      f_meas = 
                        yardstick::f_meas(truth = as.factor(df_loop_results$truth), 
                                                estimate = as.factor(df_loop_results$estimate), 
                                                data = df_loop_results)$.estimate,
                      seed = seed)
  } else if (opt$type == "regression") {
    results_df <- results_df %>% 
      tibble::add_row(., mae = 
                        yardstick::mae(truth = df_loop_results$truth, 
                                       estimate = df_loop_results$estimate, 
                                       data = df_loop_results)$.estimate, 
                      rmse = 
                        yardstick::rmse(truth = df_loop_results$truth, 
                                       estimate = df_loop_results$estimate, 
                                       data = df_loop_results)$.estimate,
                      ccc = yardstick::ccc(truth = df_loop_results$truth, 
                                            estimate = df_loop_results$estimate, 
                                            data = df_loop_results)$.estimate,
                      seed = seed)
    
  }
  
  
}

## write df ====================================================================

## write table of results to file
readr::write_csv(x = results_df, file = paste0(opt$outdir, "dummy_model_results.csv"), append = T, col_names = !file.exists(paste0(opt$outdir, "dummy_model_results.csv")))

## show the final results
cat("Performance of NULL model:", "\n")
cat("File: ", opt$input, "\n")
cat("Label: ", opt$label, "\n")
print(results_df %>% dplyr::select(-seed) %>% dplyr::summarise_all(., ~mean(.x)))

