#!/usr/bin/env Rscript
## v0.3.0a.2

## SCRIPT: dietML_ranger_tidy.R ===================================================
## AUTHOR: Andrew Oliver
## DATE:   Jan, 30 2023
##
## PURPOSE: RF model for tidymodels

## helper functions and vars ===================================================

## unregister hung-up parallel jobs
unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}

## suppress warnings
options(warn=-1)

## Negate function ("not in"):
`%!in%` <- Negate(`%in%`)

## set seed
set.seed(as.numeric(opt$seed))

## load libraries ==============================================================

library(mikropml, quietly = T, verbose = F, warn.conflicts = F)
library(tidyr, quietly = T, verbose = F, warn.conflicts = F)
library(ggplot2, quietly = T, verbose = F, warn.conflicts = F)
suppressPackageStartupMessages(library(tidymodels, quietly = T, verbose = F, warn.conflicts = F))
library(ranger, quietly = T, verbose = F, warn.conflicts = F)


## resample strategy ===========================================================

## set initial test-train split
tr_te_split <- rsample::initial_split(input, prop = as.numeric(opt$train_split), strata = label)
train <- rsample::training(tr_te_split)
test  <- rsample::testing(tr_te_split)

## set resampling scheme
folds <- rsample::vfold_cv(train, v = as.numeric(opt$folds), strata = label, repeats = 3)
#folds <- rsample::bootstraps(train, times = as.numeric(opt$folds), strata = label, apparent = F)

## recipe ======================================================================

## specify recipe (this is like the pre-process work)
dietML_recipe <- 
  recipes::recipe(label ~ ., data = train) %>% 
  recipes::update_role(tidyr::any_of(opt$subject_identifier), new_role = "ID") %>% 
  recipes::step_corr(all_numeric_predictors(), threshold = as.numeric(opt$cor_level), use = "everything") %>%
  recipes::step_zv(all_predictors())

## ML engine ===================================================================

## specify ML model and engine 
initial_mod <- parsnip::rand_forest(mode = opt$type, 
                               mtry = tune(),
                               trees = 1500,
                               min_n = tune()) %>%
  parsnip::set_engine("ranger", 
                      num.threads = 1,
                      importance = "none")

initial_mod %>% parsnip::translate()

## workflow ====================================================================

## define workflow
dietML_wflow <- 
  workflows::workflow() %>% 
  workflows::add_model(initial_mod) %>% 
  workflows::add_recipe(dietML_recipe)  

## set up parallel jobs ========================================================
## remove any doParallel job setups that may have
## unneccessarily hung around
unregister_dopar()

## register parallel cluster
cl <- parallel::makePSOCKcluster(as.numeric(opt$ncores))
doParallel::registerDoParallel(cl)

## hyperparameters =============================================================

## define the hyper parameter set
dietML_param_set <- parsnip::extract_parameter_set_dials(dietML_wflow)

## for random forests, set mtry to max features after correlation
## co-correlate features at specified threshold (get upper limit of mtry)
training_cor <- mikropml:::group_correlated_features(train %>% dplyr::select(., -label, -dplyr::any_of(opt$subject_identifier)), 
                                                      corr_thresh = as.numeric(opt$cor_level), group_neg_corr = T)

## make dataframe of what is correlated at specified threshold.
training_cor <- as.data.frame(training_cor) %>% 
  tidyr::separate(., col = training_cor, into = c("keep", "co_correlated"), sep = "\\|", extra = "merge")

## set mtry to max features after correlation
dietML_param_set <- 
  dietML_param_set %>% 
  # Pick an upper bound for mtry: 
  recipes::update(mtry = mtry(c(2, round((NROW(training_cor) * 0.9), digits = 0))))

## set up hyper parameter search
if (opt$type == "classification") {
  
  search_res <-
    dietML_wflow %>% 
    tune::tune_bayes(
      resamples = folds,
      # To use non-default parameter ranges
      param_info = dietML_param_set,
      # Generate five at semi-random to start
      initial = 5,
      iter = opt$tune_length,
      # How to measure performance?
      metrics = yardstick::metric_set(bal_accuracy, roc_auc, accuracy, kap, f_meas),
      control = tune::control_bayes(no_improve = as.numeric(opt$tune_stop),
                                    uncertain = 5,
                                    verbose = FALSE,
                                    parallel_over = "resamples",
                                    time_limit = as.numeric(opt$tune_time),
                                    seed = as.numeric(opt$seed))
    )
  
} else if (opt$type == "regression") {
  
  search_res <-
    dietML_wflow %>% 
    tune::tune_bayes(
      resamples = folds,
      # To use non-default parameter ranges
      param_info = dietML_param_set,
      # Generate five at semi-random to start
      initial = 5,
      iter = opt$tune_length,
      # How to measure performance?
      metrics = yardstick::metric_set(mae, rmse, rsq, ccc),
      control = tune::control_bayes(no_improve = as.numeric(opt$tune_stop),
                                    uncertain = 5,
                                    verbose = FALSE,
                                    parallel_over = "resamples",
                                    time_limit = as.numeric(opt$tune_time),
                                    seed = as.numeric(opt$seed))
    )
}

search_res %>% tune::show_best(opt$metric)

## stop parallel jobs
parallel::stopCluster(cl)
## remove any doParallel job setups that may have
## unneccessarily hung around
unregister_dopar()

## fit best model ==============================================================

## get the best parameters from tuning
best_mod <- 
    search_res %>% 
    tune::select_best(metric = opt$metric)

## create the last model based on best parameters
last_best_mod <- 
  parsnip::rand_forest(mtry = best_mod$mtry, min_n = best_mod$min_n) %>% 
  parsnip::set_engine("ranger", num.threads = as.numeric(opt$ncores), importance = "none") %>% 
  parsnip::set_mode(opt$type)

## update workflow with best model
best_tidy_workflow <- 
  dietML_wflow %>% 
  workflows::update_model(last_best_mod)

## fit to test data
if (opt$type == "classification") {
  final_res <- tune::last_fit(best_tidy_workflow, tr_te_split, 
                              metrics = yardstick::metric_set(bal_accuracy, 
                                                              roc_auc, accuracy, 
                                                              kap, f_meas))
} else if (opt$type == "regression") {
  final_res <- tune::last_fit(best_tidy_workflow, tr_te_split, 
                              metrics = yardstick::metric_set(mae, rmse, rsq, 
                                                              ccc))
}

cat("\n################\n")
cat("RESULTS:", "\n")
cat("##################\n\n")

## show the final results
cat("Performance of test set:", "\n")
cat("File: ", opt$input, "\n")
cat("Label: ", opt$label, "\n")
cat("Model: ", opt$model, "\n")
print(workflowsets::collect_metrics(final_res))

## graphs ======================================================================

hyperpar_perf_plot <- autoplot(search_res, type = "performance")
ggplot2::ggsave(plot = hyperpar_perf_plot, filename = paste0(opt$outdir, "training_performance.pdf"), width = 7, height = 2.5, units = "in")

hyperpar_tested_plot <- autoplot(search_res, type = "parameters") + 
  labs(x = "Iterations", y = NULL)
ggplot2::ggsave(plot = hyperpar_tested_plot, filename = paste0(opt$outdir, "hyperpars_tested.pdf"), width = 7, height = 2.5, units = "in")

## remove any doParallel job setups that may have
## unneccessarily hung around
unregister_dopar()
