#!/usr/bin/env Rscript

## SCRIPT: dietML_glmnet_tidy_ridge_lasso.R ====================================
## AUTHOR: Andrew Oliver
## DATE:   Jan, 30 2023
##
## PURPOSE: Glmnet (ridge/lasso) model for tidymodels

## helper functions and vars ===================================================

## define lasso, ridge, enet
if (opt$model == "lasso") {
  mixture_value = 1
} else if (opt$model == "ridge") {
  mixture_value = 0
} 

## suppress warnings
options(warn=-1)

## Negate function ("not in"):
`%!in%` <- Negate(`%in%`)

## unregister hung-up parallel jobs
unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}

## load libraries ==============================================================

library(mikropml, quietly = T, verbose = F, warn.conflicts = F)
library(tidyr, quietly = T, verbose = F, warn.conflicts = F)
library(ggplot2, quietly = T, verbose = F, warn.conflicts = F)
suppressPackageStartupMessages(library(tidymodels, quietly = T, verbose = F, warn.conflicts = F))
library(glmnet, quietly = T, verbose = F, warn.conflicts = F)


## resample strategy ===========================================================

## set initial test-train split
tr_te_split <- rsample::initial_split(input, prop = as.numeric(opt$train_split), strata = label)
train <- rsample::training(tr_te_split)
test  <- rsample::testing(tr_te_split)

## set resampling scheme
set.seed(1697)
folds <- rsample::vfold_cv(train, v = 10)

## recipe ======================================================================

## specify recipe (this is like the pre-process work)
dietML_recipe <- 
  recipes::recipe(label ~ ., data = train) %>% 
  recipes::update_role(tidyr::any_of(opt$subject_identifier), new_role = "ID") %>% 
  recipes::step_corr(all_numeric_predictors(), threshold = as.numeric(opt$cor_level)) %>%
  recipes::step_zv(all_predictors())

## ML engine ===================================================================

## specify ML model and engine 
initial_mod <- parsnip::logistic_reg(mode = "classification", 
                                     penalty = tune(),
                                     mixture = mixture_value) %>%
  parsnip::set_engine("glmnet")

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


## set up hyper parameter search
if (opt$type == "classification") {
  search_res <-
    dietML_wflow %>% 
    tune::tune_bayes(
      resamples = folds,
      # To use non-default parameter ranges
      param_info = dietML_param_set,
      # Generate five at semi-random to start
      initial = 20,
      iter = opt$tune_length,
      # How to measure performance?
      metrics = yardstick::metric_set(bal_accuracy, roc_auc, accuracy, kap),
      control = tune::control_bayes(no_improve = 10, 
                                    verbose = FALSE,
                                    time_limit = as.numeric(opt$tune_time))
    )
  
} else if (opt$type == "regression") {
  search_res <-
    dietML_wflow %>% 
    tune::tune_bayes(
      resamples = folds,
      # To use non-default parameter ranges
      param_info = dietML_param_set,
      # Generate five at semi-random to start
      initial = 20,
      iter = opt$tune_length,
      # How to measure performance?
      metrics = yardstick::metric_set(mae, rmse, rsq),
      control = tune::control_bayes(no_improve = 10, 
                                    verbose = FALSE,
                                    time_limit = as.numeric(opt$tune_time))
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
  parsnip::logistic_reg(mode = "classification", penalty = best_mod$penalty, mixture = mixture_value) %>% 
  parsnip::set_engine("glmnet") %>% 
  parsnip::set_mode(opt$type)

## update workflow with best model
best_tidy_workflow <- 
  dietML_wflow %>% 
  workflows::update_model(last_best_mod)

## fit to test data
final_res <- tune::last_fit(best_tidy_workflow, tr_te_split)

## show the final results
cat("\n", "Performance of test set:", "\n")
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