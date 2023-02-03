#!/usr/bin/env Rscript

## SCRIPT: shap_figures.R ======================================================
## AUTHOR: Andrew Oliver
## DATE:   Jan, 30 2023
##
## PURPOSE: fastshap and viz for tidymodels

## load libraries
library(fastshap, quietly = T, verbose = F, warn.conflicts = F)
library(shapviz, quietly = T, verbose = F, warn.conflicts = F)
library(ggplot2, quietly = T, verbose = F, warn.conflicts = F)
library(tidymodels, quietly = T, verbose = F, warn.conflicts = F)

shap.error.occured <- FALSE

tryCatch( { if (length(levels(as.factor(input$label))) == 2) {
  
  ## Prediction wrapper: first level (ie if levels are high, low, this is high)
  pfun <- function(object, newdata) {
    predict(object, data = newdata)$predictions[, 1L]
  }
  
  ## pull model out of workflow
  best_workflow <- best_tidy_workflow %>%
    fit(train)
  best_workflow_mod <- workflows::extract_fit_parsnip(best_workflow)
  
  ## pull out data
  shap_data_train <- recipes::prep(dietML_recipe, train) %>% 
    recipes::juice() %>% 
    dplyr::select(-label) %>% 
    as.matrix()
  
  ## explain with fastshap
  shap_explainations_train <- fastshap::explain(best_workflow_mod$fit, X = shap_data_train, pred_wrapper = pfun, nsim = 10)

  ## make shap viz object
  sv_train <- shapviz::shapviz(shap_explainations_train, X = shap_data_train)
  
  ## make shap plot
  importance_plot_train_1 <- shapviz::sv_importance(sv_train, kind = "both", show_numbers = TRUE, bee_width = 0.2, max_display = 10, show_other = FALSE) + 
    ggtitle(label = paste0("SHAP: ", levels(as.factor(train$label))[1], " (train)"))
  ggplot2::ggsave(plot = importance_plot_train_1, filename = paste0(opt$outdir, "importance_plot_train_1.pdf"), width = 9, height = 4.5, units = "in")
  
                              ################
  
  ## Prediction wrapper: first level (ie if levels are high, low, this is high)
  pfun <- function(object, newdata) {
    predict(object, data = newdata)$predictions[, 2L]
  }
  
  ## pull model out of workflow
  best_workflow <- best_tidy_workflow %>%
    fit(train)
  best_workflow_mod <- workflows::extract_fit_parsnip(best_workflow)
  
  ## pull out data
  shap_data_train <- recipes::prep(dietML_recipe, train) %>% 
    recipes::juice() %>% 
    dplyr::select(-label) %>% 
    as.matrix()
  
  ## explain with fastshap
  shap_explainations_train <- fastshap::explain(best_workflow_mod$fit, X = shap_data_train, pred_wrapper = pfun, nsim = 10)
  
  ## make shap viz object
  sv_train <- shapviz::shapviz(shap_explainations_train, X = shap_data_train)
  
  ## make shap plot
  importance_plot_train_2 <- shapviz::sv_importance(sv_train, kind = "both", show_numbers = TRUE, bee_width = 0.2, max_display = 10, show_other = FALSE) + 
    ggtitle(label = paste0("SHAP: ", levels(as.factor(train$label))[2], " (train)"))
  ggplot2::ggsave(plot = importance_plot_train_2, filename = paste0(opt$outdir, "importance_plot_train_2.pdf"), width = 9, height = 4.5, units = "in")
  
                             ################
  
  ## Prediction wrapper: second level (ie if levels are high, low, this is low)
  pfun <- function(object, newdata) {
    predict(object, data = newdata)$predictions[, 2L]
  }
  
  ## pull model out of workflow
  best_workflow <- best_tidy_workflow %>%
    fit(test)
  best_workflow_mod <- workflows::extract_fit_parsnip(best_workflow)
  
  ## pull out data
  shap_data_test<- recipes::prep(dietML_recipe, test) %>% 
    recipes::juice() %>% 
    dplyr::select(-label) %>% 
    as.matrix()
  
  ## explain with fastshap
  shap_explainations_test <- fastshap::explain(best_workflow_mod$fit, X = shap_data_test, pred_wrapper = pfun, nsim = 10)
  
  ## make shap viz object
  sv_test <- shapviz::shapviz(shap_explainations_test, X = shap_data_test)
  
  ## make shap plot
  importance_plot_test_1 <- shapviz::sv_importance(sv_test, kind = "both", show_numbers = TRUE, bee_width = 0.2, max_display = 10, show_other = FALSE) + 
    ggtitle(label = paste0("SHAP: ", levels(as.factor(test$label))[1], " (test)"))
  ggplot2::ggsave(plot = importance_plot_test_1, filename = paste0(opt$outdir, "importance_plot_test_1.pdf"), width = 9, height = 4.5, units = "in")
  
  ################
  
  ## Prediction wrapper: second level (ie if levels are high, low, this is low)
  pfun <- function(object, newdata) {
    predict(object, data = newdata)$predictions[, 2L]
  }
  
  ## pull model out of workflow
  best_workflow <- best_tidy_workflow %>%
    fit(test)
  best_workflow_mod <- workflows::extract_fit_parsnip(best_workflow)
  
  ## pull out data
  shap_data_test<- recipes::prep(dietML_recipe, test) %>% 
    recipes::juice() %>% 
    dplyr::select(-label) %>% 
    as.matrix()
  
  ## explain with fastshap
  shap_explainations_test <- fastshap::explain(best_workflow_mod$fit, X = shap_data_test, pred_wrapper = pfun, nsim = 10)
  
  ## make shap viz object
  sv_test <- shapviz::shapviz(shap_explainations_test, X = shap_data_test)
  
  ## make shap plot
  importance_plot_test_2 <- shapviz::sv_importance(sv_test, kind = "both", show_numbers = TRUE, bee_width = 0.2, max_display = 10, show_other = FALSE) + 
    ggtitle(label = paste0("SHAP: ", levels(as.factor(test$label))[2], " (test)"))
  ggplot2::ggsave(plot = importance_plot_test_2, filename = paste0(opt$outdir, "importance_plot_test_2.pdf"), width = 9, height = 4.5, units = "in")
  
  }

}, error = function(e) {shap.error.occured <<- TRUE} )

if (shap.error.occured == TRUE) {
  cat("\n#########################\n")
  cat("ERROR: Could not complete SHAP anlaysis.", "\n")
  cat("#########################\n\n")
}
  
  
