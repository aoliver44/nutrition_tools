#!/usr/bin/env Rscript
## v0.3.0a.8

## SCRIPT: shap_figures.R ======================================================
## AUTHOR: Andrew Oliver
## DATE:   Jan, 30 2023
##
## PURPOSE: lime and viz for tidymodels

lime.error.occured <- FALSE

tryCatch( { if (opt$type == "regression" && opt$model == "rf") {
  
  ## print message to user
  cat("\n#########################\n")
  cat("Running LIME Analysis on group: ", opt$label, "\n")
  cat("#########################\n\n")
  
  ## pull model out of workflow
  best_workflow <- best_tidy_workflow %>%
    fit(train)
  best_workflow_mod <- workflows::pull_workflow_fit(best_workflow)
  
  ## set up first part of LIME analysis. A good read on LIME can be
  ## found here: https://cran.r-project.org/web/packages/lime/vignettes/Understanding_lime.html
  explainer_caret <- lime::lime((train %>% dplyr::select(-label)), best_workflow_mod$fit, quantile_bins = TRUE)
  summary(explainer_caret)

  ## Second part of LIME, which is building models for each "case" (see above link)
  explanation_caret <- lime::explain(
    x = (test %>% dplyr::select(-label)), 
    explainer = explainer_caret, 
    n_permutations = 1000,
    dist_fun = "euclidean",
    n_features = 10, 
    feature_select = "auto",
    #n_labels = 2,
    labels = "feature_of_interest"
  )
  
  lime::plot_explanations(explanation_caret) %>%
    ggplot2::ggsave(filename = paste0(opt$outdir, "lime_heatmap_test.pdf"), height = n_distinct(explanation_caret$feature_desc)*.15, width = n_distinct(explanation_caret$case)*.25, units = "in")
  suppressMessages(dev.off())
  
}
  
  ## do the same above but only for binary classification, not regression
  if (length(levels(as.factor(train_label$label))) == 2) {
    
    cat("\n#########################\n")
    cat("Running LIME Analysis on group: ", levels(as.factor(train_label$label))[1], "\n")
    cat("#########################\n\n")
    
    ## no "bins" needed for binary classification
    explainer_caret <- lime::lime(train_data, training_fit)
    summary(explainer_caret)
    
    testing_data = cbind(test_data, test_label)
    
    explanation_caret <- lime::explain(
      x = test_data, 
      explainer = explainer_caret, 
      n_permutations = 1000,
      dist_fun = "euclidean",
      n_features = 10, 
      feature_select = "auto",
      #n_labels = 2,
      ## this needs a label to focus on, so i just take the one that is alphabetically first
      labels = levels(as.factor(train_label$label))[1]
    )
    
    lime::plot_explanations(explanation_caret) %>%
      ggplot2::ggsave(filename = paste0(opt$outdir, "lime_heatmap.pdf"), width = 15, height = 7, units = "in")
    suppressMessages(dev.off())
  }
} , error = function(e) {lime.error.occured <<- TRUE})

if (lime.error.occured == TRUE) {
  cat("\n#########################\n")
  cat("ERROR: Could not complete LIME anlaysis.", "\n")
  cat("#########################\n\n")
}