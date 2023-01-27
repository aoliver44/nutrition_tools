#!/usr/bin/env Rscript

## SCRIPT: dietML.R ===================================================
## AUTHOR: Andrew Oliver
## DATE:   Nov, 1 2022
##
## PURPOSE: Run classification or regression ML
## the dietML.R script

## docker info =================================================================
#docker run --rm -it -v /Users/$USER/Downloads/nutrition_tools/:/home aoliver44/nutrition_tools:1.1 bash

## set working dir to /home for the docker container
setwd("/home")

## add commandline options =====================================================

library(docopt, quietly = T, verbose = F, warn.conflicts = F)
"Run regression or classification ML models on a dataframe
Usage:
    dietML [--label=<label> --cor_level=<cor_level> --train_split=<train_split> --model=<model> --type=<type> --seed=<seed> --tune_length=<tune_length> --ncores=<ncores>] <input> <outdir>
    
Options:
    -h --help  Show this screen.
    -v --version  Show version.
    --label=<label> name of column that you are prediction [default: label]
    --cor_level level to group features together [default: 0.80]
    --train_split what percentage of samples should be used in training [default: 0.70]
    --model what model would you like run (options: rf,lasso,ridge,enet) [default: rf]
    --type for models that do both regression and classification [default: classification]
    --seed set random seed [default: 42]
    --tune_length number of hyperparameter combinations to sample [default: 30]
    --ncores number of processesing cores for parallel computing [default: 2]
    
Arguments:
    input  FULL path to input file for ML (output from generic_combine.R)
    outdir FULL path where results should be written
" -> doc

opt <- docopt::docopt(doc, version = 'dietML.R v1.0\n\n')

## load libraries ==============================================================

library(readr, quietly = T, verbose = F, warn.conflicts = F)
library(dplyr, quietly = T, verbose = F, warn.conflicts = F)
library(ranger, quietly = T, verbose = F, warn.conflicts = F)
library(doParallel, quietly = T, verbose = F, warn.conflicts = F)
library(parallel, quietly = T, verbose = F, warn.conflicts = F)
library(mikropml, quietly = T, verbose = F, warn.conflicts = F)
library(lime, quietly = T, verbose = F, warn.conflicts = F)
library(cowplot, quietly = T, verbose = F, warn.conflicts = F)
library(purrr, quietly = T, verbose = F, warn.conflicts = F)
library(ggplot2, quietly = T, verbose = F, warn.conflicts = F)
library(fastshap, quietly = T, verbose = F, warn.conflicts = F)

## helper functions ============================================================
## Negate function ("not in"):
`%!in%` <- Negate(`%in%`)
models <- c("rf", "lasso", "ridge", "enet")
## suppress warnings
options(warn=-1)

# opt <- data.frame(cor_level=numeric(),
#                   label=character(),
#                   train_split=numeric(),
#                   seed=numeric(),
#                   model=character(),
#                   type=character(),
#                   ncores=numeric(),
#                   tune_length=numeric(),
#                   input=character(),
#                   outdir=character())
# opt <- opt %>% tibble::add_row(cor_level = 0.80, train_split= 0.7, model = "rf", seed= 42, ncores = 4, tune_length = 10, label = c("feature_of_interest"), type= c("classification"), input = c("/home/output_old/abx_cluster_bi_metaphlan4_all.txt"), outdir="/home/simulated_output/ml_results/")


## set seed  ===================================================================

set.seed(opt$seed)

## check for inputs ============================================================

## quietly check to make sure /scripts wasn't overwritten
if (file.exists("/scripts/models/dietML_ranger.R") == FALSE) {
  stop("It appears you bind mounted docker to a virtual directory named /scripts. We
       need to use that folder. Please restart the docker image and use a different
       virtual directory name.")
}

## check for outdir and make if not there
if (dir.exists(opt$outdir) == TRUE) {
  setwd(opt$outdir)
} else {
  dir.create(path = opt$outdir)
  setwd(opt$outdir)
}

cat("Checking for input file...", "\n\n")

## check for input and break if not found
if (file.exists(opt$input) == TRUE) { 
  cat("\n#########################\n")
  cat(paste0(opt$input), " is being used as input.", "\n")
  cat("#########################\n\n")
  
} else {
  stop("Input file not found.")
}

## read in input ===============================================================

if (strsplit(basename(opt$input), split="\\.")[[1]][2] == "csv") {
  input <- readr::read_delim(file = opt$input, delim = ",") %>% 
    janitor::clean_names() %>% tidyr::drop_na() %>% 
    dplyr::select(., -dplyr::any_of("subject_id")) %>%
    suppressMessages()
  
} else if (strsplit(basename(opt$input), split="\\.")[[1]][2] %in% c("tsv","txt")){
  input <- readr::read_delim(file = opt$input, delim = "\t") %>% 
    janitor::clean_names() %>% tidyr::drop_na() %>% 
    dplyr::select(., -dplyr::any_of("subject_id")) %>%
    suppressMessages()
  
}

## make colnames appropriate for ML (ranger is picky)
colnames(input) <- make.names(colnames(input))

## check for label
if (opt$label %in% colnames(input) == TRUE) {
  cat("\n#########################\n")
  cat(paste0(opt$label), " label is being used for ", paste0(opt$type), ".\n")
  cat("#########################\n\n")
  
  input <- input %>% dplyr::rename(., "label" = opt$label)
} else {
  stop(paste0(opt$label, " not found in input."))
}

if (opt$type == "classification") {
  if(length(levels(as.factor(input$label))) > 9)
  stop("You are trying to predict 10 or more classes. That is a bit much. Did you mean to do regression?")
}
## test train split ============================================================

cat("\n#########################\n")
cat("Note: Train test split at: ", opt$train_split, "of train data.", "\n")
cat("#########################\n\n")

## create a row id to train test split on
input$id_tmp <- seq(1:NROW(input))

## create training data 
train <- input %>% dplyr::sample_frac(as.numeric(opt$train_split))
train_data <- train %>% dplyr::select(., -label)
train_label <- train %>% dplyr::select(., label)

## create testing data
test  <- dplyr::anti_join(input, train, by = 'id_tmp')
test_data  <- test %>% dplyr::select(., -label)
test_label <- test %>% dplyr::select(., label)

## remove row id
train_data$id_tmp <- NULL
test_data$id_tmp <- NULL

## run chosen model ============================================================

## check if user input model
if (opt$model %!in% models) {
  cat("\n#########################\n")
  cat("ERROR: model not found", "\n")
  cat("Please choose one of the following models for --model ", "\n")
  print(as.data.frame(models))
  cat("#########################\n\n")
}

## random forest
if (opt$model %in% c("ranger", "rf", "randomforest")) {
  source("/scripts/models/dietML_ranger.R")
}

## lasso/ridge/elastic net models
if (opt$model %in% c("lasso", "ridge", "enet", "elasticnet")) {
  source("/scripts/models/dietML_glmnet.R")
}

## make training fit plots
pdf(file = paste0(opt$outdir, "training_fit.pdf"), width=7, height=5)
plot(training_fit, plotType = "line")
dev.off()

## VIP Plots ===================================================================
## For all:
vip <- caret::varImp(object = training_fit)
pdf(file = paste0(opt$outdir, "vip_plot.pdf"), width=15, height=5)
plot(vip, top = pmin(NROW(vip$importance), 20))
suppressMessages(dev.off())

## LIME explaination ===========================================================

lime.error.occured <- FALSE

tryCatch( { if (opt$type == "regression" && opt$model == "rf") {
  
  ## print message to user
  cat("\n#########################\n")
  cat("Running LIME Analysis on group: ", opt$label, "\n")
  cat("#########################\n\n")
  
  ## set up first part of LIME analysis. A good read on LIME can be
  ## found here: https://cran.r-project.org/web/packages/lime/vignettes/Understanding_lime.html
  explainer_caret <- lime::lime(train_data, training_fit, quantile_bins = TRUE)
  summary(explainer_caret)
  
  ## LIME seems to want both the test_data and label to be together
  testing_data = cbind(test_data, test_label)
  
  ## Second part of LIME, which is building models for each "case" (see above link)
  explanation_caret <- lime::explain(
    x = test_data, 
    explainer = explainer_caret, 
    n_permutations = 1000,
    dist_fun = "euclidean",
    n_features = 10, 
    feature_select = "auto",
    #n_labels = 2,
    labels = "feature_of_interest"
  )
  
  lime::plot_explanations(explanation_caret) %>%
    ggplot2::ggsave(filename = paste0(opt$outdir, "lime_heatmap.pdf"), width = 15, height = 7, units = "in")
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

## SHAP explanation ============================================================

shap.error.occured <- FALSE

tryCatch( { if (length(levels(as.factor(train_label$label))) == 2) {
  
  ## Prediction wrapper
  pfun <- function(object, newdata) {
    predict(object, data = newdata)$predictions[, 1L]
  }
  
  ## shap values on **TRAINING** data
  shap_train <- fastshap::explain(training_fit$finalModel, X = as.matrix(train_data), pred_wrapper = pfun, nsim = 10)
  
  ## add a row index in to merge with abundance data
  shap_train$index <- c(1:NROW(shap_train)) 
  
  ## melt the shap data
  shap_train_values <- reshape2::melt(shap_train, id.vars = "index") %>% dplyr::rename(., "SHAP" = "value") 
  
  ## find the features with the highest mean absolute shap values
  shap_train_values_high <- shap_train_values %>% dplyr::group_by(., variable) %>% dplyr::summarise(., mean_shap = mean(SHAP)) %>%
    dplyr::arrange(desc(abs(mean_shap))) %>% dplyr::slice(1:20) %>% dplyr::pull(variable) %>% droplevels()
  
  ## select those features out of the melted shap data
  shap_train_values <- shap_train_values %>% dplyr::filter(., variable %in% shap_train_values_high)
  
  ## get the training abundance data & melt the data & select features that appear in shap
  train_data$index <- c(1:NROW(train_data))
  train_data_melt <- reshape2::melt(train_data, id.vars = "index") %>% dplyr::filter(., variable %in% shap_train_values_high)
  
  ## merge shap and training melted data
  shap_train_plot_data <- merge(shap_train_values, train_data_melt, by = c("index", "variable"))
  
  shap_train_plot_data %>% 
    group_split(variable) %>% 
    purrr::map(
      ~ggplot(., aes(y = variable, x = SHAP, color = log(value + 1))) + 
        geom_point(size = 3, position = position_jitter(height = 0.2), aes(alpha = 0.7)) +
        viridis::scale_color_viridis(option = "C") + 
        facet_wrap(~ variable, ncol = 1, labeller = function(x) label_value(x, multi_line = FALSE)) +
        theme_bw() +
        theme(strip.background = element_blank(), strip.text.x = element_blank(), legend.position = "none", axis.title.x = element_blank(), panel.border = element_blank(), plot.margin = margin(0.1,0.5,0.1,1, "cm")) +
        labs(x = "", y = "")
    ) %>% 
    cowplot::plot_grid(plotlist = ., align = 'hv', ncol = 1) %>% 
    ggplot2::ggsave(filename = paste0(opt$outdir, "shap_train_plot.pdf"), width = 15, height = 10, units = "in")
  
  mean_plot_data_train <- shap_train[, colnames(shap_train) %!in% c("index")]
  shap_train_plot <- mean_plot_data_train %>%
    ggplot2::autoplot(type = "contribution") + 
    theme(axis.text.x = element_text(angle = 90)) + 
    ggtitle("SHAP values: Train Data") 
  ggplot2::ggsave(plot = shap_train_plot, filename = paste0(opt$outdir, "shap_train_mean_plot.pdf"), width = 15, height = 6, units = "in")
  
  
  ## shap values on **TESTING** data
  shap_test <- fastshap::explain(training_fit$finalModel, X = as.matrix(test_data), pred_wrapper = pfun, nsim = 10)
  
  ## add a row index in to merge with abundance data
  shap_test$index <- c(1:NROW(shap_test)) 
  
  ## melt the shap data
  shap_test_values <- reshape2::melt(shap_test, id.vars = "index") %>% dplyr::rename(., "SHAP" = "value") 
  
  ## find the features with the highest mean absolute shap values
  shap_test_values_high <- shap_test_values %>% dplyr::group_by(., variable) %>% dplyr::summarise(., mean_shap = mean(SHAP)) %>%
    dplyr::arrange(desc(abs(mean_shap))) %>% dplyr::slice(1:20) %>% dplyr::pull(variable) %>% droplevels()
  
  ## select those features out of the melted shap data
  shap_test_values <- shap_test_values %>% dplyr::filter(., variable %in% shap_test_values_high)
  
  ## get the testing abundance data & melt the data & select features that appear in shap
  test_data$index <- c(1:NROW(test_data))
  test_data_melt <- reshape2::melt(test_data, id.vars = "index") %>% dplyr::filter(., variable %in% shap_test_values_high)
  
  ## merge shap and testing melted data
  shap_test_plot_data <- merge(shap_test_values, test_data_melt, by = c("index", "variable"))
  
  shap_test_plot_data %>% 
    group_split(variable) %>% 
    purrr::map(
      ~ggplot(., aes(y = variable, x = SHAP, color = log(value + 1))) + 
        geom_point(size = 3, position = position_jitter(height = 0.2), aes(alpha = 0.7)) +
        viridis::scale_color_viridis(option = "C") + 
        facet_wrap(~ variable, ncol = 1, labeller = function(x) label_value(x, multi_line = FALSE)) +
        theme_bw() +
        theme(strip.background = element_blank(), strip.text.x = element_blank(), legend.position = "none", axis.title.x = element_blank(), panel.border = element_blank(), plot.margin = margin(0.1,0.5,0.1,1, "cm")) +
        labs(x = "", y = "")
    ) %>% 
    cowplot::plot_grid(plotlist = ., align = 'hv', ncol = 1) %>% 
    ggplot2::ggsave(filename = paste0(opt$outdir, "shap_test_plot.pdf"), width = 15, height = 10, units = "in")
  
  mean_plot_data_test <- shap_test[, colnames(shap_test) %!in% c("index")]
  shap_test_plot <- mean_plot_data_test %>%
    ggplot2::autoplot(type = "contribution") + 
    theme(axis.text.x = element_text(angle = 90)) + 
    ggtitle("SHAP values: Test Data") 
  ggplot2::ggsave(plot = shap_test_plot, filename = paste0(opt$outdir, "shap_test_mean_plot.pdf"), width = 15, height = 6, units = "in")
  
  }
} , error = function(e) {shap.error.occured <<- TRUE})

if (shap.error.occured == TRUE) {
  cat("\n#########################\n")
  cat("ERROR: Could not complete SHAP anlaysis.", "\n")
  cat("#########################\n\n")
}






## Done ========================================================================

save.image(file = paste0(opt$outdir, "ML_r_workspace.rds"))

cat("\n#########################\n")
cat("Done! Results written to outdir.", "\n")
cat("#########################\n\n")