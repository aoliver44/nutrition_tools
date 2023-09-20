#!/usr/bin/env Rscript

## SCRIPT: dietML.R ===================================================
## AUTHOR: Andrew Oliver
## DATE:   Nov, 1 2022
##
## PURPOSE: Run classification or regression ML
## the dietML.R script

## docker info =================================================================
#docker run --rm -it -v /Users/$USER/Downloads/nutrition_tools/:/home aoliver44/nutrition_tools:1.1 bash

## suppress warnings
options(warn=-1)

## set working dir to /home for the docker container
setwd("/home")

## add commandline options =====================================================

library(docopt, quietly = T, verbose = F, warn.conflicts = F)
"Run regression or classification ML models on a dataframe
Usage:
    dietML [--subject_identifier=<subject_id> --label=<label> --cor_level=<cor_level> --train_split=<train_split> --model=<model> --metric=<metric> --folds=<folds> --type=<type> --seed=<seed> --tune_length=<tune_length> --tune_stop=<tune_stop> --tune_time=<time_limit> --shap=<shap> --ncores=<ncores>] <input> <outdir>
    
Options:
    -h --help  Show this screen.
    -v --version  Show version.
    --subject_identifier name of columns with subject IDs [default: subject_id]
    --label name of column that you are prediction [default: label]
    --cor_level level to group features together [default: 0.95]
    --train_split what percentage of samples should be used in training 
            [default: 0.70]
    --model what model would you like run 
            (options: rf,lasso,ridge,enet) [default: rf]
    --folds number of CV folds to tune with [default: 10]
    --metric what metric would you like to optimize in training 
            (options: roc_auc, bal_accuracy, accuracy, mae, rmse, rsq, kap, 
             f_meas, ccc) [default: bal_accuracy]
    --type for models that do both regression and classification 
            [default: classification]
    --seed set random seed [default: 42]
    --tune_length number of hyperparameter combinations to sample [default: 80]
    --tune_time length of time tune_bayes runs [default: 10]
    --tune_stop number of HP interations to let pass without a metric 
            improvement [default: 10]
    --shap attempt to calcualte shap values? [default: FALSE]
    --ncores number of processesing cores for parallel computing [default: 2]
    
Arguments:
    input  FULL path to input file for ML (output from generic_combine.R)
    outdir FULL path where results should be written
" -> doc

opt <- docopt::docopt(doc, version = 'dietML.R v0.3.0a.6\n\n')

## load libraries ==============================================================
library(readr, quietly = T, verbose = F, warn.conflicts = F)
library(dplyr, quietly = T, verbose = F, warn.conflicts = F)
library(ranger, quietly = T, verbose = F, warn.conflicts = F)
library(doParallel, quietly = T, verbose = F, warn.conflicts = F)
library(parallel, quietly = T, verbose = F, warn.conflicts = F)
library(mikropml, quietly = T, verbose = F, warn.conflicts = F)
library(lime, quietly = T, verbose = F, warn.conflicts = F)
library(ggplot2, quietly = T, verbose = F, warn.conflicts = F)
library(fastshap, quietly = T, verbose = F, warn.conflicts = F)

## helper functions ============================================================
## Negate function ("not in"):
`%!in%` <- Negate(`%in%`)

## unregister hung-up parallel jobs
unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}

## list of supported models
models <- c("rf", "lasso", "ridge", "enet")

## TEST ARGUMENTS ==============================================================

# opt <- data.frame(cor_level=numeric(),
#                   subject_identifier=character(),
#                   label=character(),
#                   train_split=numeric(),
#                   seed=numeric(),
#                   model=character(),
#                   folds=numeric(),
#                   metric=character(),
#                   type=character(),
#                   ncores=numeric(),
#                   tune_time=numeric(),
#                   tune_length=numeric(),
#                   tune_stop=numeric(),
#                   shap=character(),
#                   input=character(),
#                   outdir=character())
# opt <- opt %>% tibble::add_row(subject_identifier = "subject_id",
#                                cor_level = 0.95,
#                                train_split= 0.7,
#                                model = "rf",
#                                metric = "bal_accuracy",
#                                seed= 42,
#                                ncores = 4,
#                                folds = 10,
#                                tune_length = 50,
#                                tune_time = 10,
#                                tune_stop = 10,
#                                shap = "FALSE",
#                                label = c("cluster"),
#                                type= c("classification"),
#                                input = c("/home/nutrition_tools/ultra_merge/output/diet-life_input_bi.csv"),
#                                outdir= c("/home/output_old/")
#                                )


## set seed  ===================================================================

set.seed(as.numeric(opt$seed))

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


## check for input and break if not found
if (file.exists(opt$input) == FALSE) { 
  stop("Input file not found.\n")
}

## read in input ===============================================================

if (strsplit(basename(opt$input), split="\\.")[[1]][2] == "csv") {
  input <- readr::read_delim(file = opt$input, delim = ",") %>% 
    janitor::clean_names() %>% tidyr::drop_na() %>% 
    #dplyr::select(., -dplyr::any_of("subject_id")) %>%
    suppressMessages()
  
} else if (strsplit(basename(opt$input), split="\\.")[[1]][2] %in% c("tsv","txt")){
  input <- readr::read_delim(file = opt$input, delim = "\t") %>% 
    janitor::clean_names() %>% tidyr::drop_na() %>% 
    #dplyr::select(., -dplyr::any_of("subject_id")) %>%
    suppressMessages()
  
}

## format input ================================================================

## make colnames appropriate for ML (ranger is picky)
colnames(input) <- make.names(colnames(input))

## shuffle data row-wise
input <- input[sample(nrow(input)),]

## shuffle data col-wise
input <- input[,sample(ncol(input))]

## check for label
if (opt$label %in% colnames(input) == TRUE) {
  ## rename specified label to "label"
  input <- input %>% dplyr::rename(., "label" = opt$label)
} else {
  stop(paste0(opt$label, " not found in input."))
}

## check if classification was mis-specified
if (opt$type == "classification") {
  if(length(levels(as.factor(input$label))) > 9)
  stop("You are trying to predict 10 or more classes. That is a bit much. Did you mean to do regression?")
}

## output all the parameters used ==============================================

cat("\n#########################\n")
cat("         DietML", "\n")
cat("#########################\n\n")

cat("Parameters specified: ", "\n\n")

cat(paste0("Input: ", opt$input, "\n"))
cat(paste0("Outdir: ", opt$outdir, "\n\n"))
cat(paste0("subject_identifier: ", opt$subject_identifier, "\n"))
cat(paste0("Label: ", opt$label, "\n"))
cat(paste0("Preprocessing correlation threshold: ", opt$cor_level, "\n"))
cat(paste0("Train-Test Split: ", opt$train_split, "\n"))
cat(paste0("Model: ", opt$model, "\n"))
cat(paste0("Type: ", opt$type, "\n"))
cat(paste0("Number of folds: ", opt$folds, "\n"))
cat(paste0("Metric optimized: ", opt$metric, "\n"))
cat(paste0("Number* of HP combinations to test: ", opt$tune_length, "\n"))
cat(paste0("Tune time limit: ", opt$tune_time, "\n"))
cat(paste0("Attempt to calculate SHAP: ", opt$shap, "\n"))
cat(paste0("Number of cores: ", opt$ncores, "\n"))
cat(paste0("Random seed: ", opt$seed, "\n"))
cat(paste0("*will prematurely end if metric is not optimized in ", opt$tune_stop," iterations\n"))

## run null (dummy) model ======================================================

cat("\n#########################\n")
cat("Running null model...", "\n")
cat("#########################\n\n")

source("/scripts/models/dietML_null_tidy.R")

## run chosen model ============================================================

## check if user input model
if (opt$model %!in% models) {
  cat("\n#########################\n")
  cat("ERROR: model not found", "\n")
  cat("Please choose one of the following models for --model ", "\n")
  print(as.data.frame(models))
  cat("#########################\n\n")
}

cat("\n#########################\n")
cat("Running model...", "\n")
cat("#########################\n\n")

## random forest
if (opt$model %in% c("ranger", "rf", "randomforest")) {
  source("/scripts/models/dietML_ranger_tidy.R")
}

## lasso/ridge models
if (opt$model %in% c("lasso", "ridge")) {
    source("/scripts/models/dietML_glmnet_tidy_ridge_lasso.R")
} 


## elastic net models
if (opt$model %in% c("enet", "elasticnet")) {
    source("/scripts/models/dietML_glmnet_tidy_enet.R")
} 

## VIP Plots ===================================================================
## For all:
# vip <- caret::varImp(object = training_fit)
# pdf(file = paste0(opt$outdir, "vip_plot.pdf"), width=15, height=5)
# plot(vip, top = pmin(NROW(vip$importance), 20))
# suppressMessages(dev.off())

## LIME explanation ===========================================================

#source("/scripts/utilities/lime_figures.R")

## SHAP explanation ============================================================

if (opt$shap == TRUE) {
  
  cat("\n#########################\n")
  cat("Calculating SHAP...", "\n")
  cat("#########################\n\n")
  
  source("/scripts/utilities/shap_figures.R")
}


## Done ========================================================================

save.image(file = paste0(opt$outdir, "ML_r_workspace.rds"))

cat("\n#########################\n")
cat("Done! Results written to outdir.", "\n")
cat("#########################\n\n")
