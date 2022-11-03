#!/usr/bin/env Rscript 

## SCRIPT: dietML.R ===================================================
## AUTHOR: Andrew Oliver
## DATE:   Nov, 1 2022
##
## PURPOSE: Run classification or regression ML
## the dietML.R script

## docker info =================================================================

## docker command:
#docker run --rm -it -p 8787:8787 
#-e PASSWORD=yourpasswordhere 
#-v /Users/andrew.oliver/Documents/active_projects_github-USDA/nutrition_tools/:/home 
#amr_r_env:3.1.0

## general command:
## 

## set working dir to /home for the docker container
setwd("/home")


## add commandline options =====================================================

library(docopt)
"Run random forest regression or classification on a dataframe
Usage:
    dietML.R [--label=<label> --cor_level=<cor_level> --train_split=<train_split> --type=<type>] <input> <output>
    
Options:
    -h --help  Show this screen.
    -v --version  Show version.
    --label=<label> name of column that you are prediction [default: diet_feature]
    --cor_level level to group features together [default: 0.85]
    --train_split what percentage of samples should be used in training [default: 0.70]
    --type are you trying to do classification (discrete levels of label) or regression (continous) [default: classification]
    
Arguments:
    input  path to input file for ML
    output path where results should be written
" -> doc

opt <- docopt::docopt(doc, version = 'dietML.R v1.0\n\n')

## load libraries ==============================================================

library(readr)
library(dplyr)

## set random seed if needed
set.seed(1)

## helper functions ============================================================

## Negate function ("not in"):
`%!in%` <- Negate(`%in%`)

opt <- data.frame(subject_identifier=character(),
                  cor_level=numeric(),
                  label=character(),
                  train_split=numeric(),
                  input=character(),
                  type=character(),
                  output=character())
opt <- opt %>% tibble::add_row(subject_identifier = "subject_id", cor_level = 0.80, train_split= 0.7, label = c("cluster"), type= c("classification"), input = c("/home/output_tmp/pipeline_nutrition_test.csv"), output="/home/output_tmp/ml_results/")


## check for inputs ============================================================

## check for output dir and make if not there
if (dir.exists(opt$output) == TRUE) {
  setwd(opt$output)
} else {
  dir.create(path = opt$output)
  setwd(opt$output)
}

cat("Checking for input file...", "\n\n")

## check for input and break if not found
if (file.exists(opt$input) == TRUE) { 
  cat(paste0(opt$input), " is being used as input.", "\n\n")
} else {
  stop("Input file not found.")
}

## read in input ===============================================================

input <- readr::read_csv(file = opt$input) %>% janitor::clean_names()
colnames(input) <- make.names(colnames(input))

input$id_tmp <- seq(1:NROW(input))

## check for label
if (opt$label %in% colnames(input) == TRUE) {
  cat(paste0(opt$label), " label is being used for ", paste0(opt$type), ".\n\n")
  input <- input %>% dplyr::rename(., "label" = opt$label)
} else {
  stop(paste0(opt$label, " not found in input."))
}

## test train split ============================================================

train <- input %>% dplyr::sample_frac(as.numeric(opt$train_split))
train_data <- train %>% dplyr::select(., -label)
train_label <- train %>% dplyr::select(., label)
test  <- dplyr::anti_join(input, train, by = 'id_tmp')
test_data  <- test %>% dplyr::select(., -label)
test_label <- test %>% dplyr::select(., label)
input$id_tmp <- NULL

## CV on training data =========================================================

tuneGrid <-  expand.grid(.mtry = 2:8, 
                         .splitrule = c("gini", "extratrees"), 
                         .min.node.size = c(5, 10, 15, 20, 25))

fit_control <- caret::trainControl(method = "oob", 
                                   preProcOptions = list(cutoff = 0.85), 
                                   #classProbs = TRUE,
                                   search = "grid")

training_fit <- caret::train(x = train_data, y = as.factor(train_label$label), 
                             method = "ranger", 
                             trControl = fit_control, 
                             #metric = "ROC", 
                             preProcess = c("nzv", "corr"),
                             tuneGrid = tuneGrid)

caret::plot.train(training_fit)
confusionMatrix(data = predict(training_fit, test_data), reference = as.factor(test_label$label))
