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
"Run random forest regression or classification on a dataframe
Usage:
    dietML [--label=<label> --cor_level=<cor_level> --train_split=<train_split> --type=<type> --seed=<seed> --tune_length=<tune_length> --ncores=<ncores>] <input> <outdir>
    
Options:
    -h --help  Show this screen.
    -v --version  Show version.
    --label=<label> name of column that you are prediction [default: label]
    --cor_level level to group features together [default: 0.80]
    --train_split what percentage of samples should be used in training [default: 0.70]
    --type are you trying to do classification (discrete levels of label) or regression (continous) [default: classification]
    --seed random seed for reproducible results [default: 42]
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

## helper functions ============================================================
## Negate function ("not in"):
`%!in%` <- Negate(`%in%`)

## suppress warnings
options(warn=-1)

# opt <- data.frame(subject_identifier=character(),
#                   cor_level=numeric(),
#                   label=character(),
#                   train_split=numeric(),
#                   seed=numeric(),
#                   type=character(),
#                   ncores=numeric(),
#                   input=character(),
#                   outdir=character())
# opt <- opt %>% tibble::add_row(subject_identifier = "subject_id", cor_level = 0.80, train_split= 0.7, seed= 42, ncores = 4, label = c("species"), type= c("classification"), input = c("/home/simulated_test/iris.csv"), outdir="/home/simulated_test/ml_results/")


## check for inputs ============================================================

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

input <- readr::read_delim(file = opt$input, delim = ",") %>% 
  janitor::clean_names() %>% tidyr::drop_na() %>% 
  dplyr::select(., -dplyr::any_of("subject_id")) %>%
  suppressMessages()

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

## test train split ============================================================

cat("\n#########################\n")
cat("Note: Train test split at: ", opt$train_split, "% of train data.", "\n")
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

## set seed for reproducibility ================================================

#create a list of seed, here change the seed for each resampling
set.seed(opt$seed)

#length is = (n_repeats*nresampling)+1
seeds <- vector(mode = "list", length = 31)

#(3 is the number of tuning parameter, mtry for rf, here equal to ncol(iris)-2)
for(i in 1:30) seeds[[i]]<- sample.int(n=1000, 132)

#for the last model
seeds[[31]]<-sample.int(NROW(train_data), 1)

## CV on training data =========================================================

cat("\n#########################\n")
cat("Note: Beginning ML (", opt$type, ") ...", "\n")
cat("Preprocessesing includes near-zero variance filter and correlation threshold at ", opt$cor_level, "pearson.", "\n")
cat("#########################\n\n")

## GRID SEARCH

# ## create hyper parameter grid and train control 
# # tuneGrid <-  expand.grid(.mtry = 2:pmin(NCOL(train_data), 12),
# #                          .splitrule = c("gini", "extratrees"),
# #                          .min.node.size = seq(2, pmin(NCOL(train_data), 12), by = 2))
# 
# fit_control <- caret::trainControl(method = "repeatedcv",
#                                    number = 10,
#                                    repeats = 3,
#                                    preProcOptions = list(cutoff = as.numeric(opt$cor_level)),
#                                    classProbs = TRUE,
#                                    search = "grid",
#                                    seeds=seeds,
#                                    savePredictions = T,
#                                    allowParallel = TRUE,
#                                    verboseIter = TRUE)

## RANDOM SEARCH

fit_control <- caret::trainControl(method = "repeatedcv",
                                   number = 10,
                                   repeats = 3,
                                   preProcOptions = list(cutoff = as.numeric(opt$cor_level)),
                                   classProbs = TRUE,
                                   search = "random",
                                   savePredictions = T,
                                   allowParallel = TRUE,
                                   verboseIter = TRUE)


## make parallel jobs for model training
cl <- parallel::makePSOCKcluster(as.numeric(opt$ncores))
doParallel::registerDoParallel(cl)

if (opt$type == "classification") {
  
  ## build models
  training_fit <- caret::train(x = train_data, y = as.factor(train_label$label), 
                               preProcess = c("nzv","corr"),
                               method = "ranger", 
                               trControl = fit_control, 
                               #tuneGrid = tuneGrid,
                               tuneLength = opt$tune_length,
                               importance = "permutation"
  )
  ## stop parallel jobs
  parallel::stopCluster(cl)
  
  print(caret::confusionMatrix(data = predict(training_fit, test_data), reference = as.factor(test_label$label)))
  
  if (length(levels(as.factor(train_label$label))) == 2) {
    ## run MLeval
    png(filename = paste0(opt$outdir, "roc_auc_curve.png"), width=5, height=5, units="in", res=300)
    res <- MLeval::evalm(training_fit, plots = "r")
    suppressMessages(dev.off())
    
  }
} else if (opt$type == "regression") {
  
  ## build models
  training_fit <- caret::train(x = train_data, y = as.numeric(train_label$label), 
                               preProcess = c("nzv","corr"),
                               method = "ranger", 
                               trControl = fit_control, 
                               #tuneGrid = tuneGrid,
                               tuneLength = opt$tune_length,
                               importance = "permutation"
  )
  ## stop parallel jobs
  parallel::stopCluster(cl)
  
  ## For regression:
  hfe_pred <- predict(training_fit, test_data)
  caret::postResample(pred = hfe_pred, obs = test_label$label)
  
} else {
  ## error out
  stop("--type <classification/regression> not found.")
  
}

## make training fit plots
png(filename = paste0(opt$outdir, "training_fit.png"), width=7, height=5, units="in", res=300, type = "cairo")
plot(training_fit, plotType = "line")
dev.off()

cat("\n#########################\n")
cat("Done! Results written to outdir.", "\n")
cat("#########################\n\n")


## VIP Plots ===================================================================
## For all:
vip <- caret::varImp(object = training_fit)
png(filename = paste0(opt$outdir, "vip_plot.png"), width=7, height=5, units="in", res=300)
plot(vip, top = pmin(NROW(vip$importance), 20))
suppressMessages(dev.off())

## shap explaination ===========================================================

# explainer <- shapr::shapr(train_data, training_fit$finalModel, n_combinations = 10000)
# explanation_largesigma <- shapr::explain(test_data, explainer, approach = "empirical", prediction_zero = p)
# plot(explanation_largesigma)
# 
# #create a list of seed, here change the seed for each resampling
# set.seed(123)
# 
# #length is = (n_repeats*nresampling)+1
# seeds <- vector(mode = "list", length = 11)
# 
# #(3 is the number of tuning parameter, mtry for rf, here equal to ncol(iris)-2)
# for(i in 1:10) seeds[[i]]<- sample.int(n=1000, 3)
# 
# #for the last model
# seeds[[11]]<-sample.int(1000, 1)
# length(seeds[[10]])