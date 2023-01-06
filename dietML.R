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
library(mikropml, quietly = T, verbose = F, warn.conflicts = F)

## helper functions ============================================================
## Negate function ("not in"):
`%!in%` <- Negate(`%in%`)

## suppress warnings
options(warn=-1)

# opt <- data.frame(cor_level=numeric(),
#                   label=character(),
#                   train_split=numeric(),
#                   seed=numeric(),
#                   type=character(),
#                   ncores=numeric(),
#                   tune_length=numeric(),
#                   input=character(),
#                   outdir=character())
# opt <- opt %>% tibble::add_row(cor_level = 0.80, train_split= 0.7, seed= 42, ncores = 4, tune_length = 100, label = c("feature_of_interest"), type= c("regression"), input = c("/home/output_old/butyrate_metaphlan4.txt"), outdir="/home/simulated_output/ml_results/")


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

## set seed  ===================================================================

set.seed(opt$seed)

## Determine if your data is highly correlated =================================

## This is necessary because if, during preprocessing, you lose a lot of samples
## this can cause weird behavior in hyperparameter searches. We are gonna
## hedge agaist this.

## co-correlate features at specified threshold
training_cor <- mikropml:::group_correlated_features(train_data, 
                                                 corr_thresh = as.numeric(opt$cor_level), group_neg_corr = T)

## make dataframe of what is correlated at specified threshold.
training_cor <- as.data.frame(training_cor) %>% 
  tidyr::separate(., col = training_cor, into = c("keep", "co_correlated"), sep = "\\|", extra = "merge")

## if we think a lot of samples (greater than 10% of all samples) will get 
## dropped due to pre-processing steps, especially correlation, then we 
## constrict the search space so that ranger doesnt error out

## if you are dropping a lot due to correlation
if (length(na.omit(training_cor$co_correlated)) > (NROW(training_cor) * 0.1)) {
  if (opt$type == "classification") {
    tuneGrid <-  expand.grid(.mtry = 2:((round((NROW(training_cor) * 0.9), digits = 0)) - (length(na.omit(training_cor$co_correlated)))),
                             .splitrule = c("gini", "extratrees"),
                             .min.node.size = 2:(min(20,(round((NROW(training_cor) * 0.9), digits = 0)) - (length(na.omit(training_cor$co_correlated))))))
    
    tuneGrid <- sample_n(tuneGrid, pmin(as.numeric(opt$tune_length), NROW(tuneGrid)))
  } else {
    
    tuneGrid <-  expand.grid(.mtry = 2:((round((NROW(training_cor) * 0.9), digits = 0)) - (length(na.omit(training_cor$co_correlated)))),
                             .splitrule = c("variance", "extratrees", "maxstat"),
                             .min.node.size = 2:(min(20,(round((NROW(training_cor) * 0.9), digits = 0)) - (length(na.omit(training_cor$co_correlated))))))
    
    tuneGrid <- sample_n(tuneGrid, pmin(as.numeric(opt$tune_length), NROW(tuneGrid)))
    
  }
  
  ## else you are not dropping a lot due to correlation
} else {
  if (opt$type == "classification") {
  tuneGrid <-  expand.grid(.mtry = 2:(round((NROW(training_cor) * 0.9), digits = 0)),
                           .splitrule = c("gini", "extratrees"),
                           .min.node.size = 2:(min(20,round((NROW(training_cor) * 0.9), digits = 0))))
  
  tuneGrid <- sample_n(tuneGrid, pmin(as.numeric(opt$tune_length), NROW(tuneGrid)))
  } else {
    
    tuneGrid <-  expand.grid(.mtry = 2:(round((NROW(training_cor) * 0.9), digits = 0)),
                             .splitrule = c("variance", "extratrees", "maxstat"),
                             .min.node.size = 2:(min(20,round((NROW(training_cor) * 0.9), digits = 0))))
    
    tuneGrid <- sample_n(tuneGrid, pmin(as.numeric(opt$tune_length), NROW(tuneGrid)))
    
  }
}


## CV on training data =========================================================

cat("\n#########################\n")
cat("Note: Beginning ML (", opt$type, ") ...", "\n")
cat("Preprocessesing includes near-zero variance filter and correlation threshold at ", opt$cor_level, "pearson.", "\n")
cat("#########################\n\n")

## GRID SEARCH (of randomly defined grid)

fit_control <- caret::trainControl(method = "repeatedcv",
                                   number = 10,
                                   repeats = 3,
                                   preProcOptions = list(cutoff = as.numeric(opt$cor_level)),
                                   classProbs = TRUE,
                                   search = "grid",
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
                               tuneGrid = tuneGrid,
                               importance = "permutation"
  )
  ## stop parallel jobs
  parallel::stopCluster(cl)
  
  print(caret::confusionMatrix(data = predict(training_fit, test_data), reference = as.factor(test_label$label)))
  
  if (length(levels(as.factor(train_label$label))) == 2) {
    ## run MLeval
    pdf(file = paste0(opt$outdir, "roc_auc_curve.pdf"), width=5, height=5)
    res <- MLeval::evalm(training_fit, plots = "r")
    suppressMessages(dev.off())
    
  }
} else if (opt$type == "regression") {
  
  ## build models
  training_fit <- caret::train(x = train_data, y = as.numeric(train_label$label), 
                               preProcess = c("nzv","corr"),
                               method = "ranger", 
                               trControl = fit_control, 
                               tuneGrid = tuneGrid,
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
pdf(file = paste0(opt$outdir, "training_fit.pdf"), width=7, height=5)
plot(training_fit, plotType = "line")
dev.off()

cat("\n#########################\n")
cat("Done! Results written to outdir.", "\n")
cat("#########################\n\n")


## VIP Plots ===================================================================
## For all:
vip <- caret::varImp(object = training_fit)
pdf(file = paste0(opt$outdir, "vip_plot.pdf"), width=15, height=5)
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