#!/usr/bin/env Rscript

## SCRIPT: dietML_ranger.R ===================================================
## AUTHOR: Andrew Oliver
## DATE:   Jan, 9 2023
##
## PURPOSE: Provide ranger model to dietML

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
cat("Note: Beginning ML (", opt$type, ") using a ",opt$model," model...", "\n")
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
  pred <- predict(training_fit, test_data)
  print(caret::postResample(pred = pred, obs = test_label$label))
  
} else {
  ## error out
  stop("--type <classification/regression> not found.")
  
}
