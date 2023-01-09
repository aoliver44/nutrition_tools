#!/usr/bin/env Rscript

## SCRIPT: dietML_glmnet.R ===================================================
## AUTHOR: Andrew Oliver
## DATE:   Jan, 9 2023
##
## PURPOSE: Provide lasso,ridge,enet model to dietML

## set up train control ========================================================

fit_control <- caret::trainControl(method = "repeatedcv",
                                   number = 10,
                                   repeats = 3,
                                   preProcOptions = list(cutoff = as.numeric(opt$cor_level)),
                                   classProbs = TRUE,
                                   search = "grid",
                                   savePredictions = T,
                                   allowParallel = TRUE,
                                   verboseIter = TRUE)

## set up regression vs classification =========================================

cat("\n#########################\n")
cat("Note: Beginning ML (", opt$type, ") using a ",opt$model," model...", "\n")
cat("Preprocessesing includes near-zero variance filter and correlation threshold at ", opt$cor_level, "pearson.", "\n")
cat("#########################\n\n")


if (opt$type == "classification") {
  if (length(levels(as.factor(train_label$label))) == 2) {
    family = "binomial"
    cv_scorer = "auc"
    label = as.factor(train_label$label)
  } else {
    
    family = "multinomial"
    cv_scorer = "class"
  }
} 

if (opt$type == "regression") {
  family = "gaussian"
  cv_scorer = "mae"
  label = as.numeric(train_label$label)
}

## elastic net =================================================================

if (opt$model == "enet") {
  
  ## get a reasonable list of lambda values to test
  x = as.matrix(train_data)
  y = unlist(train_label)
  lambda = c()
  for (alpha in seq(0.2,0.8,0.1)) {
    cv_fit <- glmnet::cv.glmnet(x = x, y = y, alpha = alpha, nfolds = 10, type.measure = cv_scorer, family = family)
    lambda = append(lambda, cv_fit$lambda)
  }
  
  ## make the caret tuning grid (pseudo random search)
  ## search through reasonable lambda list and different alpha parameters
  ## alpha is the "mixing paramenter" of ridge and lasso
  tuneGrid <-  expand.grid(alpha = seq(0.2,0.8,0.1), lambda = as.numeric(lambda))
  tuneGrid <- sample_n(tuneGrid, pmin(as.numeric(opt$tune_length), NROW(tuneGrid)))
  
  ## set up parallel
  cl <- parallel::makePSOCKcluster(as.numeric(opt$ncores))
  doParallel::registerDoParallel(cl)
  
  ## run elastic net model
  training_fit <- caret::train(x = train_data, y = label, 
                               preProcess = c("nzv","corr"),
                               method = "glmnet", 
                               trControl = fit_control, 
                               tuneGrid = tuneGrid,
                               importance = "permutation")
  
  parallel::stopCluster(cl)
  plot(training_fit, plotType = "line")
  
}

## lasso =======================================================================

if (opt$model == "lasso") {
  
  ## get a reasonable list of lambda values to test
  x = as.matrix(train_data)
  y = unlist(train_label)
  lambda = c()
  for (seed in sample(1:1000,7)) {
    set.seed(seed)
    cv_fit <- glmnet::cv.glmnet(x = x, y = y, alpha = 1, nfolds = 10, type.measure = cv_scorer, family = family)
    lambda = append(lambda, cv_fit$lambda)
  }
  
  ## make the caret tuning grid (pseudo random search)
  ## search through reasonable lambda list and different alpha parameters
  ## alpha is the "mixing paramenter" of ridge and lasso
  tuneGrid <-  expand.grid(alpha = 1, lambda = as.numeric(lambda))
  tuneGrid <- sample_n(tuneGrid, pmin(as.numeric(opt$tune_length), NROW(tuneGrid)))
  
  ## set up parallel
  cl <- parallel::makePSOCKcluster(as.numeric(opt$ncores))
  doParallel::registerDoParallel(cl)
  
  ## reset seed to user defined seed
  set.seed(opt$seed)
  
  ## run lasso model
  training_fit <- caret::train(x = train_data, y = label, 
                               preProcess = c("nzv","corr"),
                               method = "glmnet", 
                               trControl = fit_control, 
                               tuneGrid = tuneGrid,
                               importance = "permutation")
  
  parallel::stopCluster(cl)
  plot(training_fit, plotType = "line")
  
}

## ridge =======================================================================

if (opt$model == "ridge") {
  
  ## get a reasonable list of lambda values to test
  x = as.matrix(train_data)
  y = unlist(train_label)
  lambda = c()
  for (seed in sample(1:1000,7)) {
    set.seed(seed)
    cv_fit <- glmnet::cv.glmnet(x = x, y = y, alpha = 0, nfolds = 10, type.measure = cv_scorer, family = family)
    lambda = append(lambda, cv_fit$lambda)
  }
  
  ## make the caret tuning grid (pseudo random search)
  ## search through reasonable lambda list and different alpha parameters
  ## alpha is the "mixing paramenter" of ridge and lasso
  tuneGrid <-  expand.grid(alpha = 0, lambda = as.numeric(lambda))
  tuneGrid <- sample_n(tuneGrid, pmin(as.numeric(opt$tune_length), NROW(tuneGrid)))
  
  ## set up parallel
  cl <- parallel::makePSOCKcluster(as.numeric(opt$ncores))
  doParallel::registerDoParallel(cl)
  
  ## reset seed to user defined seed
  set.seed(opt$seed)
  
  ## run lasso model
  training_fit <- caret::train(x = train_data, y = label, 
                               preProcess = c("nzv","corr"),
                               method = "glmnet", 
                               trControl = fit_control, 
                               tuneGrid = tuneGrid,
                               importance = "permutation")
  
  parallel::stopCluster(cl)
  plot(training_fit, plotType = "line")
  
}

## assess models

if (opt$type == "classification" ) {
  if (length(levels(as.factor(train_label$label))) == 2) {
    ## run MLeval
    print(caret::confusionMatrix(data = predict(training_fit, test_data), reference = as.factor(test_label$label)))
    
    pdf(file = paste0(opt$outdir, "roc_auc_curve.pdf"), width=5, height=5)
    res <- MLeval::evalm(training_fit, plots = "r")
    suppressMessages(dev.off())
  } else {
    print(caret::confusionMatrix(data = predict(training_fit, test_data), reference = as.factor(test_label$label)))
  }
} 

if (opt$type == "regression") {
  ## For regression:
  pred <- predict(training_fit, test_data)
  print(caret::postResample(pred = pred, obs = test_label$label))
  
}


