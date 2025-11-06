#!/usr/bin/env Rscript

## SCRIPT: shap_figures.R ===================================================
## AUTHOR: Andrew Oliver
## DATE:   Jan, 30 2023
## LAST UPDATED: Nov 5, 2025
## PURPOSE: Run SHAP analysis post DietML

## load libraries

library(fastshap, quietly = T, verbose = F, warn.conflicts = F)
library(shapviz, quietly = T, verbose = F, warn.conflicts = F)
library(ggplot2, quietly = T, verbose = F, warn.conflicts = F)
library(tidymodels, quietly = T, verbose = F, warn.conflicts = F)
library(recipes, quietly = T, verbose = F, warn.conflicts = F)

shap_analysis <- function(label, output, model, filename, shap_inputs, train, test, type, parallel_workers) {
  
  # --- Setup ---
  shap_plot_env <- new.env()
  shap.error.occured <- FALSE
  error_message <- NULL
  output_dir <- paste0(output, "/ml_analysis")
  
  # --- Load SHAP inputs ---
  split_from_data_frame <- shap_inputs$split_from_data_frame
  best_tidy_workflow <- shap_inputs$best_tidy_workflow
  diet_ml_recipe <- shap_inputs$diet_ml_recipe
  
  assign(paste0("split_from_data_frame"), split_from_data_frame, envir = shap_plot_env)
  assign(paste0("best_tidy_workflow"), best_tidy_workflow, envir = shap_plot_env)
  assign(paste0("diet_ml_recipe"), diet_ml_recipe, envir = shap_plot_env)
  
  
  ## save some initial inputs to env, in case the below 
  ## shap analysis does not finish. Occasionaly it does not finish on
  ## the "test" dataset. Which is fine, i cant think of why that is used.
  ## But if it fails, we still want as much data returned as possible, so 
  ## that is why we return everything prior to returning the test shap data
  assign("split_from_data_frame", split_from_data_frame, envir = shap_plot_env)
  assign("label", label, envir = shap_plot_env)
  
  # --- Define prediction wrapper (pfun) ---
  pfun <- NULL
  if (model == "rf") {
    if (type == "classification" && length(levels(as.factor(split_from_data_frame$data$label))) == 2) {
      pfun <- function(object, newdata) {
        preds <- predict(object, data = newdata)$predictions
        class_level <- levels(as.factor(split_from_data_frame$data$label))[1]
        preds[, class_level]
      }
    } else {
      pfun <- function(object, newdata) {
        preds <- predict(object, data = newdata)$predictions
        as.numeric(preds)
      }
    }
    
  } else if (model %in% c("enet", "lasso", "ridge")) {
    if (type == "classification" && length(levels(as.factor(split_from_data_frame$data$label))) == 2) {
      pfun <- function(object, newdata) {
        preds <- predict(object, new_data = newdata, type = "prob")
        # take the probability of the second level (positive class)
        pos_class <- names(preds)[2]
        as.numeric(preds[[pos_class]])
      }
    } else if (type == "regression") {
      pfun <- function(object, newdata) {
        preds <- predict(object, new_data = newdata, type = "numeric")
        as.numeric(preds$.pred)
      }
    }
  }
  
  if (is.null(pfun)) {
    message("Error: Could not define prediction function (pfun). Check model and type inputs.")
    shap.error.occured <- TRUE
  } else {
    # --- SHAP analysis block ---
    result <- tryCatch({
      
      shap_data_subsets <- list(list(split_from_data_frame$data, "full"), list(train, "train"), list(test, "test"))
      
      for (i in seq_along(shap_data_subsets)) {
        # Fit the model
        best_workflow <- parsnip::fit(best_tidy_workflow, shap_data_subsets[[i]][[1]])
        best_workflow_mod <- workflows::extract_fit_parsnip(best_workflow)
        
        # Prepare data
        shap_data <- recipes::prep(diet_ml_recipe, shap_data_subsets[[i]][[1]]) %>%
          recipes::juice() %>%
          dplyr::select(-label, -dplyr::any_of(opt$subject_identifier))
        assign(paste0("shap_data_", shap_data_subsets[[i]][[2]]), shap_data, envir = shap_plot_env)
        
        ## shap safety checks!
        n_rows <- nrow(shap_data)
        n_cols <- ncol(shap_data)
        # try and calc a conservative nsim, otherwise choose 10.
        safe_nsim <- max(10, floor(1200000 / (n_rows * n_cols))) # 20 features x 300 samples x 200 sims = 1200000
        ## for smaller datasets, the above could lead to huge nsim. lets set max at 200.
        safe_nsim <- ifelse(safe_nsim > 199, 200, safe_nsim)
        
        ## warning if shap analysis looks like its going to take a long time
        if ((n_cols * n_rows) > 500000) {
          warning("This input dataset is pretty large for a SHAP analysis. This may take a long time, potentially exceeding walltime limits for shared resources (e.g., HPCs)", immediate. = T)
        }
        
        message(glue::glue("Running SHAP with nsim = {safe_nsim}"))
        
        ## start a parallel process
        cl <- parallel::makeForkCluster(as.numeric(parallel_workers))
        doParallel::registerDoParallel(cl)
        
        # set the appropriate object for the model
        if (model == "rf") {
          shap_model_object <- best_workflow_mod$fit
        } else if (model == "enet") {
          shap_model_object <- best_workflow_mod
        }
        
        # Compute SHAP values
        shap_explanations <- fastshap::explain(
          object = shap_model_object,
          X = shap_data,
          pred_wrapper = pfun,
          nsim = safe_nsim,
          adjust = TRUE,
          parallel = TRUE
        )
        
        parallel::stopCluster(cl)
        
        assign(paste0("shap_explanations_", shap_data_subsets[[i]][[2]]), shap_explanations, envir = shap_plot_env)
        
        # SHAP object for plotting
        sv <- shapviz::shapviz(shap_explanations, X = shap_data)
        assign(paste0("sv_", shap_data_subsets[[i]][[2]]), sv, envir = shap_plot_env)
        
        # Generate and save plot
        plot <- shap_plot(
          sv = sv,
          label = label,
          data_subset_label = shap_data_subsets[[i]][[2]],
          split_from_data_frame = split_from_data_frame,
          filename = filename,
          output_dir = output_dir,
          data_subset_index = i,
          type = type
        )
        assign(paste0("plot_", shap_data_subsets[[i]][[2]]), plot, envir = shap_plot_env)
      }
      
      
    }, error = function(e) {
      shap.error.occured <<- TRUE
      error_message <<- e$message
      NULL
    })
  }
  
  # --- Save and return results ---
  if (shap.error.occured) {
    message(paste("SHAP analysis encountered an issue and all output files may not have been generated:", error_message))
    if (!is.null(error_message)) { 
      ## attempt to still return what was written to shap_plot_env
      save(list = ls(envir = shap_plot_env), 
           envir = shap_plot_env,
           file = file.path(paste0(output_dir, "/shap_inputs_", filename, ".RData")),
           compress = "gzip"
      )
      ## return error message
      message("Error: ", error_message)
    }
  } else {
    message("✅ SHAP analysis completed successfully.")
    save(list = ls(envir = shap_plot_env), 
         envir = shap_plot_env,
         file = file.path(paste0(output_dir, "/shap_inputs_", filename, ".RData")),
         compress = "gzip"
    )
  }
  
  # --- Clean up large local objects ---
  rm(list = ls(envir = shap_plot_env), envir = shap_plot_env)
  gc(verbose = FALSE)
  
  return(invisible(list(
    success = !shap.error.occured,
    shap_plot_env = shap_plot_env,
    error_message = error_message
  )))
  
}

shap_plot <- function(
    sv,
    label,
    data_subset_label,
    split_from_data_frame,
    filename,
    output_dir,
    data_subset_index,
    type
) {
  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Determine class labels (assumes binary classification)
  class_levels <- levels(as.factor(split_from_data_frame$data$label))
  if (length(class_levels) < 2) {
    stop("Insufficient factor levels for label")
  }
  
  # Create plot
  ## MODIFY THESE PARAMETERS IF YOU WANT THE PLOT TO LOOK DIFFERENTLY!!
  plot <- shapviz::sv_importance(
    sv,
    kind = "bee",
    show_numbers = TRUE,
    bee_width = 0.2,
    max_display = 10
  ) +
    ggtitle(label = paste0("SHAP: ", label, " (", data_subset_label, ")")) +
    labs(x = ifelse(type == "classification", paste0(
      "predictive of ",
      class_levels[2],
      " < SHAP > predictive of ",
      class_levels[1]
    ), paste0(
      "low response < SHAP > high response "
    ))) +
    theme_bw(base_size = 14)
  
  # Construct filename and save plot
  filename_out <- file.path(output_dir, paste0("shap_", filename, "_", data_subset_label, ".pdf"))
  
  ggplot2::ggsave(
    plot = plot,
    filename = filename_out,
    width = pmax(0.1 * max(nchar(colnames(sv$X))), 6),
    height = 4.5,
    units = "in"
  )
  
  message("SHAP plot saved to: ", filename_out)
  
  return(plot)
}

shap_inputs <- list("split_from_data_frame" = tr_te_split, "diet_ml_recipe" = dietML_recipe, "best_tidy_workflow" = best_tidy_workflow)

shap_analysis(
  label = opt$label, 
  output = opt$outdir, 
  model = opt$model, 
  filename = paste0(opt$model, "_", opt$seed), 
  shap_inputs = shap_inputs, 
  train = train, 
  test = test, 
  type = opt$type, 
  parallel_workers = opt$parallel_workers
)

