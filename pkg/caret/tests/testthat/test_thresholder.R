# Helper function to create a basic train object for testing thresholder
create_test_train_object <- function(n = 50, savePreds = "all", numTuneParams = 1, model_method = "rda") {
  withr::local_seed(123 + n + numTuneParams) # Vary seed for different configurations
  dat <- twoClassSim(n, intercept = -10)

  ctrl <- trainControl(
    method = "cv",
    number = 2, # Use a small number of folds/resamples for speed
    classProbs = TRUE,
    savePredictions = savePreds,
    summaryFunction = twoClassSummary,
    allowParallel = FALSE # Ensure tests run serially
  )

  current_method <- model_method
  tune_arg <- list()

  if (model_method == "glm" || numTuneParams == 0) {
    # GLM by default has no tuning parameters in the caret sense,
    # train() will add a 'parameter' column with "none"
    current_method <- "glm"
  } else if (model_method == "rda") {
    skip_if_not_installed("klaR")
    # RDA has gamma and lambda
    tune_arg <- list(tuneLength = numTuneParams) # tuneLength will generate combinations
  } else if (model_method == "knn") {
    # KNN has k
    tune_arg <- list(tuneLength = numTuneParams)
  }
  # Add more methods if needed for specific tuning parameter structures

  # Suppress warnings during training for test stability (e.g. convergence for small n)
  suppressWarnings({
    trained_model <- do.call(train,
                             c(list(Class ~ .,
                                  data = dat,
                                  method = current_method,
                                  metric = "ROC",
                                  trControl = ctrl),
                               tune_arg)
                             )
  })
  return(trained_model)
}

test_that("thresholder works with basic valid inputs (e.g., rda model)", {
  train_obj <- create_test_train_object(n = 50, savePreds = "all", numTuneParams = 1, model_method = "rda")
  thresholds <- seq(0.1, 0.9, by = 0.2)
  
  res <- thresholder(train_obj, threshold = thresholds)
  
  expect_s3_class(res, "data.frame")
  expect_true("prob_threshold" %in% names(res))
  
  expected_stats <- c("Sensitivity", "Specificity", "Pos Pred Value",
                      "Neg Pred Value", "Precision", "Recall", "F1", "Prevalence",
                      "Detection Rate", "Detection Prevalence", "Balanced Accuracy",
                      "Accuracy", "Kappa", "J", "Dist")
  expect_true(all(expected_stats %in% names(res)))
  
  # For final=TRUE (default) and numTuneParams=1 (leading to 1 bestTune row)
  expect_equal(nrow(res), length(unique(thresholds)) * nrow(train_obj$bestTune))
  expect_true(all(names(train_obj$bestTune) %in% names(res)))
})

test_that("thresholder works with model having no explicit tune params (e.g., glm)", {
  train_obj_glm <- create_test_train_object(n = 50, savePreds = "all", numTuneParams = 0, model_method = "glm")
  thresholds <- c(0.3, 0.6)
  
  expect_warning({
    res_glm <- thresholder(train_obj_glm, threshold = thresholds)
  }, "following columns have missing values")
  expect_s3_class(res_glm, "data.frame")
  expect_true("prob_threshold" %in% names(res_glm))
  # glm bestTune has a 'parameter' column with "none"
  expect_true("parameter" %in% names(res_glm))
  expect_equal(nrow(res_glm), length(thresholds) * nrow(train_obj_glm$bestTune))
})


test_that("thresholder 'final' argument works correctly", {
  # Use rda with tuneLength > 1 to ensure multiple tune combos in x$pred when savePreds="all"
  train_obj_multi_tune <- create_test_train_object(n = 60, savePreds = "all", numTuneParams = 2, model_method = "rda")
  thresholds <- c(0.3, 0.7)
  
  # final = TRUE (default)
  res_final_true <- thresholder(train_obj_multi_tune, threshold = thresholds, final = TRUE)
  expect_equal(nrow(res_final_true), length(thresholds) * 1) # Only bestTune results
  for(param_name in names(train_obj_multi_tune$bestTune)) {
    expect_equal(unique(res_final_true[[param_name]]), train_obj_multi_tune$bestTune[[param_name]])
  }

  # final = FALSE
  res_final_false <- thresholder(train_obj_multi_tune, threshold = thresholds, final = FALSE)
  pred_tunes <- unique(train_obj_multi_tune$pred[, names(train_obj_multi_tune$bestTune), drop = FALSE])
  num_unique_pred_tunes <- nrow(pred_tunes)
  expect_equal(nrow(res_final_false), length(thresholds) * num_unique_pred_tunes)
  for(param_name in names(train_obj_multi_tune$bestTune)) {
    expect_setequal(unique(res_final_false[[param_name]]), unique(train_obj_multi_tune$pred[[param_name]]))
  }
})

test_that("thresholder 'final' argument with train_obj$control$savePredictions = 'final'", {
  train_obj_sp_final <- create_test_train_object(n = 50, savePreds = "final", numTuneParams = 2, model_method = "rda")
  thresholds <- c(0.4, 0.6)

  # When savePredictions = "final", x$pred is already filtered to bestTune.
  # So, final=TRUE and final=FALSE in thresholder should yield results based on bestTune.
  res_final_true_sp <- thresholder(train_obj_sp_final, threshold = thresholds, final = TRUE)
  res_final_false_sp <- thresholder(train_obj_sp_final, threshold = thresholds, final = FALSE)

  expect_equal(nrow(res_final_true_sp), length(thresholds) * 1)
  expect_equal(nrow(res_final_false_sp), length(thresholds) * 1) # expand_preds gets x$pred which is already for bestTune
  expect_equal(res_final_true_sp, res_final_false_sp)
})

test_that("thresholder 'statistics' argument works", {
  train_obj <- create_test_train_object(n = 50, savePreds = "final", numTuneParams = 1, model_method = "rda")
  thresholds <- c(0.25, 0.75)
  
  res_sens <- thresholder(train_obj, threshold = thresholds, statistics = "Sensitivity")
  expect_true("Sensitivity" %in% names(res_sens))
  expect_false("Specificity" %in% names(res_sens))
  expect_equal(ncol(res_sens), ncol(train_obj$bestTune) + 1 + 1) # tune params + prob_thresh + Sensitivity
  
  stats_subset <- c("Accuracy", "Kappa", "J")
  res_subset <- thresholder(train_obj, threshold = thresholds, statistics = stats_subset)
  expect_true(all(stats_subset %in% names(res_subset)))
  expect_false("Sensitivity" %in% names(res_subset))
  expect_equal(ncol(res_subset), ncol(train_obj$bestTune) + 1 + length(stats_subset))
  
  res_all <- thresholder(train_obj, threshold = thresholds, statistics = "all")
  expected_stats_all <- c("Sensitivity", "Specificity", "Pos Pred Value", "Neg Pred Value", 
                          "Precision", "Recall", "F1", "Prevalence", "Detection Rate", 
                          "Detection Prevalence", "Balanced Accuracy", "Accuracy", "Kappa", "J", "Dist")
  expect_true(all(expected_stats_all %in% names(res_all)))
})

test_that("thresholder throws errors for invalid inputs", {
  train_obj_valid <- create_test_train_object(n = 30, numTuneParams = 1, model_method="glm") # glm is faster
  thresholds <- 0.5

  expect_error(thresholder(list(), threshold = thresholds),
               "`x` should be an object of class 'train'")

  train_obj_no_probs <- train_obj_valid
  train_obj_no_probs$control$classProbs <- FALSE
  expect_error(thresholder(train_obj_no_probs, threshold = thresholds),
               "`classProbs` must be TRUE in `trainControl`")

  train_obj_no_save <- train_obj_valid
  train_obj_no_save$control$savePredictions <- FALSE
  expect_error(thresholder(train_obj_no_save, threshold = thresholds),
               "`savePredictions` should be TRUE, 'all', or 'final'")
  
  train_obj_save_none <- train_obj_valid
  train_obj_save_none$control$savePredictions <- "none"
  expect_error(thresholder(train_obj_save_none, threshold = thresholds),
               "`savePredictions` should be TRUE, 'all', or 'final'")

  train_obj_mc <- train_obj_valid
  # Simulate multi-class by altering $pred$obs levels
  # The check is `if (length(levels(x$pred$obs)) > 2)`
  # train() itself would likely error or behave differently for multi-class with twoClassSummary
  # For this unit test, directly modify pred to test thresholder's check
  mc_pred_data <- train_obj_mc$pred
  mc_pred_data$obs <- factor(sample(letters[1:3], nrow(mc_pred_data), replace = TRUE))
  train_obj_mc$pred <- mc_pred_data
  expect_error(thresholder(train_obj_mc, threshold = thresholds),
               "For two class problems only")

  expect_error(thresholder(train_obj_valid, threshold = NULL),
               "Please supply probability threshold values.")
  
  expect_error(thresholder(train_obj_valid, threshold = c(0.5, 1.1)),
               "`threshold` should be on \\[0,1\\]")
  expect_error(thresholder(train_obj_valid, threshold = -0.1),
               "`threshold` should be on \\[0,1\\]")

  expect_error(thresholder(train_obj_valid, threshold = thresholds, statistics = "InvalidStat"),
               "`statistics` should be either 'all', or one or more of")
  expect_error(thresholder(train_obj_valid, threshold = thresholds, statistics = c("all", "Sensitivity")),
               "`statistics` should be either 'all', or one or more of")
})

test_that("thresholder calculates J and Dist statistics correctly and handles NAs in other stats", {
  skip_on_cran()
  # Using more data for potentially more stable Sens/Spec estimates
  train_obj <- create_test_train_object(n = 100, savePreds = "final", numTuneParams = 1, model_method="rda")
  thresholds <- seq(0.05, 0.95, by = 0.1)
  
  res <- thresholder(train_obj, threshold = thresholds, statistics = c("Sensitivity", "Specificity", "J", "Dist", "Pos Pred Value"))
  
  expect_true(all(c("J", "Dist", "Sensitivity", "Specificity") %in% names(res)))
  expect_true(is.numeric(res$J) & !all(is.na(res$J))) # Ensure J is calculated
  expect_true(is.numeric(res$Dist) & !all(is.na(res$Dist))) # Ensure Dist is calculated
  
  # Check consistency where Sens/Spec are not NA
  valid_idx <- !is.na(res$Sensitivity) & !is.na(res$Specificity)
  if(any(valid_idx)) {
    expect_equal(res$J[valid_idx], res$Sensitivity[valid_idx] + res$Specificity[valid_idx] - 1, tolerance = 1e-9)
    expect_equal(res$Dist[valid_idx], sqrt((res$Sensitivity[valid_idx] - 1)^2 + (res$Specificity[valid_idx] - 1)^2), tolerance = 1e-9)
    expect_true(all(res$J[valid_idx] >= -1 & res$J[valid_idx] <= 1))
    expect_true(all(res$Dist[valid_idx] >= 0 & res$Dist[valid_idx] <= sqrt(2)))
  }
  
  # Test that it runs if a stat like Pos Pred Value is NA (summ_stats uses na.rm=TRUE)
  # This is an indirect test of NA handling in summ_stats.
  # If Pos Pred Value is all NA (e.g. due to no positive predictions in any resample for some thresholds),
  # the column should still exist and be NA.
  expect_true("Pos Pred Value" %in% names(res))
  # No error should be thrown if some base statistics are NA.
})

test_that("thresholder handles empty input from expand_preds gracefully (edge case)", {
  train_obj <- create_test_train_object(n = 30)
  
  # Simulate x$pred being empty after merge (highly unlikely with valid train obj)
  train_obj_empty_pred <- train_obj
  train_obj_empty_pred$bestTune <- data.frame(gamma = 999, lambda = 999) # Ensure merge yields 0 rows

  res_empty <- thresholder(train_obj_empty_pred, threshold = c(0.2, 0.8))
  expect_s3_class(res_empty, "data.frame")
  expect_equal(nrow(res_empty), 0)
  
  # Check for expected column names even with 0 rows
  expected_cols <- c(names(train_obj_empty_pred$bestTune), "prob_threshold",
                     "Sensitivity", "Specificity") # Example stats
  # Actual stats depend on default "all"
  all_stats_names <- c("Sensitivity", "Specificity", "Pos Pred Value",
                       "Neg Pred Value", "Precision", "Recall", "F1", "Prevalence",
                       "Detection Rate", "Detection Prevalence", "Balanced Accuracy",
                       "Accuracy", "Kappa", "J", "Dist")
  expected_cols_full <- c(names(train_obj_empty_pred$bestTune), "prob_threshold", all_stats_names)
  expect_true(all(expected_cols_full %in% names(res_empty)))
})
