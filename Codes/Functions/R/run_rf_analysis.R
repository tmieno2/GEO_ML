# /*===========================================================
#' # GWR analysis
# /*===========================================================
# ! Run GWR and do economic analysis

# train_data <- scaled_train_data
# test_data <- scaled_test_data

run_rf_analysis <- function(train_data, test_data, x_vars, prices) {
  data <- copy(train_data)

  X <-
    data[, c("N", x_vars), with = FALSE] %>%
    data.frame()

  Y <- data[, yield]

  RF_temp <-
    grf::regression_forest(
      X = X,
      Y = Y,
      num.trees = 2000,
      min.node.size = 10
      # tune.parameters = TRUE
    )

  results <-
    test_data %>%
    .[, yield_hat := predict(RF_temp,
      newdata = .[, c("N", x_vars), with = FALSE]
    )] %>%
    .[, pi_hat := prices[, yield] * yield_hat - prices[, N] * N] %>%
    .[, .SD[which.max(pi_hat)], by = aunit_id] %>%
    .[, opt_N_hat := N] %>%
    .[, .(aunit_id, N)]

  return(brf_results)
}