# /*===========================================================
#' # GWR analysis
# /*===========================================================
# ! Run GWR and do economic analysis


run_macf_analysis <- function(reg_data, cv_data, x_vars, pN, pCorn, N_levels) {
  data <- copy(reg_data)

  Y <- data[, yield]
  W_f <- as.factor(data[, N_tgt])
  X <- data[, x_vars, with = FALSE]
  X_cv <- cv_data$data[[1]][, x_vars, with = FALSE]

  macf_tau <- grf::multi_arm_causal_forest(X, Y, W_f)

  macf_results <-
    predict(macf_tau, newdata = X_cv)[[1]][, , 1] %>%
    data.table() %>%
    .[, aunit_id := data[, aunit_id]] %>%
    melt(id.var = "aunit_id") %>%
    .[, N := as.numeric(str_extract(variable, "^[^-]*"))] %>%
    .[, profit := pCorn * value - pN * N] %>%
    .[, .SD[which.max(profit), ], by = aunit_id] %>%
    .[profit < 0, N := N_levels[1]] %>%
    .[, .(aunit_id, N)] %>%
    setnames("N", "opt_N_hat") %>%
    .[, sim := cv_data$sim]

  return(macf_results)
}