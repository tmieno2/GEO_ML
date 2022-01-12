# /*===========================================================
#' # GWR analysis
# /*===========================================================
# ! Run GWR and do economic analysis


run_brf_analysis <- function(reg_data, cv_data, x_vars, pN, pCorn, N_levels) {
  data <- copy(reg_data)

  X <-
    data[, c("N", x_vars), with = FALSE] %>%
    data.frame()

  Y <- data[, yield]

  BRF_temp <-
    grf::boosted_regression_forest(
      X = X,
      Y = Y,
      num.trees = 1000,
      min.node.size = 10
      # tune.parameters = TRUE
    )

  N_seq <-
    data.table(
      N = seq(min(data$N), max(data$N), by = 2)
    )

  brf_results <-
    copy(cv_data$data[[1]]) %>%
    .[, c("aunit_id", "yield", x_vars), with = FALSE] %>%
    expand_grid_df(., N_seq) %>%
    .[, yield_hat := predict(BRF_temp,
      newdata = .[, c("N", x_vars), with = FALSE]
    )] %>%
    .[, pi_hat := pCorn * yield_hat - pN * N] %>%
    .[, .SD[which.max(pi_hat)], by = aunit_id] %>%
    .[, opt_N_hat := N] %>%
    .[, .(aunit_id, opt_N_hat)] %>%
    .[, sim := cv_data$sim]

  return(brf_results)
}