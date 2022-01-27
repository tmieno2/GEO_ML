# /*===========================================================
#' # GWR analysis
# /*===========================================================
# ! Run GWR and do economic analysis

run_linear_analysis <- function(train_data, test_data, x_vars, prices) {
  reg_data <- copy(train_data)

  control_vars <- paste0(x_vars, collapse = "+")
  int_vars <-
    paste0(
      paste0("I(N * ", x_vars, ")", collapse = "+"),
      "+",
      paste0("I(N2 * ", x_vars, ")", collapse = "+")
    )

  feols_formula <-
    paste0(
      "yield ~ N + N2 + ",
      control_vars, "+",
      int_vars
    ) %>%
    formula()

  ols_res <- fixest::feols(feols_formula, data = reg_data)

  ols_results <-
    copy(test_data) %>%
    .[, y_hat := predict(ols_res, newdata = .)] %>%
    .[, pi_hat := prices[, yield] * y_hat - prices[, N] * N] %>%
    .[, .SD[which.max(pi_hat), ], by = aunit_id] %>%
    .[, .(aunit_id, N)]

  return(ols_results)
}