# /*===========================================================
#' # GWR analysis
# /*===========================================================
# ! Run GWR and do economic analysis


run_linear_analysis <- function(reg_data, x_vars, pN, pCorn, N_levels) {
  data <- copy(reg_data)

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

  ols_res <- fixest::feols(feols_formula, data = data)

  N_data <-
    data.table(
      N = seq(min(N_levels), max(N_levels), length = 50)
    ) %>%
    .[, N2 := N^2]

  ols_results <-
    data %>%
    .[, `:=`(
      N = NULL,
      N2 = NULL
    )] %>%
    expand_grid_df(., N_data) %>%
    .[, y_hat := predict(ols_res, newdata = .)] %>%
    .[, pi_hat := pCorn * y_hat - pN * N] %>%
    .[, .SD[which.max(pi_hat), ], by = aunit_id] %>%
    .[, .(aunit_id, N)] %>%
    setnames("N", "opt_N_hat")

  return(ols_results)
}