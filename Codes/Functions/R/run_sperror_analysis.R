# /*===========================================================
#' # GWR analysis
# /*===========================================================
# ! Run GWR and do economic analysis


run_sperror_analysis <- function(reg_data, cv_data, x_vars, pN, pCorn, N_levels) {
  train_data <- copy(reg_data)
  test_data <- copy(cv_data$data[[1]])

  control_vars <- paste0(x_vars, collapse = "+")
  int_vars <-
    paste0(
      paste0("I(N * ", x_vars, ")", collapse = "+"),
      "+",
      paste0("I(N2 * ", x_vars, ")", collapse = "+")
    )

  #* Scael the train data
  scaler_data <-
    train_data[, c("aunit_id", "yield", "N", "N2", x_vars), with = FALSE] %>%
    melt(id.vars = "aunit_id") %>%
    nest_by(variable) %>%
    mutate(scaler = 1 / max(data$value)) %>%
    data.table() %>%
    .[, .(variable, scaler)]

  scaled_train_data <-
    train_data[, c("aunit_id", "yield", "N", "N2", x_vars), with = FALSE] %>%
    scale_data(., scaler_data)

  # /*+++++++++++++++++++++++++++++++++++
  #' ## Regression
  # /*+++++++++++++++++++++++++++++++++++
  feols_formula <-
    paste0(
      "yield ~ N + N2 + ",
      control_vars, "+",
      int_vars
    ) %>%
    formula()

  ols_res <- fixest::feols(feols_formula, data = scaled_train_data)

  # /*+++++++++++++++++++++++++++++++++++
  #' ## Find EONR
  # /*+++++++++++++++++++++++++++++++++++
  #* define N space
  N_data <-
    data.table(
      N = seq(min(N_levels), max(N_levels), length = 50)
    ) %>%
    .[, N2 := N^2]

  ols_results <-
    copy(test_data) %>%
    .[, `:=`(
      N = NULL,
      N2 = NULL
    )] %>%
    expand_grid_df(., N_data) %>%
    .[, c("aunit_id", "N", "N2", x_vars), with = FALSE] %>%
    scale_data(., scaler_data) %>%
    .[, yield := predict(ols_res, newdata = .)] %>%
    .[, .(aunit_id, yield, N)] %>%
    scale_data(., scaler_data, back = TRUE) %>%
    .[, pi_hat := pCorn * yield - pN * N] %>%
    .[, .SD[which.max(pi_hat), ], by = aunit_id] %>%
    .[, .(aunit_id, N)] %>%
    setnames("N", "opt_N_hat") %>%
    .[, sim := cv_data$sim]

  return(ols_results)
}