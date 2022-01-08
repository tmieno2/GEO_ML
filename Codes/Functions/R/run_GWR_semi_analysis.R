# /*===========================================================
#' # Semi-parametric GWR analysis
# /*===========================================================

run_GWR_semi_analysis <- function(gam_formula, reg_data, N_levels, pN, pCorn) {
  data <- copy(reg_data)

  gam_setup <- gam(gam_formula, data = data)

  x_mat <-
    predict(gam_setup, data = data, type = "lpmatrix") %>%
    #* get rid of the intercept
    .[, -1] %>%
    data.table() %>%
    setnames(names(.), paste0("s_", 1:ncol(.)))

  var_names <- names(x_mat)

  gwr_formula <-
    paste0(
      "yield ~",
      paste0(var_names, collapse = "+")
    )

  data_sp <-
    cbind(data, x_mat) %>%
    st_as_sf(coords = c("X", "Y")) %>%
    as("Spatial")

  #----------------------------
  # estimated GWR coefficients
  #----------------------------
  obw <-
    bw.gwr(
      gwr_formula,
      data = data_sp,
      approach = "AICc",
      kernel = "gaussian"
      # ,
      # adaptive = T
    )

  gwr_est <-
    gwr.basic(
      gwr_formula,
      data = data_sp,
      # bw = obw,
      bw = 60,
      kernel = "gaussian"
      # ,
      # adaptive = T
    )

  gwr_beta <-
    gwr_est$SDF %>%
    st_as_sf() %>%
    .[, c("Intercept", var_names), with = FALSE] %>%
    data.table() %>%
    .[, geometry := NULL] %>%
    .[, aunit_id := data$aunit_id] %>%
    setnames(var_names, paste0("b_", var_names))

  N_seq <-
    data.table(
      N = quantile(data$N, prob = seq(0, 1, length = 100))
    )

  smth_N_seq <-
    predict(gam_setup, newdata = N_seq, type = "lpmatrix") %>%
    #* get rid of the intercept
    .[, -1] %>%
    data.table() %>%
    setnames(names(.), paste0("s_", 1:ncol(.)))

  N_data <- cbind(N_seq, smth_N_seq)

  eval_data <- expand_grid_df(gwr_beta, N_data)

  eval(parse(text = paste(
    "eval_data[, y_hat := Intercept + ",
    paste0("b_", var_names, " * ", var_names, collapse = "+"),
    "]",
    sep = ""
  )))

  gwr_semi_results <-
    eval_data %>%
    .[, profit := pCorn * y_hat - pN * N] %>%
    .[, .SD[which.max(profit), ], by = aunit_id] %>%
    setnames("N", "opt_N_hat") %>%
    .[, .(aunit_id, opt_N_hat)]

  return(gwr_semi_results)
}