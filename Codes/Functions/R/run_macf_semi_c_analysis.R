# /*===========================================================
#' # GWR analysis
# /*===========================================================
# ! Run GWR and do economic analysis


run_cf_semi_c_analysis <- function(gam_formula, x_vars, reg_data, pN, pCorn, N_levels) {

  #* R-learner Multi-arm CF
  #* 1st stage: cross-fit residualization
  #* 2nd stage: CF with cv

  data <- copy(reg_data)

  # /*+++++++++++++++++++++++++++++++++++
  #' ## Construct T matrix
  # /*+++++++++++++++++++++++++++++++++++
  #* gam set up
  gam_setup <- gam(gam_formula, data = data)

  #* construct W matrix
  W_mat <-
    predict(gam_setup, data = data, type = "lpmatrix") %>%
    #* get rid of the intercept
    .[, -1] %>%
    data.table() %>%
    setnames(names(.), paste0("W_", 1:ncol(.)))

  data <- cbind(data, W_mat)

  # /*+++++++++++++++++++++++++++++++++++
  #' ## First stage
  # /*+++++++++++++++++++++++++++++++++++
  #* residualize Y
  Y_r <-
    get_cross_fit_residuals(
      dep_var = "yield",
      x_vars = x_vars,
      data = data
    )

  #* residualize W
  W_r <-
    lapply(
      1:ncol(W_mat),
      function(it) {
        get_cross_fit_residuals(
          dep_var = names(W_mat)[it],
          x_vars = x_vars,
          data = data
        )
      }
    ) %>%
    reduce(cbind)

  X <- data[, ..x_vars] %>% as.matrix()

  # /*+++++++++++++++++++++++++++++++++++
  #' ## Second stage
  # /*+++++++++++++++++++++++++++++++++++
  macf_tau <- grf::multi_arm_causal_forest(X, Y_r, W_r)

  macf_results <-
    predict(macf_tau)[[1]][, , 1] %>%
    data.table() %>%
    .[, aunit_id := data[, aunit_id]] %>%
    melt(id.var = "aunit_id") %>%
    .[, N := as.numeric(str_extract(variable, "^[^-]*"))] %>%
    .[, profit := pCorn * value - pN * N] %>%
    .[, .SD[which.max(profit), ], by = aunit_id] %>%
    .[profit < 0, N := N_levels[1]] %>%
    .[, .(aunit_id, N)] %>%
    setnames("N", "opt_N_hat")

  return(macf_results)
}