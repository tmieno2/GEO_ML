# /*===========================================================
#' # Continuous tretment DML_OF analysis
# /*===========================================================

run_DML_OF_c_analysis <- function(gam_formula, x_vars, w_vars, reg_data) {

  # /*+++++++++++++++++++++++++++++++++++
  #' ## Construct T matrix
  # /*+++++++++++++++++++++++++++++++++++
  #* gam set up
  gam_setup <- gam(gam_formula, data = reg_data)

  #* construct T matrix
  T_mat <-
    predict(gam_setup, data = reg_data, type = "lpmatrix") %>%
    #* get rid of the intercept
    .[, -1]

  # /*+++++++++++++++++++++++++++++++++++
  #' ## Define input data
  # /*+++++++++++++++++++++++++++++++++++
  Y <- reg_data[, yield] %>% as.array() #* dependent var
  X <- reg_data[, x_vars, with = FALSE] %>% as.matrix() #* het impact driver
  W <- reg_data[, w_vars, with = FALSE] %>% as.matrix() #* controls

  #* Define some hyper parameters
  subsample_ratio <- 0.3
  # lambda_reg <- sqrt(log(ncol(W)) / (10 * subsample_ratio * nrow(Y)))

  # /*+++++++++++++++++++++++++++++++++++
  #' ## Estimation
  # /*+++++++++++++++++++++++++++++++++++
  #* estimate Doubly-Robus Orthogonal Forest
  results_se <-
    run_DML_OF_c(
      Y = Y,
      T = T_mat,
      X = X,
      W = W,
      X_test = X,
      subsample_ratio = subsample_ratio
    )

  # /*+++++++++++++++++++++++++++++++++++
  #' ## Estimate yield at various treatment levels
  # /*+++++++++++++++++++++++++++++++++++
  N_data <-
    data.table(
      N = quantile(reg_data$N, prob = seq(0, 1, length = 200))
    )

  eval_data <-
    N_data %>%
    predict(gam_setup, newdata = ., type = "lpmatrix") %>%
    #* get rid of the intercept
    .[, -1]

  #* \sum_{k=1}^3 \phi_k(t)\cdot \theta_k(x_1, x_2) at various values of t
  te <-
    lapply(
      1:ncol(eval_data),
      function(x) {
        (eval_data[, x] %>% as.matrix()) %*% t(results_se[, x] %>% as.matrix())
      }
    ) %>%
    reduce(`+`) %>%
    data.table() %>%
    setnames(names(.), as.character(reg_data$aunit_id)) %>%
    .[, N := N_data$N] %>%
    melt(id.var = "N") %>%
    setnames("variable", "aunit_id") %>%
    .[, aunit_id := as.numeric(as.character(aunit_id))]

  dmlof_c_results <-
    te %>%
    .[, profit := pCorn * value - pN * N] %>%
    .[, .SD[which.max(profit), ], by = aunit_id] %>%
    .[, .(aunit_id, N)] %>%
    setnames("N", "opt_N_dmlof_c")

  return(dmlof_c_results)
}