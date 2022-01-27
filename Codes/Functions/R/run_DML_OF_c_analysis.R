# /*===========================================================
#' # Continuous tretment DML_OF analysis
# /*===========================================================

run_DML_OF_c_analysis <- function(gam_formula, x_vars, w_vars, reg_data, cv_data, pN, pCorn, N_levels) {
  data <- copy(reg_data)
  # /*+++++++++++++++++++++++++++++++++++
  #' ## Construct T matrix
  # /*+++++++++++++++++++++++++++++++++++
  #* gam set up
  gam_setup <- gam(gam_formula, data = data)

  #* construct T matrix
  T_mat <-
    predict(gam_setup, data = data, type = "lpmatrix") %>%
    #* get rid of the intercept
    .[, -1]

  # /*+++++++++++++++++++++++++++++++++++
  #' ## Define input data
  # /*+++++++++++++++++++++++++++++++++++
  Y <- data[, yield] %>% as.array() #* dependent var
  X <- data[, x_vars, with = FALSE] %>% as.matrix() #* het impact driver
  W <- data[, w_vars, with = FALSE] %>% as.matrix() #* controls
  X_cv <-
    cv_data$data[[1]] %>%
    .[, x_vars, with = FALSE] %>%
    as.matrix()

  #* Define some hyper parameters
  subsample_ratio <- 0.7
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
      X_test = X_cv,
      subsample_ratio = subsample_ratio
    )

  # /*+++++++++++++++++++++++++++++++++++
  #' ## Estimate yield at various treatment levels
  # /*+++++++++++++++++++++++++++++++++++
  N_data <-
    data.table(
      N = quantile(data$N, prob = seq(0, 1, length = 200))
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
    setnames(names(.), as.character(data$aunit_id)) %>%
    .[, N := N_data$N] %>%
    melt(id.var = "N") %>%
    setnames("variable", "aunit_id") %>%
    .[, aunit_id := as.numeric(as.character(aunit_id))]

  eonr <-
    te %>%
    .[, profit := pCorn * value - pN * N] %>%
    .[, .SD[which.max(profit), ], by = aunit_id] %>%
    .[, .(aunit_id, N)] %>%
    setnames("N", "opt_N_hat") %>%
    .[, sim := cv_data$sim]

  return(eonr)
}