# /*===========================================================
#' # Continuous tretment DML_OF analysis
# /*===========================================================

run_DML_OF_quad_analysis <- function(gam_formula, x_vars, w_vars, reg_data, pN, pCorn, N_levels) {

  data <- copy(reg_data)

  # /*+++++++++++++++++++++++++++++++++++
  #' ## Define input data
  # /*+++++++++++++++++++++++++++++++++++
  Y <- data[, yield] %>% as.array() #* dependent var
  X <- data[, x_vars, with = FALSE] %>% as.matrix() #* het impact driver
  W <- data[, w_vars, with = FALSE] %>% as.matrix() #* controls

  #* Treatment
  min_N <- data[, min(N)]
  max_N <- data[, max(N)]

  T_mat <- 
    data[, N := (N - min_N) / (max_N - min_N)] %>%
    .[, .(N, N^2)] %>% 
    #* normalize
    as.matrix()

  #* Define some hyper parameters
  subsample_ratio <- 0.3
  # lambda_reg <- sqrt(log(ncol(W)) / (10 * subsample_ratio * nrow(Y)))

  # /*+++++++++++++++++++++++++++++++++++
  #' ## Estimation
  # /*+++++++++++++++++++++++++++++++++++
  #* estimate Doubly-Robus Orthogonal Forest
  results <-
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
    data.table(N = seq(0, 1, length = 100)) %>%
    .[, N2 := N^2] %>%
    as.matrix()

  #* \sum_{k=1}^3 \phi_k(t)\cdot \theta_k(x_1, x_2) at various values of t
  te <-
    lapply(
      1:ncol(N_data),
      function(x) {
        N_data[, x] %*% (t(results[, x] %>% as.matrix()))
      }
    ) %>%
    reduce(`+`) %>%
    data.table() %>%
    setnames(names(.), as.character(data$aunit_id)) %>%
    .[, N := N_data[, 1]] %>%
    melt(id.var = "N") %>%
    setnames("variable", "aunit_id") %>%
    .[, aunit_id := as.numeric(as.character(aunit_id))]

  eonr <-
    te %>%
    .[, profit := pCorn * value - pN * N] %>%
    .[, .SD[which.max(profit), ], by = aunit_id] %>%
    .[, .(aunit_id, N)] %>%
    .[, N := N * (max_N - min_N) + min_N] %>%
    setnames("N", "opt_N_hat") 

  return(eonr)
}