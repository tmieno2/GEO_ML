# /*===========================================================
#' # GWR analysis
# /*===========================================================
# ! Run GWR and do economic analysis


run_drof_analysis <- function(reg_data, x_vars, pN, pCorn, N_levels) {
  data <- copy(reg_data)

  N_tgt_rank_data <-
    data[, .(N_tgt = unique(N_tgt))] %>%
    .[, N_tgt_rank := rank(N_tgt) - 1]

  base_tgt_rate <- N_tgt_rank_data[N_tgt_rank == 0, N_tgt]

  data <- N_tgt_rank_data[data, on = "N_tgt"]

  Y <- data[, yield] %>% as.array() #* dependent var
  T <- data[, N_tgt_rank] %>% as.array() #* treatment
  # X <- data[, .(x, y, x * y, x^2, y^2, x^2 * y^2, x^3, y^3)] %>% as.matrix() #* het impact driver
  X <- data[, ..x_vars] %>% as.matrix() #* het impact driver
  W <- #* het impact driver #* controls
    data[, .(x, y, x * y)] %>%
    as.matrix() %>%
    cbind(X, .)
  # W <- X #* controls

  #* Define some hyper parameters
  subsample_ratio <- 0.3
  lambda_reg <- sqrt(log(ncol(W)) / (10 * subsample_ratio * nrow(Y)))

  #* estimate Doubly-Robus Orthogonal Forest
  DR_OF_res <-
    run_DR_OF(
      Y = Y,
      T = T,
      X = X,
      W = W,
      X_test = X,
      se = FALSE,
      subsample_ratio = subsample_ratio,
      lambda_reg = lambda_reg
    )

  #* make tau to a data.table
  tau_hat <-
    DR_OF_res %>%
    data.table()

  #* assign the estimated impact to analysis units
  tau_data <-
    cbind(data[, .(aunit_id)], tau_hat) %>%
    melt(id.var = "aunit_id") %>%
    setnames("value", "tau") %>%
    .[, N_tgt_rank := gsub("V", "", variable) %>% as.numeric()] %>%
    N_tgt_rank_data[., on = "N_tgt_rank"] %>%
    .[, .(aunit_id, N_tgt, tau)] %>%
    .[, sim := sim_i]

  # /*---------------------
  #' ### Raw
  # /*---------------------
  drof_results <-
    tau_data %>%
    .[, pi_delta := pCorn * tau - pN * N_tgt] %>%
    .[, .SD[which.max(pi_delta), ], by = aunit_id] %>%
    setnames("N_tgt", "opt_N_drof") %>%
    .[pi_delta < 0, opt_N_drof := base_tgt_rate] %>%
    .[, .(aunit_id, opt_N_drof)]

  return(drof_results)
}