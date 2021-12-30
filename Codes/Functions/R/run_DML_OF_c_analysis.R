# /*===========================================================
#' # Continuous tretment DML_OF analysis
# /*===========================================================

run_GWR_semi_analysis <- function(gam_formula, reg_data, N_levels, pN, pCorn) {

  #* Construct T matrix
  gam_setup <- gam(gam_formula, data = reg_data)

  T_mat <-
    predict(gam_setup, data = reg_data, type = "lpmatrix") %>%
    #* get rid of the intercept
    .[, -1]

  Y <- reg_data[, yield] %>% as.array() #* dependent var
  X <- reg_data[, .(x, y, x * y)] %>% as.matrix() #* het impact driver
  W <- X #* controls

  #* Define some hyper parameters
  subsample_ratio <- 0.3
  lambda_reg <- sqrt(log(ncol(W)) / (10 * subsample_ratio * nrow(Y)))

  #* estimate Doubly-Robus Orthogonal Forest
  results <-
    run_DML_OF(
      Y = Y,
      T = T_mat,
      X = X,
      W = W,
      X_test = X,
      subsample_ratio = subsample_ratio,
      lambda_reg = lambda_reg
    )

  N_data <-
    data.table(
      N = quantile(reg_data$N, prob = seq(0, 1, length = 100))
    )

  eval_data <-
    N_data %>%
    predict(gam_setup, newdata = ., type = "lpmatrix") %>%
    #* get rid of the intercept
    .[, -1]

  te <-
    lapply(
      1:ncol(eval_data),
      function(x) {
        (eval_data[, x] %>% as.matrix()) %*% t(results[[x]] %>% as.matrix())
      }
    ) %>%
    reduce(`+`) %>%
    data.table() %>%
    setnames(names(.), paste0(reg_data$aunit_id)) %>%
    .[, N := N_data$N] %>%
    melt(id.var = "N") %>%
    setnames("variable", "aunit_id")

  return(gwr_semi_results)
}