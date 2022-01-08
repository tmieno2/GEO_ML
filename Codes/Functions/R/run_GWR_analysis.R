# /*===========================================================
#' # GWR analysis
# /*===========================================================
# ! Run GWR and do economic analysis

run_GWR_analysis <- function(reg_data, N_levels, pN, pCorn) {
  data <- copy(reg_data)

  # /*===========================================================
  #' # GWR-R (coefficient on N^2 global)
  # /*===========================================================
  # /*+++++++++++++++++++++++++++++++++++
  #' ## Get the optimal bandwidth
  # /*+++++++++++++++++++++++++++++++++++
  data$yield_rest <- data$yield - mean(data$b2) * data$N2

  data_sp <-
    data %>%
    st_as_sf(coords = c("X", "Y")) %>%
    as("Spatial")

  # === search for optimal bandwidth ===#
  obw <- bw.gwr(
    formula(yield_rest ~ N),
    data = data_sp,
    approach = "AICc",
    kernel = "gaussian"
  )

  # === loop over transfer coefficients ===#
  gwr_r <-
    data.table(
      b2_hat = seq(
        min(data$b2),
        max(data$b2),
        by = 0.05
      )
    ) %>%
    rowwise() %>%
    mutate(data = list(
      data %>%
        mutate(yield_rest = yield - b2_hat * N2)
    )) %>%
    mutate(data_sp = list(
      data %>%
        st_as_sf(coords = c("X", "Y")) %>%
        as("Spatial")
    )) %>%
    # mutate(obw = list(
    #   bw.gwr(
    #     formula(yield_rest ~ N),
    #     data = data_sp,
    #     approach = "AICc",
    #     kernel = "gaussian",
    #     adaptive = T
    #   )
    # )) %>%
    mutate(gwr_est = list(
      gwr.basic(
        formula(yield_rest ~ N),
        data = data_sp,
        bw = 60,
        # bw = obw,
        kernel = "gaussian"
      )
    )) %>%
    mutate(data = list(
      mutate(data, y_hat = gwr_est$SDF$yhat + b2_hat * N2)
    )) %>%
    mutate(
      R2 =
        lm(yield ~ y_hat, data = data) %>% summary() %>% .$r.squared
    ) %>%
    ungroup() %>%
    arrange(desc(R2)) %>%
    slice(1)

  #----------------------------
  # estimated GWR coefficients
  #----------------------------
  gwr_beta <-
    data.table(
      aunit_id = data$aunit_id,
      b0_hat = gwr_r$gwr_est[[1]]$SDF$Intercept,
      b1_hat = gwr_r$gwr_est[[1]]$SDF$N,
      b2_hat = gwr_r$b2_hat
    )

  # === GWR optimal N rates ===#
  gwr_beta <-
    gwr_beta %>%
    # === concave responses ===#
    .[b2_hat < 0 & b1_hat > 0, opt_N_gwr := (b1_hat - pN / pCorn) / (-2 * b2_hat)] %>%
    # === convex responses: corner solution ===#
    .[
      b2_hat >= 0 | b1_hat <= 0,
      yield_left := gen_yield_QD(b0_hat, b1_hat, b2_hat, min(N_levels))
    ] %>%
    .[
      b2_hat >= 0 | b1_hat <= 0,
      pi_left := pCorn * yield_left - pN * min(N_levels)
    ] %>%
    .[
      b2_hat >= 0 | b1_hat <= 0,
      yield_right := gen_yield_QD(b0_hat, b1_hat, b2_hat, max(N_levels))
    ] %>%
    .[
      b2_hat >= 0 | b1_hat <= 0,
      pi_right := pCorn * yield_right - pN * max(N_levels)
    ] %>%
    .[
      b2_hat >= 0 | b1_hat <= 0,
      opt_N_gwr :=
        as.numeric(pi_left > pi_right) * min(N_levels) +
        as.numeric(pi_left <= pi_right) * max(N_levels)
    ] %>%
    # === limit the range of opt_N_gwr ===#
    .[, opt_N_gwr := pmin(opt_N_gwr, max(N_levels))] %>%
    .[, opt_N_gwr := pmax(opt_N_gwr, min(N_levels))] %>%
    # === keep columns ===#
    .[, .(aunit_id, opt_N_gwr)] %>%
    setnames("opt_N_gwr", "opt_N_hat")

  return(gwr_beta)
}