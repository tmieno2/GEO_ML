#* Generate a complete set of variable:
#* Plateau, Nk, b0, b1, b2, yeld error, application error

# sp_range <- 600
# gstat_model <- "Sph"
# nsim <- 10
# field_sf <-
#   readRDS(here("Data/field_with_design.rds")) %>%
#   .$field_sf %>%
#   .[[1]]

gen_field_pars <- function(sp_range, gstat_model, field_sf, nsim) {
  xy <- data.table(field_sf)[, .(X, Y, cell_id)]

  # === Nk ===#
  Nk_data <-
    gen_pars(
      mean = 200,
      psill = 1000,
      range = sp_range,
      coef_name = "Nk",
      gstat_model = gstat_model,
      xy = xy,
      nsim = nsim
    ) %>%
    # >>> normalize <<<
    .[, sd_b := sd(Nk), by = sim] %>%
    .[, mean_b := mean(Nk), by = sim] %>%
    .[, p := pnorm(Nk, mean = mean_b, sd = sd_b)] %>%
    .[, Nk := 100 + p * 150] %>%
    .[, c("cell_id", "sim", "Nk")]

  # Nk$Nk %>% hist(breaks=100)cell
  # rasterFromXYZ(Nk[sim==1, c("X","Y","Nk")]) %>% plot()

  # === ymax ===#
  ymax_data <-
    gen_pars(
      mean = 10000,
      psill = 3000000,
      range = sp_range,
      coef_name = "ymax",
      gstat_model = gstat_model,
      xy = xy,
      nsim = nsim
    ) %>%
    # >>> normalize <<<
    .[, sd_b := sd(ymax), by = sim] %>%
    .[, mean_b := mean(ymax), by = sim] %>%
    .[, p := pnorm(ymax, mean = mean_b, sd = sd_b)] %>%
    .[, ymax := 8000 + p * 8000] %>%
    .[, c("cell_id", "sim", "ymax")]

  # ymax$ymax %>% hist(breaks=100)
  # rasterFromXYZ(ymax[sim==1, c("X","Y","ymax")]) %>% plot()

  # === b0 ===#
  b0_data <-
    gen_pars(
      mean = 6000,
      psill = 200000,
      range = sp_range,
      coef_name = "b0",
      gstat_model = gstat_model,
      xy = xy,
      nsim = nsim
    ) %>%
    # >>> normalize <<< (b0 needs to be < ymax)
    .[, sd_b := sd(b0), by = sim] %>%
    .[, mean_b := mean(b0), by = sim] %>%
    .[, p := pnorm(b0, mean = mean_b, sd = sd_b)] %>%
    .[, b0 := 3000 + p * 4000] %>%
    .[, c("cell_id", "sim", "b0")]

  N_error_data <-
    gen_pars(
      mean = 0,
      psill = 0.2,
      range = 50,
      coef_name = "N_error",
      gstat_model = gstat_model,
      xy = xy,
      nsim = nsim
    ) %>%
    # >>> normalize <<<
    .[, min_b := min(N_error), by = sim] %>%
    .[, max_b := max(N_error), by = sim] %>%
    .[, p := punif(N_error, min_b, max_b)] %>%
    .[, N_error := (p - 0.5) / 5] %>%
    .[, c("cell_id", "sim", "N_error")]

  m_error_data <-
    gen_pars(
      mean = 0,
      psill = 0.015,
      range = sp_range,
      coef_name = "m_error",
      gstat_model = gstat_model,
      xy = xy,
      nsim = nsim
    ) %>%
    # >>> normalize <<<
    .[, min_b := min(m_error), by = sim] %>%
    .[, max_b := max(m_error), by = sim] %>%
    .[, p := punif(m_error, min_b, max_b)] %>%
    .[, m_error := -0.2 + p * 0.4] %>%
    .[, c("cell_id", "sim", "m_error")]

  # === b1, b2 ===#
  cell_data <-
    Nk_data %>%
    .[ymax_data, on = c("sim", "cell_id")] %>%
    .[b0_data, on = c("sim", "cell_id")] %>%
    # === derive b1, b2 from b0, ymax, and Nk
    .[, b1 := (-2) * (b0 - ymax) / Nk] %>%
    .[, b2 := (b0 - ymax) / Nk^2] %>%
    setnames("ymax", "plateau") %>%
    .[m_error_data, on = c("sim", "cell_id")] %>%
    .[N_error_data, on = c("sim", "cell_id")]

  # /*+++++++++++++++++++++++++++++++++++
  #' ## Splitting parameters
  # /*+++++++++++++++++++++++++++++++++++
  # /*---------------------
  #' ### b1
  # /*---------------------
  b1_split_factor <-
    gen_pars(
      mean = 0,
      psill = 1,
      range = sp_range,
      coef_name = "b1sf",
      gstat_model = gstat_model,
      xy = xy,
      nsim = nsim
    ) %>%
    # >>> normalize <<<
    .[, min_b1sf := min(b1sf), by = sim] %>%
    .[, max_b1sf := max(b1sf), by = sim] %>%
    .[, b1sf := punif(b1sf, min_b1sf, max_b1sf) + 0.5] %>%
    .[, b1sf_slack := pmax(1 - b1sf, 0)] %>%
    .[, c("cell_id", "sim", "b1sf", "b1sf_slack")]

  cell_data <-
    b1_split_factor[cell_data, on = c("sim", "cell_id")] %>%
    .[, b1_1 := b1 * b1sf] %>%
    .[b1_1 < b1, b1_1 := b1] %>%
    .[, b1_2 := fifelse(b1_1 > b1, b1, b1 * (1 + b1sf_slack))]

  # identical(cell_data[, pmin(b1_1, b1_2)], cell_data[, b1])

  # /*---------------------
  #' ### b2
  # /*---------------------
  b2_split_factor <-
    gen_pars(
      mean = 0,
      psill = 1,
      range = sp_range,
      coef_name = "b2sf",
      gstat_model = gstat_model,
      xy = xy,
      nsim = nsim
    ) %>%
    # >>> normalize <<<
    .[, min_b2sf := min(b2sf), by = sim] %>%
    .[, max_b2sf := max(b2sf), by = sim] %>%
    .[, b2sf := punif(b2sf, min_b2sf, max_b2sf) + 0.5] %>%
    .[, b2sf_slack := pmax(1 - b2sf, 0)] %>%
    .[, c("cell_id", "sim", "b2sf", "b2sf_slack")]

  cell_data <-
    b2_split_factor[cell_data, on = c("sim", "cell_id")] %>%
    .[, b2_1 := b2 * b2sf] %>%
    .[b2_1 < b2, b2_1 := b2] %>%
    .[, b2_2 := fifelse(b2_1 > b2, b2, b2 * (1 + b2sf_slack))]

  # /*---------------------
  #' ### Nk
  # /*---------------------
  Nk_split_factor <-
    gen_pars(
      mean = 0,
      psill = 1,
      range = sp_range,
      coef_name = "Nksf",
      gstat_model = gstat_model,
      xy = xy,
      nsim = nsim
    ) %>%
    # >>> normalize <<<
    .[, min_Nksf := min(Nksf), by = sim] %>%
    .[, max_Nksf := max(Nksf), by = sim] %>%
    .[, Nksf := punif(Nksf, min_Nksf, max_Nksf) + 0.5] %>%
    .[, Nksf_slack := pmax(1 - Nksf, 0)] %>%
    .[, c("cell_id", "sim", "Nksf", "Nksf_slack")]

  cell_data <-
    Nk_split_factor[cell_data, on = c("sim", "cell_id")] %>%
    .[, Nk_1 := Nk * Nksf] %>%
    .[Nk_1 < Nk, Nk_1 := Nk] %>%
    .[, Nk_2 := fifelse(Nk_1 > Nk, Nk, Nk * (1 + Nksf_slack))]

  # /*+++++++++++++++++++++++++++++++++++
  #' ## Irrelevant parameters
  # /*+++++++++++++++++++++++++++++++++++
  cell_data <-
    cell_data %>%
    .[, .(data = list(.SD)), by = sim] %>%
    rowwise() %>%
    mutate(data = list(
      mutate(
        data,
        theta_1_1 = faux::rnorm_pre(b1, mu = 0, r = 0.7),
        theta_1_2 = faux::rnorm_pre(b1, mu = 0, r = 0.7),
        theta_2_1 = faux::rnorm_pre(b2, mu = 0, r = 0.7),
        theta_2_2 = faux::rnorm_pre(b2, mu = 0, r = 0.7)
      )
    )) %>%
    unnest() %>%
    data.table()

  return(cell_data)
}