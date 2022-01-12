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
  #' ### split b1
  # /*---------------------
  cell_data <-
    cell_data %>%
    #* split b0 (additive): b0_1 and b0_2
    split_par_additive("b0", ., nsim, xy, sp_range, gstat_model)[., on = c("sim", "cell_id")] %>%
    #* split plateau (min): plateau_1 and plateau_2
    split_par_min("plateau", ., nsim, xy, sp_range, gstat_model)[., on = c("sim", "cell_id")] %>%
    #* split plateau_1 (min): plateau_1_1 and plateau_1_2
    split_par_min("plateau_1", ., nsim, xy, sp_range, gstat_model)[., on = c("sim", "cell_id")] %>%
    #* split plateau_2 (min): plateau_2_1 and plateau_2_2
    split_par_min("plateau_2", ., nsim, xy, sp_range, gstat_model)[., on = c("sim", "cell_id")] %>%
    #* split Nk (additive): Nk_1 and Nk_2
    split_par_additive("Nk", ., nsim, xy, sp_range, gstat_model)[., on = c("sim", "cell_id")] %>%
    #* split Nk_1 (additive): Nk_1_1 and Nk_1_2
    split_par_additive("Nk_1", ., nsim, xy, sp_range, gstat_model)[., on = c("sim", "cell_id")] %>%
    #* split Nk_2 (additive): Nk_2_1 and Nk_2_2
    split_par_additive("Nk_2", ., nsim, xy, sp_range, gstat_model)[., on = c("sim", "cell_id")] 

    # identical(cell_data[, pmin(b1_1, b1_2)], cell_data[, b1])

  # /*+++++++++++++++++++++++++++++++++++
  #' ## Create irrelevant parameters
  # /*+++++++++++++++++++++++++++++++++++
  cell_data <-
    cell_data %>%
    .[, .(data = list(.SD)), by = sim] %>%
    rowwise() %>%
    #* create irrelevant variables
    mutate(data = list(
      mutate(
        data,
        theta_plateau_1 = faux::rnorm_pre(plateau, mu = 0, r = 0.7),
        theta_plateau_2 = faux::rnorm_pre(plateau, mu = 0, r = 0.7),
        theta_Nk_1 = faux::rnorm_pre(Nk, mu = 0, r = 0.7),
        theta_Nk_2 = faux::rnorm_pre(Nk, mu = 0, r = 0.7),
        theta_b0_1 = faux::rnorm_pre(b0, mu = 0, r = 0.7),
        theta_b0_2 = faux::rnorm_pre(b0, mu = 0, r = 0.7)
      )
    )) %>%
    unnest() %>%
    data.table()

  return(cell_data)
}