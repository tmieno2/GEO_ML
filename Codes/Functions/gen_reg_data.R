gen_reg_data <- function(i, field_with_design, field_parameters) {

  print(i)

  experiment_data <-
    data.table(field_with_design) %>%
    .[i, ] %>%
    left_join(
      .,
      dplyr::select(field_parameters, field_col, sp_range, field_pars),
      by = c("field_col", "sp_range")
    )

  # === load cell level data (coef, error) ===#
  cell_data <- experiment_data$field_pars[[1]]
  field <- experiment_data$field_sf[[1]] %>% data.table()

  # /*+++++++++++++++++++++++++++++++++++
  #' # Define the levels of experimental input rate by simulation id
  # /*+++++++++++++++++++++++++++++++++++
  N_levels_data <-
    cell_data[, .(sim, Nk)] %>%
    .[Nk > 300, Nk := 300] %>%
    .[, .(N_levels = list(
      c(
        min(Nk) - 20,
        quantile(Nk, prob = 0.2),
        quantile(Nk, prob = 0.4),
        quantile(Nk, prob = 0.6),
        quantile(Nk, prob = 0.8),
        max(Nk) + 20
      ) %>%
      pmax(0, .) %>%
      round()
    )), by = sim]

  # /*+++++++++++++++++++++++++++++++++++
  #' # Assign input rate
  # /*+++++++++++++++++++++++++++++++++++
  #* the number of blocks
  block_num <- unique(field$block_id) %>% length()

  #* assign N rates
  n_assign_data <-
    lapply(
      cell_data[, sim] %>% unique(),
      function(x) {

        #* find the N_levels for the sim
        N_levels <- N_levels_data[sim == x, N_levels][[1]]

        #* assign N rates
        assign_input_rate(
          N_levels = N_levels,
          block_num = block_num,
          design = experiment_data$design_name
        ) %>%
        .[, sim := x]

      }
    ) %>%
    rbindlist()

  reg_data <-
    field[cell_data, on = "cell_id"] %>%
    n_assign_data[., on = c("sim", "block_id", "plot_in_block_id")] %>%
    #* used for calculating spatial measures
    .[, Nid := as.numeric(as.factor(N))] %>%
    #* target N rate for mapping (before application error is added)
    #===add cell-level N application noise===#
    .[, N := N * (1 + N_error * 0.1)] %>%
    .[N < 0, N := 0] %>%
    .[, N2 := N^2] %>%
    #* deterministic yield
    .[, det_yield := gen_yield_QP(b0, b1, b2, Nk, N)] %>%
    #* create yield errors 
    .[, mean_det_yield := mean(det_yield), by = sim] %>%
    .[, yield_error := mean_det_yield * m_error] %>%
    .[, yield := det_yield + yield_error]  %>%
    #* remove observations in the buffer zone 
    .[buffer == 0, ] %>%
    #* aggregate the data by analysis unit
    .[,
      lapply(.SD, mean),
      by = .(sim, aunit_id),
      .SDcols = c("yield", "yield_error", "Nid", "N", "N2", "b0", "b1", "b2", "Nk", "X", "Y")
    ] %>%
    nest_by_dt(by = "sim") %>%
    N_levels_data[., on = "sim"] 

  final_data <-
    experiment_data %>%
    mutate(reg_data = list(reg_data)) %>%
    dplyr::select(- field_pars, - field_sf)

  print(paste0("Saving the data to ", final_data$data_file_name))

  saveRDS(final_data, final_data$data_file_name)

}
