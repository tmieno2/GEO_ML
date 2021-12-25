# /*===========================================================
#' # MC simulation
# /*===========================================================
mc_sim_results <-
  lapply(
    1:nrow(field_with_design), # indices to loop over
    function(x) {

      # ! things to do
      #* 1. estimate GWR and optimal N
      #* 2. estimate BRF and optimal N

      w_data <- field_with_design[x, ]

      # ! for debugging
      # data <- sim_results$data[[1]]
      # pCorn <- sim_results$pCorn[[1]]
      # pN <- sim_results$pN[[1]]
      # sim <- sim_results$sim[[1]]
      # N_levels <- sim_results$N_levels[[1]]

      sim_results <-
        #* read the right regression data for this
        #* experimental setting from the data_file_name
        readRDS(here(w_data$data_file_name)) %>%
        dplyr::select(reg_data, pCorn, pN) %>%
        unnest() %>%
        rowwise() %>%
        # ! MC simulations
        mutate(mc_results = list(
          tryCatch(
            mc_simulate(
              data = data,
              pCorn = pCorn,
              pN = pN,
              sim = sim,
              N_levels = N_levels
            ),
            error = function(e) NULL
          )
        )) %>%
        dplyr::select(sim, mc_results)

      results_return <-
        w_data %>%
        mutate(sim_results = list(sim_results))

      return(results_return)
    }
  ) %>%
  rbindlist()

#* save the field parameters
saveRDS(mc_sim_results, here("Results/mc_sim_results.rds"))