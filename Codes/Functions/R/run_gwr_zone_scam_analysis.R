run_gwr_zone_scam_analysis <- function(gwr_results, reg_results, num_zones, reg_data, pCorn, pN) {
  data <- copy(reg_data)
  #--- grouping the data into 5 groups based on beta ---#
  zone_data <- gwr_results %>%
    mutate(
      zone = cut(
        opt_N_gwr,
        breaks = quantile(opt_N_gwr, prob = seq(0, 1, length = num_zones + 1)),
        include.lowest = TRUE
      )
    ) %>%
    mutate(zone_txt = factor(paste0("Zone ", as.numeric(zone)))) %>%
    .[, .(aunit_id, zone)]

  opt_N_scam <-
    zone_data[data, on = "aunit_id"] %>%
    nest_by(zone) %>%
    mutate(scam_res = list(
      # scam(yield ~ s(N, bs = "micv", k = 4), data = data)
      gam(yield ~ s(N, k = 4), data = data)
    )) %>%
    mutate(opt_data = list(
      data.table(N = seq(min(data$N), max(data$N), length = 100)) %>%
        .[, y_hat := predict(scam_res, newdata = .)] %>%
        .[, profit := pCorn * y_hat - pN * N] %>%
        .[which.max(profit), ]
    )) %>%
    dplyr::select(zone, opt_data) %>%
    unnest() %>%
    data.table() %>%
    setnames("N", "opt_N_hat")

  results <-
    opt_N_scam[zone_data, on = "zone"] %>%
    .[, .(aunit_id, opt_N_hat)]

  return(results)
}