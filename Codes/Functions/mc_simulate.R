# /*===========================================================
#' # MC simulations
# /*===========================================================
#! Run GWR and BRF and do economic analysis

mc_simulate <- function(data, pCorn, pN, sim, N_levels) {

  print(sim)
  
  #/*~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### SCAM
  #/*~~~~~~~~~~~~~~~~~~~~~~*/

  #=== scam regression ===#
  scam_res <- gam(yield~s(N, k=5) + s(X, k = 6) + s(Y, k = 6) + ti(X, Y, k = 6), 
                  data=data)
  
  opt_N_scam <- data.table(N=seq(min(N_levels),max(N_levels),length=1000)) %>%
    .[, X := data[1, ] %>%  pull(X)] %>%
    .[, Y := data[1, ] %>%  pull(Y)] %>%
    .[, yhat := predict(scam_res, newdata=.)] %>%
    .[, profit := pCorn*yhat - pN*N] %>%
    .[profit == max(profit), N] %>%
    max(min(N_levels),.) %>%
    min(max(N_levels),.)
  
  
  
  #/*~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### GWR estimate
  #/*~~~~~~~~~~~~~~~~~~~~~~*/
  
  #=== regression data in sp ===#
  reg_data_sp <- 
    data %>%
    st_as_sf(coords = c("X", "Y")) %>%
    as("Spatial")
  
  
  #=== gwr estimation with different bandwidth ===#
  #! note: buffer zone data are dropped from analysis
  gwr_beta <- estimate_GWR(
    reg_data_sp = reg_data_sp,
    N_levels = N_levels,
    pN = pN,
    pCorn = pCorn
  )

  
  
  
  #/*~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### Results data
  #/*~~~~~~~~~~~~~~~~~~~~~~*/
  
  est_beta <- gwr_beta %>% 
    .[, opt_N_scam:=opt_N_scam]

  return(est_beta)

}