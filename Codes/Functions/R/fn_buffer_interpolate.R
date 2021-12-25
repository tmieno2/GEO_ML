

#=== spatial interpolate the buffer cells ===#

fn_buffer_interpolate <- function(full_map, full_data, est_data, varname){
    
    # cell level data
    f <- full_data[, .(cell_id, aunit_id)] %>% 
        est_data[.,on='aunit_id'] %>% 
        left_join(full_map[, c('cell_id','buffer','X','Y')], ., by='cell_id') 
    
    sdata <- data.table(f)
    
    # effective sample points
    PT <- sdata[buffer==0, ]
    coordinates(PT) = ~X+Y      # spatial points
    
    # whole field grids
    PX <- sdata[, .(buffer, X, Y, cell_id, aunit_id)]
    coordinates(PX) = ~X+Y  	# spatial points
    gridded(PX) = TRUE      	# spatial pixel
    
    # IDW interpolated variables
    for(x in varname){
        PX@data[,x] = gstat::idw(as.formula(paste0(x, "~1")), 
                                       PT, newdata=PX, idp=4)$var1.pred
        
        # ................................................
        # Note: The in-sample points are not interpolated,
        #	don't know why, but this is what we need
        # ................................................
        
        # if in-sample values are override, save an error report
        if(!all(PT@data[,x]==PX@data[PX$buffer==0,x])){
            saveRDS("interpolation override error",
                    paste0('./Results/error-',temp_size, '_',temp_range, 
                           '_', temp_design, '_', 'simulation_', i_sim,
                           '.rds')
            )
        }
    }

    # aggregate back to aunit level
    inter_data <- PX@data %>% data.table() %>%
        .[, lapply(.SD, mean), by=aunit_id, .SDcols=varname]
    
    
    # # check ..........................................................
    # plot(f[varname])
    # f <- full_data[, .(cell_id, aunit_id)] %>%
    #     inter_data[.,on='aunit_id'] %>%
    #     left_join(full_map[, c('cell_id','buffer','X','Y')], ., by='cell_id')
    # plot(f[varname])
    # ..................................................................
    
    return(inter_data)
}

# ..............................................................................
# Note: the au_sf is not grids/pixels because the subplots are not equal size
# therefore, we have to first interpolate at the cell level, and then aggregate
#	back to aunit level
# ..............................................................................
















# {	
#     # cell level data
#     sdata <- data[, .(cell_id, aunit_id)] %>% 
#         gwr_beta[.,on='aunit_id'] %>% 
#         left_join(f[, c('cell_id','buffer')], ., by='cell_id') %>%
#         cbind(., st_coordinates(st_centroid(.))) %>%
#         data.table()
#     
#     # effective sample points
#     PT <- sdata[buffer==0, .(buffer, X, Y, cell_id, aunit_id, opt_N_gwr)]
#     coordinates(PT) = ~X+Y      # spatial points
#     
#     # whole field grids
#     PX <- sdata[, .(buffer, X, Y, cell_id, aunit_id)]
#     coordinates(PX) = ~X+Y  	# spatial points
#     gridded(PX) = TRUE      	# spatial pixel
#     
#     # IDW interpolated opt N rates
#     PX$opt_N_gwr = gstat::idw(opt_N_gwr~1, PT, newdata=PX, idp=4)$var1.pred
#     # -> the in-sample points are not interpolated, which
#     #	is great, but not sure why
#     
#     # aggregate N rates back to subplot level
#     gwr_beta_full <- PX@data %>% data.table() %>%
#         .[, .(opt_N_gwr=mean(opt_N_gwr)), by=aunit_id]
#     
#     # update gwr_beta
#     if(all(PT$opt_N_gwr==PX$opt_N_gwr[PX$buffer==0])){
#         gwr_beta <- gwr_beta_full
#     } else {
#         gwr_beta <- gwr_beta # if strip designs, no buffer zones, just use old beta
#     }
# }
# 
# {
#     # cell level data
#     sdata <- data[, .(cell_id, aunit_id)] %>%
#         data_rf_perfect[.,on='aunit_id'] %>%
#         left_join(f[, c('cell_id','buffer')], ., by='cell_id') %>%
#         cbind(., st_coordinates(st_centroid(.))) %>%
#         data.table()
#     
#     # effective sample points
#     PT <- sdata[buffer==0, .(buffer, X, Y, cell_id, aunit_id, opt_N_rf_perfect)]
#     coordinates(PT) = ~X+Y      # spatial points
#     
#     # whole field grids
#     PX <- sdata[, .(buffer, X, Y, cell_id, aunit_id)]
#     coordinates(PX) = ~X+Y  	# spatial points
#     gridded(PX) = TRUE      	# spatial pixel
#     
#     # IDW interpolated opt N rates
#     PX$opt_N_rf_perfect = gstat::idw(opt_N_rf_perfect~1, PT, newdata=PX, idp=4)$var1.pred
#     # -> the in-sample points are not interpolated, which
#     #	is great, but not sure why
#     
#     # aggregate N rates back to subplot level
#     data_rf_perfect_full <- PX@data %>% data.table() %>%
#         .[, .(opt_N_rf_perfect=mean(opt_N_rf_perfect)), by=aunit_id]
#     
#     # update gwr_beta
#     if(all(PT$opt_N_rf_perfect==PX$opt_N_rf_perfect[PX$buffer==0])){
#         data_rf_perfect <- data_rf_perfect_full
#     } else {
#         data_rf_perfect <- data_rf_perfect
# }

