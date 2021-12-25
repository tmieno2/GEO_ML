

### =======================
### Monte Carlo Simulation: 
### =======================

sim <- function (i_sim,field,field_dt,pCorn,pN,m_error,coef_data,type='summary'){
	
	print( 
		paste(temp_length, temp_size, temp_range, temp_design, paste0('simulation ', i_sim),
				 sep=", ")
		)
	
	#/*~~~~~~~~~~~~~~~~~~~~~~*/
	#' ### Data preparation
	#/*~~~~~~~~~~~~~~~~~~~~~~*/
	#=== merge full field data with the coefs data ===#
	data <- coef_data[sim==i_sim,][data.table(field),on='cid'] %>%
		.[,opt_N:=(pN/pCorn-b1)/(2*b2)] %>%
		.[,opt_N:=pmin(Nk,opt_N)] %>%
		.[,opt_N:=pmax(0,opt_N)] %>%
		m_error[sim==1,.(cid,m_error)][.,on='cid']

	#/*~~~~~~~~~~~~~~~~~~~~~~*/
	#' ### Assign N
	#/*~~~~~~~~~~~~~~~~~~~~~~*/
	#=== define experimental N treatment levels ===#
	all_opt_N <- data[,N_star] %>% pmin(.,300)
	N_levels <- c(
		quantile(all_opt_N,prob=0.01)-20,
		quantile(all_opt_N,prob=0.2),
		quantile(all_opt_N,prob=0.4),
		quantile(all_opt_N,prob=0.6),
		quantile(all_opt_N,prob=0.8),
		quantile(all_opt_N,prob=0.99)+20
	) %>% 
		pmax(0, .) %>%
		round()
	
	#=== count number of treatment blocks ===#
	block_num <- data[,block_id] %>% unique() %>% length()
	
	
	#=== N design ===#
	N_design <- fn_N_design(N_levels, block_num, temp_design)
	

	#=== merge N treatments to field data ===#
	data %<>% N_design[.,on=c('block_id','plot_in_block_id')] %>%
		#===N treatment id
		.[, Nid:=as.numeric(as.factor(N))] %>%
	  .[, Ntg:=factor(N)] %>%
	  .[buffer==1, Ntg:="buffer"] %>%
	  .[, Ntg:=factor(Ntg, levels=c("buffer",as.character(levels(Ntg)[-7])))] %>%
	  #===add cell-level N application noise===#
		N_error[sim==i_sim,.(cid,N_error)][.,on='cid'] %>%
		.[,N:=N*(1+N_error*0.1)] %>%
		.[N<0,N:=0] %>%
		.[,N2:=N^2]



	#=== cell-level N mapping (sf) ===#
	f <- data %>%
	  left_join(field[,'cid'], ., by='cid')


	#=== N mapping ===#
	# {
	#   ggplot() +
	#     geom_sf(data = f, aes(fill =(N)), size=0.1) +
	#     ggtitle(paste0(temp_length, " cells")) +
	#     scale_fill_gradient(low="white", high="black",
	#                         guide=guide_colorbar(frame.colour="black")) +
	#     theme(
	#       plot.title = element_text(hjust = 0.5),
	#       legend.position='right',
	#       legend.key.size = unit(0.4, 'cm'),
	#       legend.text = element_text(size=10),
	#       axis.text=element_text(color='black'))
	#   ggsave(file=paste0("./Graph/design_map_",
	#                      "cell_",temp_length,".png"),
	#          height=6,width=6.5)
	#   }
	# {
	#   ggplot() +
	#     geom_sf(data = f, aes(fill =(Ntg)), size=0.1) +
	#     ggtitle(temp_design) +
	#     scale_fill_grey(name="N rate \n(kg/ha)", start=1, end=0) +
	#     # scale_fill_viridis_d(name="N rate (kg/ha)", direction = -1) +
	#     theme(
	#       plot.title = element_text(hjust = 0.5),
	#       legend.position='right',
	#       legend.key.size = unit(0.4, 'cm'),
	#       legend.text = element_text(size=10),
	#       axis.text=element_text(color='black'))
	#     ggsave(file=paste0("./Graph/design_map_",
	#                        "cell_",temp_length,"_buffer.png"),
	#            height=6,width=6.5)
	# }



	# #=== plot-level N polygons ===#
	# Af <- aggregate(f, by=list(f$plot_id), FUN=mean)
	# Af <- cbind(Af, Af %>% st_centroid %>% st_coordinates) %>%
	#   as_Spatial()


	#/*~~~~~~~~~~~~~~~~~~~~~~*/
	#' ### Generate yield
	#/*~~~~~~~~~~~~~~~~~~~~~~*/
	data %<>% .[,det_yield:=gen_yield_QP(b0,b1,b2,Nk,N)] %>%
		#=== add error component ===#
		.[,yerror:=det_yield*m_error] %>%
		.[,yield:=det_yield*(1+m_error)] %>%
		#=== keep the relevant vars ===#
		.[,.(cid,N,N2,m_error,det_yield,yerror,yield,b0,b1,b2,N_star,Nk,plateau,opt_N,
			 buffer,aunit_id)]


	#/*~~~~~~~~~~~~~~~~~~~~~~*/
	#' ### Aggregate data by analysis unit
	#/*~~~~~~~~~~~~~~~~~~~~~~*/
	#=== by analysis unit ===#
	reg_data <- data[,.(yield=mean(yield),
	                             N=mean(N),N2=mean(N2),
								 b0=mean(b0),b1=mean(b1),b2=mean(b2),
								 Nk=mean(Nk)),
								 by=aunit_id] %>%
		left_join(au_sf,.,by='aunit_id') %>%
		#---buffer zone dropped
		na.omit()


	#/*~~~~~~~~~~~~~~~~~~~~~~*/
	#' ### SCAM
	#/*~~~~~~~~~~~~~~~~~~~~~~*/
	# scam_res <- gam(yield~s(N, k=5) + s(X, k = 6) + s(Y, k = 6) + ti(X, Y, k = 6), data=reg_data)
	#
	# opt_N_scam <- data.table(N=seq(min(N_levels),max(N_levels),length=1000)) %>%
	# 	.[, X := reg_data[1, ] %>%  pull(X)] %>%
	# 	.[, Y := reg_data[1, ] %>%  pull(Y)] %>%
	# 	.[, yhat := predict(scam_res,newdata=.)] %>%
	# 	.[, profit := pCorn*yhat-pN*N] %>%
	# 	.[profit == max(profit), N] %>%
	# 	max(min(N_levels),.) %>%
	# 	min(max(N_levels),.)


	#/*~~~~~~~~~~~~~~~~~~~~~~*/
	#' ### GWR
	#/*~~~~~~~~~~~~~~~~~~~~~~*/
	reg_data_sp <- reg_data %>%
		as('Spatial')

	reg_formula <- formula(yield~N+N2)

	#=== gwr estimation with bw ===#
	gwr_est <- gwr.basic(
		reg_formula,
		data=reg_data_sp,
		bw=100,
		kernel="gaussian",
		adaptive=T
	)

	#=== estimated optimal N rates ===#
	gwr_beta <- data.table(
		aunit_id=reg_data$aunit_id,
		b0_hat = gwr_est$SDF$Intercept,
		b1_hat = gwr_est$SDF$N,
		b2_hat = gwr_est$SDF$N2
	) %>%
	    #... concave responses
	    .[b2_hat<0&b1_hat>0, opt_N_gwr := (b1_hat-pN/pCorn)/(-2*b2_hat)] %>%
	    #... convex responses: corner solution
	    .[b2_hat>=0|b1_hat<=0, yield_left :=
	          gen_yield_QD(b0_hat,b1_hat,b2_hat,min(N_levels))] %>%
	    .[b2_hat>=0|b1_hat<=0, pi_left :=
	          pCorn*yield_left - pN*min(N_levels) - fixed_cost] %>%
	    .[b2_hat>=0|b1_hat<=0, yield_right :=
	          gen_yield_QD(b0_hat,b1_hat,b2_hat,max(N_levels))] %>%
	    .[b2_hat>=0|b1_hat<=0, pi_right :=
	          pCorn*yield_right - pN*max(N_levels) - fixed_cost] %>%
	    .[b2_hat>=0|b1_hat<=0, opt_N_gwr :=
	          as.numeric(pi_left>pi_right)*min(N_levels) +
	          as.numeric(pi_left<=pi_right)*max(N_levels) ] %>%
	    #... limit the range of opt_N_gwr
	    .[, opt_N_gwr:=pmin(opt_N_gwr, max(N_levels))] %>%
	    .[, opt_N_gwr:=pmax(opt_N_gwr, min(N_levels))]


	#=== spatial interpolate the buffer zone cells ===#
	# gwr_beta <- fn_buffer_interpolate(full_map=f,
	#                                   full_data=data,
	#                                   est_data=gwr_beta,
	#                                   varname="opt_N_gwr")


	#' #/*~~~~~~~~~~~~~~~~~~~~~~*/
	#' #' ### Random Forest
	#' #/*~~~~~~~~~~~~~~~~~~~~~~*/
	#' library(grf)
	#'
	#' #=== use perfect info ===#
	#' X <- reg_data %>% data.table() %>% .[, c("N","b0","b1","b2","Nk"), with = FALSE] %>% data.frame()
	#' Y <- reg_data %>% data.table() %>% .[, yield]
	#' BRF_temp <- boosted_regression_forest(
	#'   X = X,
	#'   Y = Y,
	#'   num.trees = 2000,
	#'   min.node.size = 10)
	#' N_seq <- seq(min(N_levels), max(N_levels), by = 2)
	#' data_test <- reg_data %>% data.table()
	#' data_rf_perfect <- data_test[, c('aunit_id',"b0","b1","b2","Nk", 'yield'), with = FALSE] %>%
	#'   .[rep(1:nrow(.), each = length(N_seq)), ] %>%
	#'   .[, N := rep(N_seq, nrow(.)/length(N_seq))] %>%
	#'   .[, yield_hat := predict(BRF_temp, newdata = .[, c("N","b0","b1","b2","Nk"), with = FALSE])] %>%
	#'   .[, pi_hat := pCorn * yield_hat - pN * N] %>%
	#'   .[, .SD[which.max(pi_hat)], by = aunit_id] %>%
	#'   .[, opt_N_rf_perfect:= N] %>%
	#'   .[, c('aunit_id', 'opt_N_rf_perfect')]
	#'
	#' #=== spatial interpolate the buffer zone cells ===#
	#' data_rf_perfect <- fn_buffer_interpolate(full_map=f,
	#'                                   full_data=data,
	#'                                   est_data=data_rf_perfect,
	#'                                   varname="opt_N_rf_perfect")
	data_rf_perfect=data.table(aunit_id=au_sf$aunit_id, opt_N_rf_perfect=200)


	#/*----------------------------------*/
	#' ## Economic analysis
	#/*----------------------------------*/

	data_return <- data %>%
	  gwr_beta[.,on='aunit_id'] %>%
	  data_rf_perfect[.,on='aunit_id'] %>%
	  #=== base yield (yield without N) ===#
	  .[,base_yield:=gen_yield_QP(b0,b1,b2,Nk,0)] %>%
	  #=== GWR raw ===#
	  .[,yield_gwr:=gen_yield_QP(b0,b1,b2,Nk,opt_N_gwr)] %>%
	  .[,pi_gwr:=pCorn*yield_gwr-pN*opt_N_gwr-fixed_cost] %>%
	  #=== rf ===#
	  .[,yield_rf_perfect:=gen_yield_QP(b0,b1,b2,Nk,opt_N_rf_perfect)] %>%
	  .[,pi_rf_perfect:=pCorn*yield_rf_perfect-pN*opt_N_rf_perfect-fixed_cost] %>%
	  #=== True Optimal ===#
	  .[,yield_opt:=gen_yield_QP(b0,b1,b2,Nk,opt_N)] %>%
	  .[,pi_opt:=pCorn*yield_opt-pN*opt_N-fixed_cost]

	# take field average
	data_return <- data_return %>%
	  .[,.(
	    opt_N_gwr=mean(opt_N_gwr, na.rm=T),
	    opt_N=mean(opt_N, na.rm=T),
	    opt_N_rf_perfect=mean(opt_N_rf_perfect, na.rm=T),
	    pi_gwr=mean(pi_gwr, na.rm=T),
	    pi_rf_perfect=mean(pi_rf_perfect, na.rm=T),
	    pi_opt=mean(pi_opt, na.rm=T)
	  )] %>%
	  #=== correlation between b and N
	  .[, cor_b1_N := cor(data$b1, data$N)] %>%
	  .[, cor_b2_N := cor(data$b2, data$N)] %>%
	  .[, cor_Nk_N := cor(data$Nk, data$N)] %>%
	  .[, cor_pl_N := cor(data$plateau, data$N)] %>%
	  .[, cor_e_N := cor(data$m_error, data$N)] %>%
	  .[, cor_ye_N := cor(data$yerror, data$N)] %>%
	  .[, sim:=i_sim]

	return(data_return)
}