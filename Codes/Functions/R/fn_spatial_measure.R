


# create the list of spatial property measures
# N.shp: plot-level N rate polygon shapefile
# Nid: N treatment rate id (1, 2, 3, 4, 5, 6)



fn_spatial_measure <- function(N.shp){
	
	# ---------------------------
	#'  Minimum Spanning Tree
	# ---------------------------
	# Piepho et al (2018): "degree of spreading" by Tedin (1931)
	
	#=== MST for each treatment ===#
	MST <- c()
	for(trt in 1:6){
		#spatial polygons
		pg <- N.shp[N.shp$Nid==trt,]
		#k-nearest neighbor list
		knn <- knearneigh(coordinates(pg), k=nrow(pg)-1)
		pg.nb <- knn2nb(knn)
		#use plot row/column id as weights
		dpad <- as.data.frame(pg)[,c("plot_col_id","plot_row_id")]
		#calculating costs
		lcosts <- nbcosts(pg.nb, dpad)
		#listw
		nb.w <- nb2listw(pg.nb, lcosts, style="B")	
		# Note: coordinates for euclidean distance calculation (data patronized); 
		#		they are the original coords of points, so don't worry about the
		#		switching centroids of voronoi polygons.
		
		#=== Minimum spanning tree ===#
		mst.pg <- mstree(nb.w, ini=1)
		#=== mean length of edges
		MST[trt] <- mean(mst.pg[,3])
	}
	MSTmin <- min(MST)
	
	#=== mstree plot ===#
	# par(mar=c(4,4,4,4))
	# plot(pg, axes=TRUE)
	# plot(mst.pg, coordinates(pg), col=2,
	# 	 cex.lab=1.5, cex.circles=1, fg="blue", add=TRUE)
	# mtext(temp_design, side=3, line=1)
	
	
	# ---------------------------
	#'  Gradation
	# ---------------------------
	# measure the smoothness of the trial rates distribution
	# gradual change in the same direction, or goes up and down
	# formula: g_i = (N_i - N_i-1)*(N_i+1 - N_i)
	#		positive: gradual; negative: up and down
	
	#===gradation measure===#
	N.shp <- data.frame(N.shp) %>% data.table() %>%
	    .[order(plot_row_id, plot_col_id),] %>%
	    #===by row===#
	    .[, N_lag:=c(NA,Nid[-.N]), by=plot_row_id] %>%
	    .[, N_fwd:=c(Nid[-1],NA), by=plot_row_id] %>%
	    .[,grad_row:=sign(Nid-N_lag)*sign(N_fwd-Nid)] %>%
	    .[is.na(grad_row),grad_row:=0] %>%
	    #===by column===#
	    .[, N_lag:=c(NA,Nid[-.N]), by=plot_col_id] %>%
	    .[, N_fwd:=c(Nid[-1],NA), by=plot_col_id] %>%
	    .[,grad_col:=sign(Nid-N_lag)*sign(N_fwd-Nid)] %>%
	    .[, c("plot_id","grad_row","grad_col")] %>%
	    .[is.na(grad_col),grad_col:=0] %>%
	    merge(N.shp, ., by="plot_id")
	GR_row <- mean(N.shp$grad_row, na.rm=T)
	GR_col <- mean(N.shp$grad_col, na.rm=T)
	
	
	# ---------------------------
	#'  Neighbor Balance (NB)
	# ---------------------------
	
	#===all treatment pairs===#
	P_all <- expand.grid(x1=1:6,x2=1:6) %>% data.table() %>%
		#===duplicated pairs removed===#
		.[, pair_1:=pmin(x1,x2)] %>%
		.[, pair_2:=pmax(x1,x2)] %>%
		.[, c("pair_1","pair_2")] %>%
		unique()
	
	#===adjacent plots counting===#
	P_row <- data.frame(N.shp) %>% data.table() %>%
		.[order(plot_row_id, plot_col_id),] %>%
		#===by row===#
		.[, N_left:=Nid] %>%
		.[, N_right:=c(Nid[-1],NA), by=plot_row_id] %>%
		#===duplicated pairs===#
		.[, pair_1:=pmin(N_left,N_right)] %>%
		.[, pair_2:=pmax(N_left,N_right)] %>%
		.[, c("pair_1","pair_2")]
	P_col <- data.frame(N.shp) %>% data.table() %>%
		.[order(plot_row_id, plot_col_id),] %>%
		#===by column===#
		.[, N_up:=Nid] %>%
		.[, N_down:=c(Nid[-1],NA), by=plot_col_id] %>%
		#===duplicated pairs===#
		.[, pair_1:=pmin(N_up,N_down)] %>%
		.[, pair_2:=pmax(N_up,N_down)] %>%
		.[, c("pair_1","pair_2")]
	P_diag_1 <- data.frame(N.shp) %>% data.table() %>%
		.[, plot_diag_id:=plot_col_id-plot_row_id] %>%
		#===by diagonal NW-SE===#
		.[, N_up:=Nid] %>%
		.[, N_down:=c(Nid[-1],NA), by=plot_diag_id] %>%
		#===duplicated pairs===#
		.[, pair_1:=pmin(N_up,N_down)] %>%
		.[, pair_2:=pmax(N_up,N_down)] %>%
		.[, c("pair_1","pair_2")]
	P_diag_2 <- data.frame(N.shp) %>% data.table() %>%
		.[, plot_diag_id:=max(plot_col_id)-plot_col_id-plot_row_id] %>%
		#===by diagonal NE-SW===#
		.[, N_up:=Nid] %>%
		.[, N_down:=c(Nid[-1],NA), by=plot_diag_id] %>%
		#===duplicated pairs===#
		.[, pair_1:=pmin(N_up,N_down)] %>%
		.[, pair_2:=pmax(N_up,N_down)] %>%
		.[, c("pair_1","pair_2")]
	P <- rbind(P_row, P_col, P_diag_1, P_diag_2) %>% 
		na.omit() %>%
		#===count pairs===#
		.[, .(count=.N), by=.(pair_1,pair_2)] %>%
		#===join all pairs===#
		.[P_all, on=.(pair_1,pair_2)] %>%
		.[is.na(count),count:=0]
	# Pcount <- table(P$count)
	# names(Pcount) <- paste0("n_",names(Pcount))
	# NB_list[[ith]] <- Pcount
	#===use Gini coefficient to measure NB===#
	library(reldist)
	NB_gini <- gini(P$count)
	
	
	# ---------------------------
	#' van Es spatial balance
	# ---------------------------
	
	#===all possible plot pairs===#
	df <- N.shp@data[, c('plot_id','Nid')]
	df$lon <- coordinates(N.shp)[,1]
	df$lat <- coordinates(N.shp)[,2]
	
	h_df <- expand.grid(id_1=1:nrow(df), id_2=1:nrow(df)) %>% 
	  data.table() %>%
	  #===remove duplicated trt pairs===#
	  .[, plot_1:=pmin(id_1,id_2)] %>%
	  .[, plot_2:=pmax(id_1,id_2)] %>%
	  .[plot_1!=plot_2, ] %>%
	  unique(., by=c('plot_1','plot_2')) %>%
	  .[, c('id_1','id_2')] %>%
	  #===match lon/lat to id_1===#
	  .[, plot_id:=id_1] %>%
	  .[df, on='plot_id'] %>%
	  setnames(., c('Nid','lon','lat'), c('trt_1','lon_1','lat_1') ) %>%
	  #===match lon/lat to id_2===#
	  .[, plot_id:=id_2] %>%
	  .[df, on='plot_id'] %>%
	  setnames(., c('Nid','lon','lat'), c('trt_2','lon_2','lat_2') ) %>%
	  .[, plot_id:=NULL] %>%
	  na.omit() %>%
	  #===remove same-value trt pairs===#
	  .[trt_1!=trt_2, ] %>%
	  #===distance===#
	  .[, h:=sqrt((lon_1-lon_2)^2+(lat_1-lat_2)^2)] %>%
	  #===trt contrasts===#
	  .[, value_1:=pmin(trt_1,trt_2)] %>%
	  .[, value_2:=pmax(trt_1,trt_2)] %>%
	  .[, c('value_1','value_2','h')] %>%
	  setnames(., c('value_1','value_2'), c('trt_1','trt_2')) %>%
	  .[order(trt_1,trt_2),]
	
	#===average distance by trt constrast===#
	m_df <- h_df[, .(h=mean(h)), by=c('trt_1','trt_2')]
	van_Es_var <- sd(m_df$h)
	
	
	# ---------------------------
	#'  Moran's I
	# ---------------------------
	
	# # contiguity weights matrix (queen)
	# nb <- poly2nb(N.shp, queen=TRUE)      # neighborhood list 
	# Wls <- nb2listw(nb, style="B")     # listw
	
	# k-nearest neighbor list
	knn <- knearneigh(coordinates(N.shp), k=8)
	nb <- knn2nb(knn)  # neighborhood list 
	Wls <- nb2listw(nb, style="B")     # listw
	
	
	# global Moran's I
	moran_I <- moran.test(N.shp$Nid, Wls)$estimate[1]

	
	
	# ---------------------------
	#'  Return results
	# ---------------------------
	SB_result <- list(MSTmin, GR_row, GR_col, NB_gini, moran_I, van_Es_var)
	names(SB_result) <- c('MSTmin', 'GR_row', 'GR_col', 'NB_gini', 'moran_I', 'van_Es_var')
	return(SB_result)
	
}







