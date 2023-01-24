source("colourscale.R")

plot_dp <- function(dens, dis, threshold = 0, Dim = 2){
	plot(log(dis), log(dens), pch = 19, cex = 0.5)

	logdist<-seq(from = min(log(dis)), to = range(dis,finite=T)[2], length.out=100);

	lines(logdist,-Dim * logdist + threshold,lty=2,col="black")	
}

plot_dat <- function(dat, cl = NULL, outliers = NULL){

	if(is.null(cl)){
		cols <- "black"
	}else{
		n_cl <- max(cl)
		cols <- viridis(n = n_cl)[cl]
	}

	if(is.null(outliers)){
		thresh <- rep(T, nrow(dat))
	}else{
		thresh <- outliers
	}


	plot(dat, pch = 1, col = cols, ann = F, xaxt = "n", yaxt = "n", cex = 0.5)
	points(dat[thresh,], pch = 19, col = cols[thresh], cex = 0.5)
}
