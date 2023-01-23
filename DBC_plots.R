source("colourscale.R")

plot_dp <- function(dens, dis){
	plot(log(dis), log(dens), pch = 19, cex = 0.5)

	logdist<-seq(from = min(log(dis)), to = range(dis,finite=T)[2], length.out=100);

	lines(logdist,-2*logdist ,lty=2,col="black")	
}

plot_dat <- function(dat, cl = NULL, outliers = NULL){

	if(is.null(cl)){
		cols <- "black"
	}else{
		n_cl <- max(cl)
		cols <- rainbow(n_cl)[cl]
	}

	if(is.null(outliers)){
		thresh <- rep(T, nrow(dat))
	}else{
		thresh <- outliers
	}


	plot(dat, pch = 1, col = cols, ann = F, xaxt = "n", yaxt = "n", cex = 0.5)
	points(dat[thresh,], pch = 19, col = cols[thresh], cex = 0.5)
}
