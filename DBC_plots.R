
plot_dp <- function(dens, dis){
	plot(log(dis), log(dens), pch = 19, cex = 0.5)

	logdist<-seq(log(dis)[1],range(dis,finite=T)[2],length.out=100);
	logdist<-seq(-10,20)

	lines(logdist,-2*logdist ,lty=2,col="black")
	
}
