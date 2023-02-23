library(mlbench)
library(MASS)

source("scripts/DBC_functions.R")
source("scripts/colourscale.R")

plot_fig1 <- function(){
	set.seed(345)
	
	par(mfrow = c(1, 2))

	dat <- mlbench.2dnormals(n = 2000, cl = 2, r = 4, sd = 1)$x
	plot(dat, col = "blue4", pch = 19, ann = F, xaxt = "n", yaxt = "n", cex = 0.5)

	dens <- kde2d(x = dat[,1], y = dat[,2])
	persp(dens, col = "blue4", border = "white", ylab = "", xlab = "", zlab = "Density")
		
}




plot_fig2 <- function(){

	par(mfrow = c(1, 3), mar = c(5, 2, 5, 1))

	dat <- read.table("data/data.dat")

	euc_dist <- as.matrix(dist(dat, upper = T))

	for(k in c(2, 200, 1000)){
		dens <- calculate_density(euc_dist, KNN = k)

		gridsize <- 25

		x_break <- seq(from = min(dat[,1]), to = max(dat[,1]), length.out = gridsize)
		y_break <- seq(from = min(dat[,2]), to = max(dat[,2]), length.out = gridsize)
			
		x <- cut(dat[,1], x_break, labels = F)
		y <- cut(dat[,2], y_break, labels = F)

		mat <- matrix(0, nrow = gridsize, ncol = gridsize)
		
		bin_dens <- aggregate(dens, by = list(x, y), FUN = mean)
		
		for(i in 1:nrow(bin_dens)) mat[bin_dens[i, 1], bin_dens[i, 2]] <- bin_dens[i, 3]

		persp(x = x_break, y = y_break, z = mat, col = "blue4", ylab = "", xlab = "", zlab = "Density", phi = 0, theta = 10, border = "white", lwd = 1, cex.lab = 3)
		mtext(text = paste("k =", k), side = 3, line = 0, cex = 3)
		
}


}

