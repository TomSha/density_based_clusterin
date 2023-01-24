library(mlbench)
library(MASS)


plot_fig1 <- function(){
	set.seed(345)
	
	par(mfrow = c(1, 2))

	dat <- mlbench.2dnormals(n = 2000, cl = 2, r = 4, sd = 1)$x
	plot(dat, col = "blue", pch = 19, ann = F, xaxt = "n", yaxt = "n", cex = 0.5)

	dens <- kde2d(x = dat[,1], y = dat[,2])
	persp(dens, col = "blue", ylab = "", xlab = "", zlab = "Density")
		
}
