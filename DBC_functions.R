
library(mlbench)


dat <- mlbench.2dnormals(n = 3000, cl = 3, r = 3, sd = 1)
cl_id <- dat$classes
dat <- dat$x


############################################################################
# calculates euclidean distance between rows of a data matrix
#
# Input :
# dat : a data matrix
# 
# Output
# euc_dist : the euclidean distance matrix

calculate_dist <- function(dat){
	euc_dist <- as.matrix(dist(dat, upper = T))
	return(euc_dist)
}
############################################################################


############################################################################
# calculate the local density of each data point
#
# Input :
# euc_dist : is the distance matrix of the data points
# KNN : is the integer of K nearest neighbours used to calculate the density
#
# Output :
# dens : a vector of the local density for each data point
# kNN_dist : a vector of the distance to the kth nearest neighbour

calculate_density <- function(euc_dist, KNN){

	# for each data point find the dist to KthNN
	kNN_dist <- apply(euc_dist, 2, function(x) sort(x)[(KNN + 1)])

	# find the local density for each point
	dens <- 1 / kNN_dist ^ ncol(dat)
	returnList <- list(dens = dens, kNN_dist = kNN_dist)
	return(returnList)
}
###########################################################################
	

############################################################################
# calculate the nearest neighbour of higher density (NNHD) for each data point
#
# Input :
# euc_dist : is the distance matrix of the data points
# dens : a vector of the local density for each data point
#
# Output :
# distance : a vector of the distance to the NNHD for each data point
# NNDH : a vector of who is the NNHD for each data point

calculate_NNHD <- function(euc_dist, dens){

	n_dat <- ncol(euc_dist)
	distance <- vector("list", length = n_dat)
	NNHD <- vector("list", length = n_dat)

	#find nearest neighbour of higher density
	for(i in 1 : n_dat){ 
		higher_density <- dens > dens[i]
	
		# if point i is the highest density point then set its distance to the maximum distance in the data and its NNHD to be itself
		if(sum(higher_density) == 0){
			distance[[i]] <- max(euc_dist)
			NNHD[[i]] <- i

		}else{

			distance[[i]] <- min(euc_dist[higher_density, i])
			NNHD[[i]] <- as.integer(which(euc_dist[, i] == distance[[i]]))
		}
	}
	
	distance<-unlist(distance)
	NNHD<-unlist(NNHD)

	returnList<-list("dis" = distance, "NNHD" = NNHD)
	return(returnList)
}
############################################################################


############################################################################
# Find the centers of the clusters and then assosciate the rest of the data points to one of those centers
#
# Input :
# 
# dens : a vector of the local density for each data point
# distance : a vector of the distance to the NNHD for each data point
# NNDH : a vector of who is the NNHD for each data point
# dat : data points
# threshold : the threshold for whether a point is considered a cluster centre. Higher values will result in fewer clusters
#
# Output :
# 
# clusters : a vector of which cluster a data point belongs to
# centers : a vector of which data points were assigned as centers

calculate_clusters<-function(dens, distance, NNHD, dat, threshold = 0){
	dimensionality <- ncol(dat)
	clusters <- rep(0, nrow(dat))

	#find the centers of each cluster 
	centers <- which(log(dens) > log(distance) * -dimensionality + threshold)

	for (i in 1 : length(centers)){
		clusters[centers[i]] <- i
	}

	#associate neurons to their correct centers
	while(any(clusters == 0)){
		clusters[clusters == 0] <- clusters[NNHD[clusters == 0]]
	}
	returnList <- list("clusters" = clusters, "centers" = centers)
	return(returnList)
}	
############################################################################


############################################################################
# Find the points considered outliers i.e. those that do not associate very well to any cluster 
#
# Input: 
# dat: data points
# euc_dist : is the distance matrix of the data points
# knn : an integer for k (the k nearest neighbours must be in the same cluster for a point NOT to be considered an outlier)
# clusters : a vector of which cluster each data point has been assigned to
# dens : a vector of the local density for each data point
#
# Output:
# outlier_thresh : a logical vector of whether a data point is considered an outlier

calculate_outliers <- function(dat, euc_dist, knn, clusters, dens){

	# find whether the kNNs are in the same cluster
	NN <- apply(euc_dist, 2, function(x) order(x)[1 : (knn + 1)])
	NN_clusters <- apply(NN, 2, function(x) clusters[x])
	NN_thresh <- apply(NN_clusters, 2, function(x) (min(x) - max(x)) == 0)

	# find the maximum density outlier for each cluster and use this density as a minimum density threshold
	n_cl <- max(clusters)
	min_dens <- rep(0, n_cl)

	for(i in 1 : n_cl){
		cl_outliers <- !NN_thresh & clusters == i

		if(sum(cl_outliers == 0)){
			min_dens[i] <- 0
		}else{
			min_dens[i] <- max(dens[cl_outliers])
	}
	dens_thresh <- dens > min_dens[clusters]

	outlier_thresh<-as.logical(NN_thresh*dens_thresh)
	return(outlier_thresh)

	}
}
############################################################################


DBC_function<-function(dat, KNN, threshold = 0, outliers = T, outlier_knn = 2){
	euc_dist <- calculate_dist(dat)
	dens <- calculate_density(euc_dist, KNN)
	dis <- calculate_NNHD(euc_dist, dens$dens)
	clusters <- calculate_clusters(dens$dens, dis$dis, dis$NNHD, dat, threshold)

	if(outliers){
		outlier_thresh<-calculate_outliers(dat, euc_dist, outlier_knn, clusters$clusters, dens$dens)	
	}else{
		outlier_thresh<-rep(T,nrow(dat))
	}

	returnList<-list("dens" = dens$dens, "dis" = dis$dis, "clusters" = clusters$clusters, "outliers" = outlier_thresh, "kNN_dist" = dens$kNN_dist)
	return(returnList)
}



