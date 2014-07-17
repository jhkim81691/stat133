library(RUnit)
errMsg <- function(err) print(err)
load('ex1-tests.rda')

# Implement the function dataDist. Your function should take the following
# arguments:
#
# <data>: a dataframe whose variables may be of any type.
# <norm>: the method used to measure distance. This must be one of
#   'euclidean' (default), 'maximum', 'manhattan', 'canberra', 'binary', or 
#   'minkowski' 
#
# Your function should return:
#
# A dissimilarity matrix for all numeric variables in <data>. This should
# match the type of argument that can be used as an argument to R's "hclust"
# function.

dataDist <- function(data, norm='euclidean') {
	data.is.numeric <- sapply( 1:ncol(data), function(x) is.numeric(data[, x]))
	dis.matrix <- dist(data[, data.is.numeric], method=norm)
	
    return(dis.matrix)
}

tryCatch(checkEquals(c(data.dist.t), c(dataDist(iris))), error=function(err)
         errMsg(err))


# Implement the function "clustLabel". Your function should take the
# following arguments:
#
# <data>: a dataframe whose variables may be of any type.
# <norm>: the method used to measure distance. This must be one of
#   'euclidean' (default), 'maximum', 'manhattan', 'canberra', 'binary', or
#   'minkowski'
# <k>: a numeric value specifying the number of clusters
#
# Your function should return:
#
# A vector of group labels (integers ranging from 1 to <k>) for each
# observation in <data>
#
# HINT: you may find the cutree function useful

clustLabel <- function(data, norm='euclidean', k) {
	data.hclust <- hclust(dataDist(data, norm))
	data.cutree <- cutree(data.hclust, k=k)
	
    return(data.cutree)
}

tryCatch(checkEquals(clust.label.t, clustLabel(iris, k=3)),
         error=function(err) errMsg(err))


# Implement the function evalClusters. Your function should take the
# following arguments:
#
# <data>: a dataframe whose variables may be of any type.
# <true.labels>: a factor vector giving the "true" label for each
#   observation in <data>
# <norm>: the method used to measure distance. This must be one of
#   'euclidean' (default), 'maximum', 'manhattan', 'canberra', 'binary', or
#   'minkowski'
# <k>: a numeric value specifying the number of clusters
#
# Your function should run hierarchical clustering on <data> with distance
# defined by <norm> and label each observation as one of <k> groups. For
# each cluster 1 to <k>, your function should determine the most represented
# factor level. Your function should return a numeric vector indicating the
# fraction of observations in each cluster from that factor level.
#
# HINT: running the command names(which.max(table(vector))) will tell you the
# name of the factor that occurs most frequently in vector

evalClusters <- function(data, true.labels, norm='euclidean', k) {
	data.cutree <- clustLabel(data, norm=norm, k=k)
	data.clusters <- lapply( 1:k, function(x) data.cutree==x)
	
	data.levels <- unclass(true.labels)
	data.match.label <- lapply( 1:k, function(x) data.levels[data.clusters[[x]]])
	data.frequent <- as.numeric(sapply( 1:k, function(x) names(which.max(table(data.match.label[[x]])))))
	
	factor.frac.logical <- sapply( 1:k, function(x) data.levels[data.clusters[[x]]] == data.frequent[x])
	factor.frac <- sapply( 1:k, function(x) sum(factor.frac.logical[[x]]) / length(factor.frac.logical[[x]]))
    return(factor.frac)
}

tryCatch(checkEquals(eval.clusters.t, evalClusters(iris, iris$Species, k=3)),
         err=function(err) errMsg(err)) 


# Implement the function heightCluster. Your function should take the
# following arguments:
#
# <data>: a dataframe whose variables may be of any type.
# <norm>: the method used to measure distance. This must be one of
#   'euclidean' (default), 'maximum', 'manhattan', 'canberra', 'binary', or
#   'minkowski'
# <h>: a numeric value specifying the height at which to cut the tree. 
# <...>: additional parameters to pass to your plot
#
# Your function should return:
#
# An integer vector of group labels for each observation in <data>
# determined by cutting an 'hclust' tree at height <h>. In addition, your
# function should produce a cluster dendrogram with a horizontal red line
# at <h>.

heightCluster <- function(data, norm='euclidean', h, ...) {
    data.hclust <- hclust(dataDist(data, norm))
	data.cutree <- cutree(data.hclust, h=h)
	
	plot(data.hclust, ...)
	abline(h = h, col="red")
	
	return(data.cutree)
}

tryCatch(checkEquals(height.cluster.t, heightCluster(iris, h=4, cex=0.2)),
         error=function(err) errMsg(err))
