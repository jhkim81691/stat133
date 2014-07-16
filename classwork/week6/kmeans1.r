############################
##  Simulate data
############################

# set vars
n = list()
n$groups = 5
n$rep = 9
n$points = n$rep * n$groups
cols = 1:5
grps = rep(1:n$groups, each=n$rep)

# make the RNG run from a fixed start 
set.seed(42)

# create high, moderate, and low variance clustersbundle up data w/ a list
d1 = list()
d2 = list()

x.means = c(1, 3, 2, 1, 3)
y.means = c(1, 1, 2, 3, 3)
d1$means = x.means[grps]
d2$means = y.means[grps]

d1$high = rnorm(n$points, d1$means, sd=1)
d2$high = rnorm(n$points, d2$means, sd=1)

d1$moderate = rnorm(n$points, d1$means, sd=0.3)
d2$moderate = rnorm(n$points, d2$means, sd=0.3)

d1$low = rnorm(n$points, d1$means, sd=0.1)
d2$low = rnorm(n$points, d2$means, sd=0.1)

############################
##  Plot simulated data
############################

# Helper function to plot data in d1 vs d2 space
plotSim1 = function(x, y, title="Cluster analysis") {
    plot(x, y, pch=19, col="blue", main=paste(title, "variance"))
}

# Helper function to plot data in d1 vs d2 space (w/ labels)
plotSim = function(x, y, title="Cluster analysis", labels=cols[grps]) {
    plot(x, y, pch=19, col=labels, main=title)
}

# Plot w/out labels
plotSim1(d1$high, d2$high, "high")
plotSim1(d1$moderate, d2$moderate, "moderate")
plotSim1(d1$low, d2$low, "low")

# Plot w/ labels
plotSim(d1$low, d2$low, "low")
plotSim(d1$moderate, d2$moderate, "moderate")
plotSim(d1$high, d2$high, "high")


############################
##  Hierarchical Clustering
############################

df = data.frame(x=d1$moderate, y=d2$moderate)

# Use the wss to help assess the number of clusters
#wss = sapply(1:15, function(i) sum(kmeans(df, centers=i)$withinss))
wss = sapply(1:15, function(i) kmeans(df, centers=i)$tot.withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
  ylab="Within groups sum of squares")

# how sensitive is this to the initial starting point
wss = sapply(1:15, function(i) kmeans(df, centers=i, nstart=10)$tot.withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
  main="Best of 10 runs w/ different starting points",
  ylab="Within groups sum of squares")


# perform k-means cluster analysis
fit = kmeans(df, 5)        # 5 cluster solution

# get cluster assignments
fit$cluster

# get cluster means
fit$centers

# how would you do this if kmeans didn't return this?
aggregate(df, by=list(fit$cluster), FUN=mean)

# plot the cluster means on top of the data
par(mfrow=c(1,2))
plotSim(d1$moderate, d2$moderate, "moderate (true labels)")
points(fit$centers, pch='x')

plotSim(d1$moderate, d2$moderate, "moderate (inferred labels)", cols[fit$cluster])
points(fit$centers, pch='x')

# perform k-means cluster analysis
fit = kmeans(df, 5, nstart=10)        # 5 cluster solution

# get cluster assignments
fit$cluster

# get cluster means
fit$centers

# how would you do this if kmeans didn't return this?
aggregate(df, by=list(fit$cluster), FUN=mean)

# plot the cluster means on top of the data
par(mfrow=c(1,2))
plotSim(d1$moderate, d2$moderate, "moderate (true labels) best of 10")
points(fit$centers, pch='x')

plotSim(d1$moderate, d2$moderate, "moderate (inferred labels) best of 10", cols[fit$cluster])
points(fit$centers, pch='x')

# append cluster assignment
df = data.frame(df, fit$cluster)
