############################
##  Simulate data
############################

# set vars
n = list()
n$groups = 3
n$rep = 4
n$points = n$rep * n$groups
cols = c("blue", "red", "green")
grps = rep(1:3, each=n$rep)

# make the RNG run from a fixed start 
set.seed(42)

# create high, moderate, and low variance clustersbundle up data w/ a list
d1 = list()
d2 = list()

d1$means = grps 
d2$means = rep(c(1,2,1), each=n$rep)

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
    text(x, y, labels=as.character(1:n$points), pos=1, offset=0.5)
}

# Helper function to plot data in d1 vs d2 space (w/ labels)
plotSim = function(x, y, title="Cluster analysis") {
    plot(x, y, pch=19, col=cols[grps], main=paste(title, "variance"))
    text(x, y, labels=as.character(1:n$points), pos=1, offset=0.5)
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

# get all pair-wise distances
df = data.frame(d1=d1$moderate, d2=d2$moderate)
distances = dist(df)  # Euclidean
plotIt = function(v) plotSim(d1[[v]], d2[[v]], v)

# agglomerative (i.e., bottom-up) hierachical clustering
hc = list()
hc$complete = hclust(distances)
plot(hc$complete, main = "Cluster Dendrogram (moderate variance)")

# how does it do on high variance
df1 = data.frame(d1=d1$high, d2=d2$high)
distances.high = dist(df1)  # Euclidean
hc$complete1 = hclust(distances.high)
plot(hc$complete1, main = "Cluster Dendrogram (high variance)")

par(mfrow = c(2, 2))  # 4 plots
plotIt("moderate")       # top left
plot(hc$complete)        # top right
abline(h=1.5, col='red')
plotIt("high")           # bottom left
plot(hc$complete1)       # bottom right
abline(h=3.5, col='red')

hc$average = hclust(distances, method="average")
hc$median = hclust(distances, method="median")
par(mfrow = c(2, 2))  # 4 plots
plotIt("moderate")       # top left
plot(hc$average)         # top right
abline(h=1, col='red')
plotIt("moderate")       # bottom left
plot(hc$median)          # bottom right
abline(h=1, col='red')

manhattan.distances = dist(df, method="manhattan")
maximum.distances = dist(df, method="maximum")
hc$complete.man = hclust(manhattan.distances)
hc$complete.max = hclust(maximum.distances)
par(mfrow = c(2, 2))  # 4 plots
plotIt("moderate")       # top left
plot(hc$complete.man)         # top right
abline(h=1, col='red')
plotIt("moderate")       # bottom left
plot(hc$complete.max)          # bottom right
abline(h=1, col='red')

manhattan.distances = dist(df1, method="manhattan")
maximum.distances = dist(df1, method="maximum")
hc$complete.man = hclust(manhattan.distances)
hc$complete.max = hclust(maximum.distances)
par(mfrow = c(2, 2))  # 4 plots
plotIt("high")       # top left
plot(hc$complete.man)         # top right
abline(h=1, col='red')
plotIt("high")       # bottom left
plot(hc$complete.max)          # bottom right
abline(h=1, col='red')
