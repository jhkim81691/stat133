load("ex1.rda")

# We have loaded the data frame "df" into your workspace. It contains the
# following variables:
#
# > names(df)
# [1] "x1" "x2" "x3" "x4" "f" 
#
# The first 4 columns are measurements and the last column "f" codes
# which of 5 groups each row belongs to.


# (1 point)
# Create a subset of "df" containing only the measurement columns.
# Save this as <df.x>

# df.x = your code here

### PCA

# (1 point)
# Create a prcomp object using the measurement data and call it pca

# pca = your code here

# (1 point)
# Create a matrix pca.x which is the data projected onto it first 2
# principal components

# pca.x = your code here


## kmeans

# (4 points)
# Perform k-means clustering on pca.x with k=5 and 30 randomly
# choosen starting points.  Save this as <km>.

set.seed(42)
# km = your code here

# Now using <km>, which you just made, you will need to create
# several more variables.  Save the labels prouced for each
# row of pca.x as <km.lables>. Save the matrix of cluster
# centers as <km.centers>.  Save the number of points in each
# cluster as <km.counts>.

# km.lables = your code here
# km.centers = your code here
# km.counts = your code here

## Plots

# (5 point)

# You should make 2 plots in one figure.  Make sure that they
# are side by side.  Both plots should show the points of
# pca.x plotted such that PC1 is on the x-axis.
# The first plot should have the colors set based on the
# true lables (i.e., col=df$f) and should have the
# title "true".
# The second plot should have the points colored according
# to km.labels and should have the title "kmeans".
# The second plot should also have the km.centers plotted
# on top as 'x's. Use any colors for your plots

# your code here
