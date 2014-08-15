library(RUnit)
errMsg <- function(err) print(err)


# Load some data
load("ex3.rda")

# This loads a dataframe named df where:
#
# > class(df)
# [1] "data.frame"
# > summary(df)
#        x1               x2               x3               x4       
#  Min.   :0.7047   Min.   :0.7335   Min.   :0.8113   Min.   :0.754  
#  1st Qu.:1.7146   1st Qu.:1.5528   1st Qu.:1.0427   1st Qu.:1.693  
#  Median :1.9995   Median :2.0237   Median :1.9993   Median :1.993  
#  Mean   :2.0080   Mean   :2.0207   Mean   :2.0057   Mean   :1.998  
#  3rd Qu.:2.3162   3rd Qu.:2.4524   3rd Qu.:2.9707   3rd Qu.:2.304  
#  Max.   :4.6381   Max.   :6.0513   Max.   :3.3255   Max.   :3.290  
#  NA's   :2        NA's   :1                                        

# You first task is to implement some helper functions, which you
# will later use for "cleaning" df (i.e., you will remove rows
# of df using these functions).

# (4 point) Implement the too.many.na function
# 
# Takes the following arguments:
#   df: a numeric dataframe
#   threshold: a number between 0 and 1
#   axis: 1 or 2 (i.e., rows or columns)
#
# Returns an unnamed numeric vector containing the indices of the
# rows (if 'axis' is 1) or columns (if 'axis' is 2) whose proportion 
# of NA values is strictly greater than the 'threshold'.

too.many.na = function(df, threshold, axis) {
    # your code here
}

testdf1 = data.frame(1:4, c(NA,1,2,3))
tryCatch(checkEquals(1, too.many.na(testdf1, 0, 1)),
         error=function(err) errMsg(err))

tryCatch(checkEquals(2, too.many.na(testdf1, 0, 2)),
         error=function(err) errMsg(err))

tryCatch(checkEquals(0, length(too.many.na(testdf1, 0.6, 1))),
         error=function(err) errMsg(err))

# (4 point) Implement the outlier.cutoff function
# 
# Takes the following arguments:
#   x: a numeric vector
#   rm.na: TRUE or FALSE, if true, any ‘NA’ and ‘NaN’'s are removed
#          from ‘x’ before the quantiles are computed.
#
# Returns a numeric vector containing two elements. The first element is the
# lower cutoff. The second element is the upper cutoff.  For the purpose of the
# exam, the lower and upper cutoffs are defined as follows:
#    lower cutoff = Q1 - 2.5*IQR
#    upper cutoff = Q3 + 2.5*IQR
# where
#    Q1 is the first quartile or the 25th percentile
#       (i.e., splits off the lowest 25% of data from the highest 75%)
#    Q3 is the third quartile or the 75th percentile
#       (i.e., splits off the highest 25% of data from the lowest 75%)
#    IQR is the difference Q3 - Q1

outlier.cutoff = function(x, rm.na){
    # your code here
}

set.seed(42)
x = c(100, rnorm(100))
cuts = c(-3.829464, 3.899827)

tryCatch(checkEquals(cuts, unname(outlier.cutoff(x, FALSE)), tolerance=1e-6),
         error=function(err) errMsg(err))

x = c(x, NA)
tryCatch(checkException(outlier.cutoff(x, FALSE), silent=TRUE),
         error=function(err) pass)

tryCatch(checkEquals(cuts, unname(outlier.cutoff(x, TRUE)), tolerance=1e-6),
         error=function(err) errMsg(err))

# (4 point) Implement the remove.outliers function
# 
# Takes the following arguments:
#   df: a numeric dataframe
#   cuts: a matrix as returned by outlier.cutoff
#
# Returns a subset of df with all the rows with elements outside
# the cutoffs in <cuts> removed (i.e., remove rows with element which
# are strictly greater than the upper bound or strictly less than
# the lower bound) 

remove.outliers = function(df, cuts) {
    # your code here
}

# Now you are going to create some new datastructures by calling these functions
# on df.

# (1 point) Create a vector na.indices
#
# This is a vector containing the rows of df with any NAs.  You can use the
# function too.many.na, which you created above.

# na.indices = your code here

# (1 point) Create a dataframe df.no.nas
#
# This should be a subset of df with all rows containing any NAs removed.
# You can use na.indices for this step.

# df.no.nas = your code here

# (1 point) Create a matrix cuts
#
# Each column of cuts should be the result of calling outlier.cutoff on
# the corresponding column of df.no.nas. You can set rm.na to TRUE or
# FALSE since df.no.nas shouldn't have any NAs.
#
# > dim(cuts)
# [1] 2 4

# cuts = your code here

# (1 point) Create a dataframe df.clean
#
# This is a subset of df with all rows containing any NAs removed
# as well as any rows with elements outside the cutoffs specified
# by cuts removed.

# df.clean = your code here
