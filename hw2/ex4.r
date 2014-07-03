library(RUnit)
errMsg <- function(err) print(err)
load('ex4-tests.rda')

# Suppose you are wondering whether your dataset contains duplicated
# variables. However, the dataset is too large to search through by hand (this
# would very likely be the case with microarray data). Implement the function
# "identifyDuplicates" that searches over each pair of variables to determine if
# one is a duplicate of the other. Your function should take the following
# arguments:
#
# <data>: a data frame where the columns correspond to variables and the rows to
# observations 
#
# Your function should return:
#                                     
# <duplicate.pairs>: an **unnamed matrix** (check out the documentation for
#   as.matrix and unname) with dimensions num.duplicates x 2 with each row giving
#   both indices of any pair of duplicated variables. Please organize this matrix
#   as follows:
#
#   1) the index in the first column should be the lower of the two for each row
#   2) the indicies in the first column should be arranged in increasing order
#      (this means that the indices in the second column may not be ordered)
#   3) pairs that suggest a variable is a duplicate of itself should not be
#      included
#   4) there should only be one reference to each duplicated pair 
#   5) 
#
# if there are no duplicated pairs, your function should return the value
# numeric(0)
#
# HINT: you might find the identical, upper.tri/lower.tri, and which (with
# arr.ind=T) functions helpful (though they are not the only ways to do this)

identifyDuplicates <- function(data) {
	n <- ncol(data)
	duplicates <- sapply(1:(n-1), function(x)
		as.matrix(data[ ,x]==data[ ,(x+1):n]))
	
	l <- length(duplicates)
	duplicate.sum <- sapply(1:l, function(x) apply(duplicates[[x]], 2, sum))
	duplicate.check <- sapply(1:l, function(x) duplicate.sum[[x]] == 3)
	
	duplicate.pairs <- numeric(0)
	
	if (sum(sapply(1:l, function(x) sum(duplicate.check[[x]] ) )) > 0) {
		for (i in which(sapply(1:l, function(x) sum(duplicate.check[[x]]) > 0 ))) {
			duplicate.pairs <- rbind(duplicate.pairs, t(rbind(i, (which(duplicate.check[[i]]))+i) )) 
		}
		dimnames(duplicate.pairs) = NULL
		unname(duplicate.pairs, force=T)
	}
	return (duplicate.pairs)
}
    
tryCatch(checkEquals(numeric(0), identifyDuplicates(ex4.test1)),
         error=function(err) errMsg(err))
tryCatch(checkEquals(identify.duplicates.t, identifyDuplicates(ex4.test2)),
         error=function(err) errMsg(err))
