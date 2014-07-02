library(RUnit)
errMsg <- function(err) print(err)
load('ex3-tests.rda')

# Suppose you are given a data frame where all but one of the variables are
# numeric. The final variable (though not necessarily final in position) is a
# factor associated with different levels of your observations. Implement the
# function "meanByLevel" that returns the mean value for each of the numeric
# variables by the levels given from the factor variable. Your function should
# take the following arguments:
#
# <data>: a data frame where all but one of the variables are numeric. The final
#   variable is a factor giving the different levels of the observations. **The
#   factor variable is not necessarily the final variable in position.**
#
# Your function should return:
#
# <level.means>: the means of each of the variables broken down by each of the
#   levels (this should be a num.factors x num.numeric.variables matrix).

meanByLevel <- function(data) {
	check.levels <- sapply(1:ncol(data), function(x) is.character(levels(data[, x])))
	name.levels <- levels(data[, check.levels])
	
    num.levels <- 1:length(name.levels)
	
	level.means <- t(sapply(num.levels, function(x)
		sapply(data[data[,check.levels]==name.levels[x], !check.levels], mean)))
	dimnames(level.means) <- list(name.levels, dimnames(level.means)[[2]])
	
	return(level.means)
}

tryCatch(checkIdentical(mean.by.level.t, meanByLevel(iris)), error=function(err)
         errMsg(err))

# Suppose you are given a data frame with the same structure as in the previous
# part of the question. You are interested in identifying the difference between
# the overall average for a given variable and the factor level average for that
# variable. You want this difference to be standardized by the overall standard
# deviation for that variable. Implement the function "stdLevelDiff" that does
# this for each of the numeric variables in your data frame. Your function
# should take the following arguments:
#
# <data>: a data frame where all but one of the variables are numeric. The final
#   variable is a factor giving the different levels of the observations. **The
#   factor variable is not necessarily the final variable in position**
#
# Your function should return: 
#
# <level.diff> the difference between mean by factor level and overal
#   mean for each variable divided by the overall standard deviation for each
#   variable. This should be a num.factors x num.numeric.variables matrix.
#   NOTE: you may need to use R's transpose function to make sure that the
#   dimensions of your return value are correct.

stdLevelDiff <- function(data) {
	level.means <- meanByLevel(data)
	
	check.levels <- sapply(1:ncol(data), function(x) is.character(levels(data[, x])))
	
	data.means <- apply(data[,!check.levels], 2, mean)
	data.sd <- apply(data[,!check.levels], 2, sd)
	
	level.diff <- t(sapply(1:nrow(level.means), function(x) (level.means[x, ]-data.means)/data.sd))
	dimnames(level.diff) <- list(levels(data[, check.levels]), dimnames(level.diff)[[2]])
	return(level.diff)
}

tryCatch(checkIdentical(std.level.diff.t, abs(stdLevelDiff(iris))),
         error=function(err) errMsg(err))
