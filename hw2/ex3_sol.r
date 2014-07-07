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

    # your code here
    factor.variable <- sapply(data, function(var) class(var)=='factor')
    
    level.means <- sapply(data[, !factor.variable], function(var)
                          by(var, data[,factor.variable], mean))
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

    # your code here
    factor.variable <- sapply(data, function(var) class(var)=='factor')
    level.means <- meanByLevel(data)
    overall.means <- sapply(data[, !factor.variable], mean)
    overall.sd <- sapply(data[, !factor.variable], sd)
    level.diff <- apply(level.means, 1, function(level)
                        (level - overall.means) / overall.sd)
    return(t(level.diff))
}

tryCatch(checkIdentical(std.level.diff.t, abs(stdLevelDiff(iris))),
         error=function(err) errMsg(err))
