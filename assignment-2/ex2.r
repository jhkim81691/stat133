library(RUnit)
errMsg = function(err) print(paste("ERROR:", err))


# Implement the function "truncate", a function that trims a given vector by
# removing the upper and lower specified quantiles. Your function should take
# the following arguments:
#
# <input.vector>: the numeric vector to be truncated
# <trim>: a numeric value between 0 and 0.5 that specifies the upper and lower
# quantiles of <input.vector> that should be removed.
#
# Your function should return the following value:
#
# <truncated.vector>: the remaining values of input.vector after removing the
# upper and lower quantiles

truncate <- function(input.vector, trim) {

    stopifnot(0<=trim & trim<=0.5)
    
    # your code here

}

tryCatch(checkEquals(c(2, 3, 4), truncate(1:5, trim=0.25)), error=function(err)
         errMsg(err))

tryCatch(checkIdentical(integer(0), truncate(1:6, trim=0.5)),
         error=function(err) errMsg(err))


# Implement the function "standardNormalize". This function should take a
# vector, subtract the mean and divide by the standard deviation. In addition,
# the function should return a logical indicating whether any of the values of
# the input vector are more than 3 standard deviations away from the mean. Your
# function should take the following arguments:
#
# <input.vector>: a numeric vector
#
# Your function should return the following values in the form of a list (this
# is the only way to return them since they are different types):
#
# <sn.vector>: the numeric vector that results from subtracting the mean (of
# <input.vector>) and divinding by the standard deviation (of <input.vector>).
# <outlier>: a logical that indicates whether any of the observations are more
# than 3 standard deviations from the mean.


standardNormalize <- function(input.vector) {

    # your code here

}

set.seed(47)
check1.target <- c(-3.0037729, 0.4937101, 0.3755566, 0.3271615, 0.2841576,
                   0.3201076, 0.2101504, 0.2193791, 0.3114874, 0.2868933,
                   0.1751695)
check1.test <- c(1, rnorm(10, 10, .25))

tryCatch(checkEquals(check1.target, standardNormalize(check1.test)[[1]],
                     tolerance=1e-5), error=function(err) errMsg(err))
tryCatch(checkTrue(standardNormalize(check1.test)[[2]]),
         error=function(err) errMsg(err))


set.seed(47)
check2.target <- c(1.6397422, 0.1892725, -0.4048326, -0.9327546, -0.4914274)
check2.test <- rnorm(5, -5, 5)

tryCatch(checkEquals(check2.target, standardNormalize(check2.test)[[1]],
                     tolerance=1e-5), error=function(err) errMsg(err))
tryCatch(checkTrue(!standardNormalize(check2.test)[[2]]),
         error=function(err) errMsg(err))
