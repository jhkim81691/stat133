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
    quant.cutoff <- quantile(input.vector, c(trim, 1-trim))
    truncated.vector <- input.vector[input.vector>=min(quant.cutoff) &
                                     input.vector<=max(quant.cutoff)]
    return(truncated.vector)
}

tryCatch(checkEquals(4:7, truncate(1:10, trim=0.25)), error=function(err)
         errMsg(err))

tryCatch(checkIdentical(integer(0), truncate(1:10, trim=0.5)),
         error=function(err) errMsg(err))


# Implement the function "standardNormalize". This function should take a
# vector, subtract the mean and divide by the standard deviation. In addition,
# the function should return a logical indicating whether any of the values of
# the input vector are more than 3 standard deviations away from the mean. Your
# function should take the following arguments:
#
# <input.vector>: a numeric vector
#
# Your function should return the following values (HINT-you'll have to think
# about how to return these values):
#
# <sn.vector>: the numeric vector that results from subtracting the
# mean (of <input.vector>) and divinding by the standard deviation (of
# <input.vector>). THIS VALUE SHOULD BE THE FIRST OBJECT RETURNED 
# <outlier>: a logical that indicates whether any of the observations are more
# than 3 standard deviations from the mean. THIS VALUE SHOULD BE THE SECOND
# OBJECT RETURNED

standardNormalize <- function(input.vector) {

    # your code here
    sn.vector <- (input.vector - mean(input.vector)) / sd(input.vector)
    outlier <- sum(abs(sn.vector) > 3) > 0
    return(list(sn.vector=sn.vector, outlier=outlier))
}

load('ex2-tests.rda')

set.seed(47)
check1.test <- c(rnorm(100, 5, 1), 0)
tryCatch(checkEquals(standard.normalize.t1[[1]],
                     standardNormalize(check1.test)[[1]], tolerance=1e-5),
         error=function(err) errMsg(err))

tryCatch(checkTrue(standardNormalize(check1.test)[[2]]),
         error=function(err) errMsg(err))


set.seed(47)
check2.test <- rnorm(50, 5, 5)
tryCatch(checkEquals(standard.normalize.t2[[1]],
                     standardNormalize(check2.test)[[1]], tolerance=1e-5),
         error=function(err) errMsg(err))

tryCatch(checkTrue(!standardNormalize(check2.test)[[2]]),
         error=function(err) errMsg(err))
