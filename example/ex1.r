##   Please replace "NotImplemented" with working code per the specification
##   in the comment after the function header.  To get an idea of whether
##   your function implementation works, you can run the tests at the bottom
##   of the file by calling this file with `Rscript` from a Bash prompt.

mySD <- function(x) {
    ## Take in a vector of numbers and return its standard deviation.
    "NotImplemented"
}

myMean <- function(x) {
    ## Take in a vector of numbers and return its mean.
    "NotImplemented"
}

## tests
library(RUnit)
errmsg <- function(err) {
     print(paste("MY_ERROR:  ",err))
}

x = c(1:100)
x.sd = 29.011491975882016447
x.mean = 50.5
tryCatch(checkEquals(mySD(x), x.sd),
         error = function(err) errmsg(err))
tryCatch(checkEquals(myMean(x), x.mean),
         error = function(err) errmsg(err))
