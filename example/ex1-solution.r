mySD <- function(x) {
    ## Take in a vector of numbers and return its standard deviation.
    sqrt(var(x))
}

myMean <- function(x) {
    ## Take in a vector of numbers and return its mean.
    sum(x)/length(x)
}

## tests
library(RUnit)
errmsg <- function(err) {
     print(paste("ERROR:  ",err))
}

x = c(1:100)
x.sd = 29.011491975882016447
x.mean = 50.5
tryCatch(checkEquals(mySD(x), x.sd),
         error = function(err) errmsg(err))
tryCatch(checkEquals(myMean(x), x.mean),
         error = function(err) errmsg(err))
