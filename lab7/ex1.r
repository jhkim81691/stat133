library(RUnit)
errMsg <- function(err) print(err)
load('lab7-tests.rda')

# Implement the function dataGenerator. Your function should take the following
# arguments:
#
# <X>: an n x p matrix of predictor variables (or vector if p=1)
# <betas>: the "true" coefficients of the regression equation
# <var>: the variance of the error term epsilon in the regression equation
#   (sigma^2)
#
# Your function should return (Y) a length <n> vector of responses: Y=X<betta> +
# epsilon. If length(<betas>) is not equal to ncol(<X>), your function should
# throw the error: "length betas does not match p"

dataGenerator <- function(X, betas, var) {
	X <- as.matrix(X)
	
	if (length(betas) != ncol(X)) stop("length betas does not match p")
	else {
		epsilon <- rnorm(nrow(X), 0, sqrt(var))
		Y <- X %*% betas + epsilon
		return(Y)
	}
}

set.seed(47)
tryCatch(checkEquals(lab7$dataGenerator.t, dataGenerator(lab7$predictors, 4, 4)),
         error=function(err) errMsg(err))


# Implement the function betaEstimator. Your function should take the following
# arguments:
#
# <X>: an n x p matrix of predictor variables (or vector if p=1)
# <betas>: the "true" coefficients of the regression equation
# <var>: the variance of the error term epsilon in the regression equation
#
# Your function should generate response values using your dataGenerator
# function. Using this data, your function should run a linear regression and
# return an estimate of the values of <betas>.

betaEstimator <- function(X, betas, var) {
	Y = dataGenerator(X, betas, var)
	fit = lm(Y ~ X)
	coefs = fit$coef[2:length(fit$coef)]
	return(coefs)
}

set.seed(47)
tryCatch(checkEquals(lab7$betaEstimator.t, betaEstimator(lab7$predictors, 4, 4)),
         error=function(err) errMsg(err))


# It turns out that under certain assumptions, our estimates of beta follow a
# normal distribution with mean beta and variance (X^T X) ^ -1
# *sigma^2. Implement the function betaVariance. Your function should take the
# following arguments:
#
# <X>: an n x p matrix of predictor variables (or vector if p=1)
# <var>: the variance of the error term epsilon in the regression equation
#
# Your function should calculate and return a vector giving the variance of beta
# hat.

betaVariance <- function(X, var) {
	beta.var <- c(solve(t(X) %*% X) * var)
	return(beta.var)
}

tryCatch(checkEquals(lab7$betaVariance.t, betaVariance(lab7$predictors, 4)),
         error=function(err) errMsg(err))


# Generate a 100 x 1 vector of predictor variables X by sampling from a normal
# distribution with mean 0 and variance 1. Using these predictors, run 1000
# simulations where you generate response variables Y according to the
# regression equation with beta=3 and sigma^2=2. For each of your simulations,
# estimate beta hat and store this value as beta.hats. Calculate the proportion
# of beta.hats that are within 2 sd of beta over your 1000 simulations. Store
# this value as prop.2sd.
# ***make sure to set your seed to 47 before running your simulations***

# beta.hats <- your code here
# prop.2sd <- your code here

set.seed(47)
n = 100
n.sims = 1000
beta = 3
var = 2

X = rnorm(n)
beta.hats <- replicate(n.sims, betaEstimator(X, beta, var))
beta.var = betaVariance(X, var)
beta.2sd = 2 * sqrt(beta.var)
prop.2sd <- sum(beta.hats <= beta + beta.2sd & beta.hats >= beta - beta.2sd) / n.sims