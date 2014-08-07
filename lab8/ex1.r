# FINAL LAB!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
library(RUnit)
errMsg <- function(err) print(err)
load('lab8-tests.rda')

# Implement the function normalMixture. Your function should take the following
# arguments:
#
# <n1>: the number of observations to generate from the first dist'n
# <n2>: the number of observations to generate from the second dist'n
# <mu1>: the mean of the first normal dist'n
# <var1>: the variance of the first normal dist'n
# <mu2>: the mena of the second normal dist'n
# <var2>: the variance of the second normal dist'n
#
# Your function should generate <n1> + <n2> random variables. The first <n1>
# observations should be normal mean = <mu1> and var = <var1>. The last <n2>
# observations should be normal mean = <mu2> and var = <var2>.

normalMixture <- function(n1, n2, mu1, var1, mu2, var2) {

    # your code here

}

set.seed(47)
output.1 <- normalMixture(5, 5, 0, 1, 0, 1)
tryCatch(checkEquals(lab8$normalMixture.t, output.1),
         error=function(err) errMsg(err))


# Implement the function logGenerator. Your function should take the following
# arguments:
#
# <X>: a numeric vector of predictor variables
# <beta0>: a numberic value giving the bias (intercept) of the regression
#   equation
# <beta1>: a numeric value giving the slope of the regression equation
#
# Your function should generate probabilities accoring to the logistic function:
# 1/(1+exp(-y)) where y=<beta0>+<beta1>*X

logGenerator<- function(X, beta0, beta1) {

    # your code here
}

set.seed(47)
output.2 <- logGenerator(lab8$normalMixture.t, 1, 2)
tryCatch(checkEquals(lab8$logGenerator.t, output.2), error=function(err)
         errMsg(err))


# Implement the function toBinom. Your function should take the following
# arguments:
#
# <p>: a numeric vector of probabilities
#
# Your function should return a vector of binomial(1, p_i) random variables for
# each value in <p>.

toBinom <- function(p) {

    # your code here

}

set.seed(47)
output.3 <- toBinom(lab8$logGenerator.t)
tryCatch(checkEquals(lab8$toBinom.t, output.3), error=function(err) errMsg(err))


# Implement the function mae. Your function should take the following arguments:
#
# <true.probs>: a numeric vector of true values
# <pred.vals>: a numeric vector of predicted values
#
# Your function should calculate the average absolute error between predicted
# and true values.

mae <- function(true.vals, pred.vals) {

    # your code here

}

tryCatch(checkEquals(1, mae(1:5, 2:6)), error=function(err) errMsg(err))


# Generate predictor variables using your normalMixture with n1=50, n2=50,
# mu1=-1, mu2=1, var1=var2=1. Store this value as the variable
# <x.vals>. Using this data, generate probabilities with your logGenerator
# function by setting beta0=??? and beta1=???. Store this variable as
# <p.vals>. Using these values, generate binomial random variables using your
# toBinom function. Store this data as <y.vals>. Create a dataframe called
# <model.data> where the first column is <x.vals> and the second is
# <y.vals>. Name these columns x and y respectively. Fit linear and logistic
# regression models predicting y from x with data=<model.data>. Call these
# models <fit.lm> and <fit.log>. Run R's predict function on each of your models
# (make sure to set type='response' for logistic regression) and name these
# outputs <preds.lm> and <preds.log>. Use your mae function to evaluate the mean
# absolute error between predicted values and <p.vals>. Store these output as
# <mae.lm> and <mae.log>.
#*** Set your seed to 47 before generating <x.vals> but do not change it
#otherwise***

set.seed(47)

