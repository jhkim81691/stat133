## Homework 7
source("utilities.r")

## In this exercise we'll be looking at Pearson's data on father and
## son heights. Load the data using the statement below:

father.son <- read.csv("father-son.csv")

## There are 1078 observations of father heights (fheight) and their
## sons' heights (sheight). The measurements are in inches.

## Create a scatterplot of the sons' heights (y-axis) versus the
## fathers' heights (x-axis). Use pch = 20 in your call to plot.

fs.scatterplot <- function() {
    plot(sheight ~ fheight, data = father.son, pch = 20)
}

fs.scatterplot()

## Fit a linear model (see "lm") to the data, and call this "fs.lm".
## What are the coefficients? Save this in a variable named "fs.coef"

(fs.lm <- lm(sheight ~ fheight, data = father.son))
(fs.coef <- fs.lm$coefficients)

## Write a function "fs.predict" that takes a fitted "lm" object and a
## vector of father heights and outputs the predicted heights of their
## sons.
## 
## <fs.lm> - An "lm" object, fitted previously
## 
## <new.fheight> - A vector of new father heights, not necessarily
##                 present in the data we fit
##
## Hint: Look up "predict.lm"

fs.predict <- function(fs.lm, new.fheight) {
    predict(fs.lm, newdata = data.frame(fheight = new.fheight))
}

test(unname(fs.predict(fs.lm, 70)), 69.8731, tolerance = 0.0001)

## Plot the residuals versus the fitted values with a horizontal
## line at 0. Do the residuals look independent and normally
## distributed?

plot.residuals <- function() {
    with(fs.lm, plot(residuals ~ fitted.values))
    abline(h = 0)
}

plot.residuals()

## 
## Remember that it's not necessary for errors/residuals to be
## normally distributed in order to fit ordinary least squares
## (OLS). However, if we assume that errors are normally distributed,
## then the estimators for beta (the coefficients) are also normally
## distributed, and we can construct confidence intervals for them.
##
## What's the lower and upper bound of the 95% confidence interval for
## the parameter on fheight? Hint: Look up the help for "lm" and look
## under "See Also" for useful functions.
## 
## Save the confidence interval as a vector of length 2 named
## fheight.slope.confidence.interval

fheight.slope.confidence.interval <- confint(fs.lm)[1,]

## There are two types of intervals we may be interested in when doing
## linear modeling. One interval, called the "confidence interval" or
## confidence band, is an interval on *parameters*. It tells us, for
## example, 95% of the time where the true mean might lie.
##
## The other interval, called the predictive interval, tells us where
## most of the population lies. For example, given a father's height,
## we might use a 95% predictive interval to denote a probable range
## for his son's height. Note that this is a much bigger range than
## the confidence interval for the true mean of sons heights given a
## specific father's height.  The predictive interval is generally
## much larger than the confidence interval.

## Write a function "fs.confidence" that takes in fs.lm and a vector
## "new.fheight" of new father heights for whom we're trying to
## predict a range of their sons' heights.
##
## Return a matrix of size length(new.fheight) x 2, where the left
## column is the lower predictive bound on the sons' heights and the
## right column is the upper bound.
##
## Hint look at the helm for "predict" or "predict.lm"

fs.confidence <- function(fs.lm, new.fheight) {
    predict(fs.lm, newdata = data.frame(fheight = new.fheight), interval = "confidence")[,2:3]
}

test(unname(fs.confidence(fs.lm, 70)), c(69.68266380, 70.06357032))

## Now, on top of the scatterplot of father vs son heights, plot 5 lines:
##
## 1 line for the regression line - in red
## 2 lines for the lower and upper 95% confidence interval - in green
## 2 lines for the lower and upper 95% prediction interval - in blue

plot.bands <- function() {
    fs.scatterplot()
    abline(fs.lm, col = "red")

    with(as.data.frame(predict(fs.lm, interval = "confidence")), {
        theorder <- order(father.son$fheight)
        lines(father.son$fheight[theorder], lwr[theorder], col = "green")
        lines(father.son$fheight[theorder], upr[theorder], col = "green")
    })

    with(as.data.frame(predict(fs.lm, interval = "prediction")), {
        theorder <- order(father.son$fheight)
        lines(father.son$fheight[theorder], lwr[theorder], col = "blue")
        lines(father.son$fheight[theorder], upr[theorder], col = "blue")
    })
}

plot.bands()


## Create a function "r.squared" that calculates the coefficient of
## determination. Its arguments are
##
## <y> - the observed y values
## <y.fitted> - the predicted or fitted values of y output by the model
##
## See https://en.wikipedia.org/wiki/Coefficient_of_determination
## 
## The coefficient of determination is calculated by taking 1 minus
## the sum of squared residuals divided by the total sum of squares
## (sum of the squared difference between all y values and the mean y
## value).

r.squared <- function(y, y.fitted) {
    ssr <- sum((y-y.fitted)^2)
    sst <- sum((y-mean(y))^2)
    1 - ssr/sst 
}

## What is the r.squared of our linear model on the father-son data?
## Save this in a variable named "fs.rsquared"

(fs.rsquared <- r.squared(father.son$sheight, fs.lm$fitted.values))
