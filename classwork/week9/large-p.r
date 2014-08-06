library(car)
library(MASS)
library(glmnet)

# plot variables against one another to check for linear relationship 
makelines = function(x,y) {
    points(x,y,pch=20)
    abline(lsfit(x,y),col="blue")
}
pairs(mtcars, panel=makelines)

# create model and evaluate assumptions
set.seed(47)
n.train <- 24
train.idcs <- sample(1:nrow(mtcars), n.train)
cars <- list()
cars$train <- mtcars[train.idcs,]
cars$test <- mtcars[-train.idcs,]

fit.full <- lm(mpg~., data=cars$train)
plot(fit.full)

# calculate mean squared error
mse <- function(residuals) {

    n <- length(residuals)
    return(sum(residuals^2)/n)
}

predictions <- predict(fit.full, cars$test)
residuals <- predictions - cars$test$mpg
full.mse <- mse(residuals)

# Should we remove some predictor variables from our model? AIC penalizes based
# on the number of predictors in your model. (AIC = 2p - 2log(likelihood))
stepAIC(fit)
fit.reduced <- lm(mpg~drat+wt+gear+carb, data=cars$train)

predictions <- predict(fit.reduced, cars$test)
residuals <- predictions - cars$test$mpg
reduced.mse <- mse(residuals)

# We can use other methods to reduce dimensions. Lasso penalizes your model
# based on the "size" of the coefficients. (sum_j abs(beta_j))
mpg.idx <- names(mtcars) == 'mpg'
lasso.cv <- cv.glmnet(as.matrix(cars$train[,!mpg.idx]),
                                    as.matrix(cars$train[,mpg.idx]), nfolds=6, alpha=1)
opt.lambda.idx <- which(lasso.cv$cvm==min(lasso.cv$cvm))
opt.lambda <- lasso.cv$lambda[opt.lambda.idx]

fit.lasso <- glmnet(as.matrix(cars$train[,!mpg.idx]),
                    as.matrix(cars$train[,mpg.idx]), alpha=1, lambda = opt.lambda)

predictions <- predict(fit.lasso, as.matrix(cars$test[,!mpg.idx]))
residuals <- predictions - cars$test$mpg
lasso.mse <- mse(residuals)
