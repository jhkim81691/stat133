library(rpart)

load("digits.rda")

# fit classification tree and print summary
model.rpart = rpart(y~., method="class", data=data.frame(y=d$train.y, d$train.x))
printcp(model.rpart)

# plot classification treee
plot(model.rpart, uniform=TRUE, main="Classification Tree for Digit Recognition.")
text(model.rpart, all=TRUE, cex=0.75)

# another way to plot it
library(maptree)
draw.tree(model.rpart, cex=0.5, col=gray(0:8/8))

# using the classification tree predict classes of test set
prediction.rpart = predict(model.rpart, newdata=data.frame(d$test.x), type="class")

# how well did it do?
mean(prediction.rpart == d$test.y)
table('Actual Class'=d$test.y, 'Predicted Class'=prediction.rpart)
