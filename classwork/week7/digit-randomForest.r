library(randomForest)

load("digits.rda")

# fit random forest and print summary
model.rf = randomForest(y~., data=data.frame(y=d$train.y, d$train.x))
#model.rf = randomForest(y~., data=data.frame(y=d$train.y, d$train.x), ntree=1000)
print(model.rf)

# plot variable importance
varImpPlot(model.rf, sort=TRUE) 

# using the random forest predict classes of "test set"
prediction.rf = predict(model.rf, d$test.x)

# how well did it do?
# note:  we don't need to do cross-validation on random forests to get
# an unbiased estimate of error rate
mean(prediction.rf == d$test.y)
table('Actual Class'=d$test.y, 'Predicted Class'=prediction.rf)
