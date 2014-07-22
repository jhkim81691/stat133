library(class)
library(RUnit)

load("digits.rda")

# use knn to predict 
prediction.knn = knn(d$train.x, d$test.x, d$train.y, k=10)

# how well did it do?
mean(prediction.knn == d$test.y)
table('Actual Class'=d$test.y, 'Predicted Class'=prediction.knn)

# let's implement our own knn to make sure we understand it
mode = function(x) names(which.max(table(x)))
my.knn = function(train, test, cl, k=1) {
    x = rbind(train, test)
    ind = 1:nrow(train)
    d = as.matrix(dist(x))[-ind, ind]
    indices = t(apply(d, 1, function(x) order(x)[1:k]))
    ys = apply(indices, 1, function(x) mode(cl[x]))
    return(factor(unname(ys)))
}

# let's test that it works as expected
train = rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
test = rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
cl = factor(c(rep("s",25), rep("c",25), rep("v",25)))

tryCatch(
    checkEquals(
        knn(train, test, cl, k=3),
        my.knn(train, test, cl, k=3)
    ),
    error=function(err) print(err)
)
