# Classification
#
# Using the handwritten digit recognition dataset from the Kaggle website.
# http://www.kaggle.com/c/digit-recognizer
# The data consists of a large number of 28×28-pixel 8-bit images and a corresponding
# label (0, 1, . . . , 9).

#####################################################################
## Load test and training data
#####################################################################

n = 1000
d = list()
d$all = read.csv("digits.csv")

class(d$all)
names(d$all)
head(names(d$all))
dim(d$all)

# sample w/out replacement 2n items
set.seed(42)
smpl = sample.int(nrow(d$all), 2*n)

# training set is the firs n
d$train = d$all[smpl[1:n], ]
d$train.mat = as.matrix(d$train)
d$train.x = as.matrix(d$train)[, -1]
d$train.y = as.factor(d$train$label)

# test set is the second n
d$test = d$all[smpl[-(1:n)], ]
d$test.mat = as.matrix(d$test)
d$test.x = as.matrix(d$test)[, -1]
d$test.y = as.factor(d$test$label)

# save
save(d, file="digits.rda")

#####################################################################
## EDA
#####################################################################

# make sure both training and test data have roughly similar numbers
# of examples of each digit
table(d$train.y)
table(d$test.y)

# examine distribution of pixels per digit
boxplot(d$train.x ~ d$train.y)
boxplot(d$test.x ~ d$test.y)

# examine the overall distribution of pixels
hist(as.vector(d$train.x))
hist(as.vector(d$test.x))

# draw images

# draw average images

