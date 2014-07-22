# load the data saved by the digits-eda.r script
load("digits.rda")

# make a little image plotting helper function
colors = colorRampPalette(colors=c('white','black'))
drawImage = function(img) {
    image(matrix(img, 28)[,28:1], col=colors(256), xaxt='n', yaxt='n')
}

# set the number and layout of the plots per figure
par(mfrow=c(2,5))

# plot the first 10 digits from both the training and test set
apply(d$train.x[1:10, ], 1, drawImage)
apply(d$test.x[1:10, ], 1, drawImage)

# plot the first 10 examples of the number 8 from the training set
# <-- your code here

# compute the average digits for both the training and test set
train.avg = sapply(by(d$train.x, d$train.y, colMeans), identity)
test.avg = sapply(by(d$test.x, d$test.y, colMeans), identity)

# plot them
apply(train.avg, 2, drawImage)
apply(test.avg, 2, drawImage)
