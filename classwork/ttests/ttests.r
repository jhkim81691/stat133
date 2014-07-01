babies.data <- read.csv('babies.csv')

# Implement the function "plotWeightGroups" that divides the data into
# tow populations based on the mothers weight (<weight>) and plots
# densities of baby birthweight (<bwt>) for each group. Your function
# should take the following arguments:
#
# <data>: any subset of the <babies.data> dataset
# <weight.boundary>: the cutoff used to determine the two
#   populations. Mothers with weights less than or equal to this value should
#   make up one group while mothers with weights greater than this value
#   should make up the other
# <...>: any additional plotting parameters
#
# Your function should return:
# 
# Nothing, but it should print out the appropriate desnities. distinguish
# the two groups by making the heavier group red and the lighter group
# black. 

plotWeightGroups <- function(data, weight.boundary, ...) {

    # your code here
    idcs <- data$weight <= weight.boundary
    group1 <- data[idcs, ]
    group2 <- data[!idcs, ]

    plot(density(group1$bwt), ...)
    lines(density(group2$bwt), col='red')
}

# Suppose that you are interested in performing a t-test to determine
# whether there is a significant difference in birthweight of babies born
# to heavier mothers as opposed to lighter mothers. Should this be a one
# sided or two sided test? Implement the function "tTestWeight" that
# performs the appropriate t-test and returns the t-statistic along with its
# associated p-value. Your function should take the following arguments:
#
# <data>: any subset of the babies.data dataset
# <weight.boundary>: the cutoff used to determine the two
#   populations. Mothers with weights less than or equal to this value should
#   make up one group while mothers with weights greater than this value
#   should make up the other.
#
# Your function should return the following:
#
# <t.output>: a list where the first element is the t-statistic of your
# test and the second element is the p-value.

tTestWeight <- function(data, weight.boundary) {

        # your code here
        idcs <- data$weight <= weight.boundary
        group1 <- data[idcs, ]
        group2 <- data[!idcs, ]

        test <-  t.test(group1$bwt, group2$bwt, alternative='less')
        t.output <- list(t.stat=test$statistic, p.val=test$p.value)
        return(t.output)
}


# Suppose you are interested in testing the hypothesis that the average
# birthweight for babies of smokers is less than the average birthweight of
# non-smokers. You would like to make your test more powerful (i.e. better
# able to reject if the alternative hypothesis is true) by accounting for
# the fact that babies of heavier mothers weigh more on average (see
# previous test). Implement te function "plotStratified" to compare the
# distributions of these four groups. Your function should take the
# following arguments:
#
# <data>: any subset of the babies.data dataset.
# <weight.boundary>: : the cutoff used to determine the two
#   populations. Mothers with weights less than or equal to this value
#   should make up one group while mothers with weights greater than this
#   value should make up the other.
# <...>: any additional plotting parameters.
#
# Your function should return the following:
#
# Nothing, but it should create two plots. One should display the densities
# of birthweights, broken down by babies of smokers and babies of non-smokers,
# amongst heavier mothers. The other should compare the same but for babies
# of lighter mothers. Please color the density for smoker babies red in
# both plots. RECALL: par(mfrow=c(2, 1)) divides the plotting grid into 2
# rows and 1 column.

plotStratified <- function(data, weight.boundary, ...) {

    # your code here
    heavy.idcs <- data$weight > weight.boundary

    par(mfrow=c(2, 1))
    by(data[, c('bwt', 'smoke')], heavy.idcs, function(group) {
                plot(density(group$bwt[group$smoke==0]), ...)
                lines(density(group$bwt[group$smoke==1]), col='red')
            })
    return(NULL)
}

# Implement the function "stratifiedTest" to perform a t-test on each of
# the weight groups in the previous problem. Your function should take the
# following arguments:
#
# <data>: any subset of the babies.data dataset.
# <weight.boundary>: : the cutoff used to determine the two
#   populations. Mothers with weights less than or equal to this value
#   should make up one group while mothers with weights greater than this
#   value should make up the other.
#
# Your function should return the following:
#
# <t.outputs>: a list with the test results from each t-test. The first
#   element of this list should be a list itself, containing the t-statistic
#   and p-value for the t-test that compares babies of smokers with babies of
#   non-smokers amongst heavier mothers. The second should be the same test
#   results but for the lighter mother subset.

stratifiedTest <- function(data, weight.boundary) {

        # your code here
        heavy.idcs <- data$weight > weight.boundary

        t.outputs <- by(data[, c('bwt', 'smoke')], heavy.idcs, function(group) {
                            test <- t.test(group$bwt[group$smoke==1],
                                           group$bwt[group$smoke==0],
                                           alternative='less')
                            return(list(test$statistic, test$p.value))
                        })
        
        return(t.outputs)
}

                        
