library(RUnit)
errMsg <- function(err) print(err)
babies.data <- read.csv('babies.csv')
load('lab3-tests.rda')

# Suppose you would like to compare the average birth weight for babies of
# smokers and babies of non-smokers. To reduce the variability of the
# groups being compared, you want to split the data based on one of the
# other variables (gestation, parity, age, height, or weight) before
# running your t-test. Implement a function that splits the data based on a
# given variable and cutoff value for that variable and then performs
# t-test in each group to compare the birthweights of smoker babies vs
# non-smoker babies. Your function should take the following arguments:
#
# <data>: any subset of the babies.data dataset
# <group.variable>: a string
#   containing the name of the variable that the data will be stratified by
#   (one of: 'gestation', 'parity', 'age', 'height', or 'weight')
# <group.cutoff>: a numeric value defining the boundary between the two
#   groups. One group should contain all observations with <group.variable>
#   less than or equal to <group.cutoff> while the other group should contain
#   all observations such that <group.value> is greater than
# <group.cutoff>. You may assume that this value is specified by the user
#   in such a way that there are both smokers and non-smokers in each group.
# <test.alternative>: a character string that is one of c("two.sided",
#   "less", "greater") specifying the directionality of thet test
#
# Your function should return the following:
#
# <t.outputs>: a list containint two lists corresponding to the outputs of
#   each test (first list: the subset below the cutoff, second list: the
#   subset above the cutoff). Each of these lists should contain two
#   elements: the t-statistic as its first element and the p-value as its
#   second element.
# In addition, your function should return two plots one for each group) in
#   the same window. Each plot should contain the densities for both non-smokers
#   (black) and smokers (red). Do not worry about any other parameters for
#   the plot.

stratifiedTest <- function(data, group.variable, group.cutoff, test.alternative="less") {

    stopifnot(group.variable %in% names(data)[2:6]) 
	data.subset1 <- data[ (data[, group.variable] <= group.cutoff), ]
	data.subset2 <- data[ (data[, group.variable]  > group.cutoff), ]
	
	#smoke(1) smoked, smoke(0) did not smoke
	bwt1.smoke <- data.subset1[ (data.subset1[, "smoke"] == 1), "bwt"]
	bwt1.nonsmoke <- data.subset1[ (data.subset1[, "smoke"] == 0), "bwt"]
	bwt2.smoke <- data.subset2[ (data.subset2[, "smoke"] == 1), "bwt"]
	bwt2.nonsmoke <- data.subset2[ (data.subset2[, "smoke"] == 0), "bwt"]
	
	t.output1 <- t.test(bwt1.smoke, bwt1.nonsmoke, test.alternative)
	t.output2 <- t.test(bwt2.smoke, bwt2.nonsmoke, test.alternative)
	
	t.outputs <- list(c(t.output1[[1]], t.output1[[3]]), c(t.output2[[1]], t.output2[[3]]))
	
	par(mfrow=c(1,2))
	plot(density(bwt1.nonsmoke), col="black", main="Below cutoff")
	lines(density(bwt1.smoke), col="red")
	
	plot(density(bwt2.nonsmoke), col="black", main="Above cutoff")
	lines(density(bwt2.smoke), col="red")
	
	return(t.outputs)
}

output.t1 <- stratifiedTest(babies.data, "height", 64)
tryCatch(checkEquals(stratified.test.t1, unname(unlist(output.t1))),
         error=function(err) errMsg(err))

output.t2 <- stratifiedTest(babies.data, "gestation", 280)
tryCatch(checkEquals(stratified.test.t2, unname(unlist(output.t2))),
         error=function(err) errMsg(err))
