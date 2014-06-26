library(RUnit)
errMsg <- function(err) print(err)
load('test-vars.rda')
load('prostatedata.Rda')

#Included in your folder, you will find the file prostatedata.Rda. The variable
#<prost.data> from this file contains gene expression levels of 1000 genes for
#102 individuals. These individuals are labeled as either having prostate data
#(2) or not (1). Please perform the following exploratory data analysis (EDA)
#tasks. **We have already defined the names of the variables we are asking
#for. Please do not change these. You will need to remove the "#" in front of
#each variable so that they are not commented out.** You may use any additional
#code before and after the variables to get the object we are requesting. Good
#luck!

# As we mentioned, patients with prostate cancer are labeled with a 2. How many of
# these patients are there?

# n.prostate <- # your code here

# Suppose we're interested in the mean of the expression levels for each gene,
# broken down by cancer status. Please create a 2 x 1000 matrix giving the
# average expression level for each gene by cancer status. The first row should
# correspond to patients without prostate cancer and the second to patients with
# (i.e. the entry of row one column one should be the expression level of the
# gene from the first row averaged over all patients without prostate
# cancer). Do the same for a trimmed mean with trim parameter of 0.1.

# status.means <- # your code here
# status.trim.means <- # your code here


# Back to the original dataset. We are interested in looking at the distribution
# of expression levels for each patient. Produce a a single plot containing a
# boxplot of each patient's gene expression levels. Patients without prostate
# cancer should be colored blue while patents with should be colored red. Set
# the pch parameter to '.'.

# your code here

# Suppose we want to remove any gene that has an unusually low expression level
# across many patients. We'll define "unusually low" as below the quantile
# determined by <q.cutoff>, and "many patients" with another cutoff
# <max.low.expr>. Implement the function "quantileCutoff" that checks each gene
# to see how many times that gene occurs below the quantile determined by
# <q.cutoff>. This function should give the indices of any gene that occurs
# below the quantile cutoff for more than a specified number of patients. Your
# function should take the following arguments:
#
# <data>: **any** subset of the prost.data dataset
# <q.cutoff>: a numeric between 0 and 1 specifying what quantile defines a gene
#   as having a low expression level. The actual value for the cutoff should be
#   determined by finding the <q.cutoff> quantile of expression levels over the
#   **entire dataset**. Use a strict less than inequality when defining genes
#   having low expression levels.
# <max.low.expr>: the maximum number of patients that may express a given gene
#   at a low level before the gene's index is returned
#
# Your function should return the following:
#
# <low.expr.idcs>: the indices of any gene that is expressed at a low level in
#   more than <max.low.expr> patients

quantileCutoff <- function(data, q.cutoff, max.low.expr) {

    # your code here
}

tryCatch(checkEquals(quantile.cutoff.t, quantileCutoff(test.data, 0.25, 10)),
         error=function(err) errMsg(err))

# Our question of interest is whether certain genes exhibit different expression
# levels between patients with cancer and those without. The first thing we'll
# need to do to answer this is convert the expression levels to
# t-statistics. Implement the function "tConvert" that takes the following
# arguments:
#
# <gene>: any single row vector of the prost.data matrix
#
# Your function should return
#
# <t.stat>: the t statistic for the difference of means between the two
# groups. This is given by
#
#   (mean.1 - mean.2) / s.12 * sqrt(1/n.1 + 1/n.2), where
#   s.12=sqrt{[(n.1-1)*s.1^2 + (n.2-1)*s.2^2] / [n.1 + n.2 - 2]}
#
# (if this is not familiar, check out the wikipedia entry on the student's
# t-test page for independent two sample t-tests with unequal sample sizes)

tConvert <- function(gene) {

    # your code here
}

tryCatch(checkEquals(0.5889667, unname(tConvert(test.data[1, ])),
                     tolerance=1e-6), error=function(err) errMsg(err))

# Implement the function pValConvert that takes any subset of genes from the
# dataset, calculates their t-statistics, and then returns the p-value
# associated with those t-statistics. Your function should take the
# following arguments:
#
# <data>: any subset of genes from the prost.data dataset
#
# Your function should return
#
# <p.vals>: the p-values associated with the difference in means between cancer
# and non-cancer patients
#
# ***NOTE***: "pt" will be a useful function here (you have 100 degrees of
# freedom). Since this is a two-sided test you'll need to use the absolute value
# of your t-statistic as an argument, subtract the output from 1, and multiply
# by 2 to get the actual p-value.

pValConverter <- function(data) {

    # your code here
}

tryCatch(checkEquals(p.val.converter.t, pValConverter(test.data)),
         error=function(err) errMsg(err))

# Create a histogram of all the p-values from the prost.data dataset. This
# histogram should have the following attributes:
# 1) the title "Prostate Data p-values"
# 2) an x-axis label of "p-values"
# 3) the y-axis should be scaled by probability (as opposed to frequency)
# 4) the breaks should occur at increments of 0.05
# 5) you should add a red line at y=1 across the entire plot (look at the
# "abline" function). This line represents the expected proportion p-values in
# each bin.

# your code here


# You should notice that your histogram contains a spike for the lowest
# p-values. Some of these represent discoveries (i.e. genes whose expression
# levels are actually different between cancer and non-cancer
# patients). However, we are testing 1000 p-values, so we expect that some of
# these will be small by chance. In other words, we expect there to be 50 tests
# with p-values less than 0.05. If we decide to reject the hypothesis that a
# gene is expressed at the same level in each group at some level <alpha>, we
# would expect <alpha>*1000 of these "discoveries" to be false positives and the
# remaining to be true discoveries (1000 is because we are performing 1000
# hypothesis tests). Implement the function "FDR" that caclulates this false
# discovery rate. Your function should take the following parameters:
#
# <data>: **any** subset of the prost.data dataset
# <alpha>: the level at which hypotheses are rejected (i.e. p-values of <data>
# that are smaller than <alpha> will be rejected)
#
# Your function should return the following arguments:
#
# <fdr>: the proportion of rejected values that we expect to be false
# discoveries

FDR <- function(data, alpha) {

    # your code here
}

tryCatch(checkEquals(0.1923077, FDR(prost.data[1:500, ], 0.005), tolerance=1e-6),
         error=function(err) errMsg(err)) 
    
