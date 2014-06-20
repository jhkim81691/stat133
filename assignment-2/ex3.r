library(RUnit)
errMsg <- function(err) print(paste('ERROR:', err))

# Inside this folder you will find the file "babies.csv". This data comes from
# Professor Nolan (at Berkeley). It contains information for 1174 babies. The
# variables in the dataset are as follows:
#
# <bwt>: the birth weight of the baby (in ounces)
# <gestation>: the gestation period (in days)
# <parity>: a binary variable indicating whether the baby is the mothers first
# child (1) or not (0)
# <age>: the age of the mother when the child was born
# <hieght>: the height of the mother (in inches)
# <weight>: the weight of the mother (in pounds)
# <smoke>: a binary variable indicating whether the mother smoked (1) or did not
# smoke(0) while pregnant

# Implement the function "smokeDiff". This function should take any subset of
# the data and return the difference in average birthweights <bwt> between
# babies of smokers and babies of non-smokers divided by the standard deviation
# of all birthweights <bwt> for the subset. Your function should take te
# following arguments:
#
# <data.subset>: some subset of the babies.csv data
#
# Your function should return the following:
#
# <normalized.diff>: the difference between average birthweights for babies of
# smokers and babies of non-smokers for the subset. This difference should be
# normalized (i.e. divided by) the standard deviation of <bwt> for the entire
# subset.

smokeDiff <- function(data.subset) {

    # your code here

}


# Implement the function "heavyDiff". This function should take any subset of
# the data and return the difference in average birthweights <bwt> between
# babies of heavier mothers and babies of lighter mothers divided by the
# standard deviation of all birthweights <bwt> for the subset. Your function
# should contain the following variables:
#
# <data.subset>: some subset of the babies.csv data
# <weight.cutoff>: a numeric giving the cutoff weight to distinguish "heavy"
# mothers from "light" mothers.
#
# Your function should return the following:
#
# <normalized.diff>: the difference between the average birthweights for babies
# of heavy (i.e. greater than <weight.cutoff>) mothers and babies of light
# (i.e. less than or equal to <weight.cutoff>) for the subset. This difference
# should be normalized (i.e. divided by) the standard deviation of <bwt> for the
# entire subset.

heavyDiff <- function(data.subset, weight.cutoff) {

    # your code here

}

# Implement the function "superSubset". This function should take the data set
# and return the *indices* of all observations that match the specified subsetting
# requirements. Your function should contain the following variables:
#
# <data>: a data frame of the data in the babies.csv file (or any dataframe with
# the same variables)
# <max.height>: the largest allowable height in the subset (inclusive)
# <max.weight>: the largest allowable weight in the subset (inclusive)
# <max.age>: the largest allowable age in the subset (inclusive)
# <parity>: a binary (i.e 0-1) variable indicating whether the subset should
# contain only first time mothers (1) or only non-first time mothers (0)
#
# Your function should return the following:
#
# <subset.idcs>: the indices of all observations in the dataset that satisfy the
# constraints specified by the subsetting arguments

superSubset <- function(data, max.height, max.weight, max.age, parity) {

    #your code here
    
    subset.idcs <- which(data$height<=max.height & data$weight<=max.weight &
                         data$age<=max.age & data$parity==parity)
    return(subset.idcs)
}

# Load the data here. If you want to use the supplied unit tests, you must keep
# the name <babies.data>. Use "superSubset" to create the data frame
# <mothers.subset> of mothers who are:
# 1) less than or equal to 65 inches tall
# 2) less than or equal to 150 lbs
# 3) less than or equal to 30 years old
# 4) not first time mothers
# With this subset, use you "smokeDiff" function to compare the difference in
# average birthweight between babies of smokers and babies of nonsmokers in
# <mothers.subset>. Store this variable as <subset.diff>.


# babies.data <- load the data here
# mothers.subset <- # your code here
# subset.diff <- # your code here

# tests for smokeDiff
tryCatch(checkEquals(-0.4639051, smokeDiff(babies.data[1:500, ]),
                     tolerance=1e-6), error=function(err) errMsg(err))
tryCatch(checkIdentical(NaN, smokeDiff(babies.data[0, ])), error=function(err)
                        errMsg(err))

# tests for heavyDiff
tryCatch(checkEquals(0.1876471, heavyDiff(babies.data[1:500, ], 140),
                     tolerance=1e-6), error=function(err) errMsg(err))
tryCatch(checkIdentical(NaN, heavyDiff(babies.data[0, ], 140)),
         error=function(err) errMsg(err))

# test for superSubset
tryCatch(checkEquals(394, length(superSubset(babies.data, max.height=65,
                                      max.weight=150, max.age=30, parity=0))),
         error=function(err) errMsg(err))
