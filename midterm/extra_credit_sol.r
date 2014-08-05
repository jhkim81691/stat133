library(RUnit)
errMsg <- function(err) print(err)


# For the extra credit problem you need to create a little simulation
# study of the distribution of the sum of three rolls of a 6-sided die
#
# In particular, you need to
# - write a function to generate the sum of three random
#   tosses of a fair die.
# - use this function to generate 10,000 of these sums.
# - plot the histogram

# (2 points) Implement the flip.sum function 
#
# Takes the following arguments:
#   n: an integer 
#
# Returns the sum of n rolls of a 6-sided die
#
# Hint: Rolls from a 6-sided die can be generated using
# R's sample function to sample with replacement from the
# numbers 1 to 6.

flip.sum = function(n=10) {
    rolls = sample(1:6, n, replace=TRUE)
    return(sum(rolls))
}

set.seed(47)
sums = c(44, 29, 30)
tryCatch(checkEquals(sums, c(flip.sum(), flip.sum(), flip.sum())),
         error=function(err) errMsg(err))

set.seed(47)
sums = c(23, 21, 15)
tryCatch(checkEquals(sums, c(flip.sum(5), flip.sum(5), flip.sum(5))),
         error=function(err) errMsg(err))

# (1 points) Create a vector called random.sums by calling flip.sum()
# 10,000 times. Plot the histogram of random.sums. Make sure
# to plot the density (not the counts) and set the breaks to 50. 

random.sums = replicate(10000, flip.sum())
hist(random.sums, breaks=50, freq=FALSE)
