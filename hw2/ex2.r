library(RUnit)
errMsg <- function(err) print(err)
load('ex2-tests.rda')

# Implement the function "simpleNormSim". This function should generate several
# simulations of samples from a specified normal distribution. The number of
# variables sampled should be constant across simulations. The variables in
# each simulation should have equal variance but may have different means. Your
# function should take the following arguments:
#                                        
# <sim.size>: a numeric constant indicating the number of samples in each
#   simulations.
# <means>: a numeric vector indicating the mean for each each simulation.
# <var>: a numeric constant indicating the variance of the random
#   variables in each simulation.
#
# Your function should return:
#
# <simulation>: a **list** that has the same length as <means>. The jth entry of
#   this list should be a sample of <sim.size> random normal variables with
#   variance given by <var> and mean given by the jth entry of <means>.

simpleNormSim <- function(means, sim.size=50, var=1) {

    # your code here
}

set.seed(47)
tryCatch(checkIdentical(simple.norm.sim.t, simpleNormSim(c(25, 50, 75))),
         error=function(err) errMsg(err)) 

# Implement the function "advancedNormSim". This function should generate several
# simulations of samples from a specified normal distribution. The number of
# samples, mean, and variance should all be allowed to vary accross each
# simulation. Your function should take the following arguments:
#                                        
# <sim.sizes>: a numeric vector whose length is equal to the number of
#   simulations that will be performed. The entries give the number of samples
#   drawn in the corresponding simulation.
# <means>: a numeric vector whose length corresponds to the number of
#   simulations that will be performed. The entries give the means of the normal
#   distribution in the corresponding simulation.
# <vars>: a numeric vector whose length corresponds to the number of simulations
#   that will be performed. The entries give the variances of the normal
#   distributions in the corresponding simulation.
#
# Your function should return the following:
#
# <simulations>: a **list** whose length corresponds to the number of
#   simulations performed. The jth entry of this list should be a samples obtained
#   from rnorm with parameters n, mean, and sd given by the jth entries of the
#   corresponding arguments.

advancedNormSim <- function(sim.sizes, means, vars) {

    stopifnot(all.equal(length(sim.sizes), length(means), length(vars)))
    # your code here
}   

sizes.t <- c(25, 50, 25, 50)
means.t <- c(0, 5, 5, 0)
vars.t <- c(1, 2, 1, 2)

set.seed(47)
tryCatch(checkIdentical(advanced.norm.sim.t, advancedNormSim(sizes.t, means.t,
                                                             vars.t)),
         error=function(err) errMsg(err))
