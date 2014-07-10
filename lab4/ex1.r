library(RUnit)
errMsg <- function(err) print(err)

# Poisson processes are common stochastic processes used to model arrival
# times. The basic form is a counting process for the number of events that
# have occured up to time t. In addition, Poisson processes satisfy the
# following properties:
#
# 1) The number of events at time 0 is 0
# 2) The number of events that occur in independent intervals are
#    independent of one another 
# 3) The probability distribution of the number of events that occur in
#    some interval depends on the length of the interval
# 4) N(t)~Poisson(lambda*t) where lambda represents the rate of the process
# 5) No occurances are simulatneous

# Implement the function "poisProc" that randomly generates the number of
# events in some time period according to a poisson process with specified
# rate. Your function should take the following arguments:
#
# <rate>: the rate of the poisson process per unit of time
# <time>: the length (in time units) over which the process occurs
#
# Your function should return:
#
# <n.events> a randomly generated integer indicating the number of events
#   that occured over the time interval t


poisProc <- function(rate, t) {

    # your code here


}

set.seed(47)
tryCatch(checkEquals(10, poisProc(0.5, 10)), error=function(err)
         errMsg(err))


# Implement the function poisSim that takes the following arguments:
#
# <time.intervals> a numeric vector specifying time intervals to be used
#   for a poisson process
# <rate> a numeric value specifying the rate of the poisson process
#
# Your function should return:
#
# <n.arrivals> a randomly generated integer vector with the same length as
#   <time.intervals> specifying the number of arrivals in each period of
#   <time.intervals>

poisSim <- function(rate, time.intervals=1) {

    # your code here

}

set.seed(47)
tryCatch(checkEquals(c(2, 0, 1, 2, 1), poisSim(0.25, 1:5)),
         error=function(err) errMsg(err))

# Suppose that a store opens at 9 and customers arrive according to a
# poisson process with rate=10 per hour. Furthermore, suppose that we count
# the number of customers who arrive at the store during the following time
# intervals: 9-9:30, 9:30-10:30, 10:30-12, and 12-2. Use R's replicate
# function to generate 10000 simulations of this scenario. Store this
# variable as <simulations>

set.seed(47)

# times <- your code here
# simulations <- your code here


# In a single plotting window, create the following: one histogram of the
# number of arrivals for each time interval (row of <simulations>).  The
# x-axis for each of these histograms should be identical, ranging from 0
# to the maximum number of people that arrived over any time period in any
# simulation. Set freq=F for all your histograms, label the x-axis
# "arrivals", and set the main title to NULL.
#
# Add a red line of a poisson distribution with parameter that corresponds
# to the time interval of the histogram
# HINT: it may be easiest to turn your simulations matrix into a list (w/
# one entry per row) and use mapply

