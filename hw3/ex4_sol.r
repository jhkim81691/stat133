library(RUnit)
errMsg <- function(err) print(err)
load('ex4-tests.rda')

# Included in the hw4 folder, you will find the "cancer.csv" dataset which
# contains the following variables:
# ID: patient id number
# TRT: patient treatment group (0=control, 1=treatment)
# AGE: patients age in years
# WEIGHIN: patients weight at the beginning of the study
# STAGE: inital cancer stage
# TOTALCIN: initail oral condition
# TOTALCW2: oral condition at week 2
# TOTALCW4: oral condition at week 4
# TOTALCW6: oral condition at week 6

# Produce a plot of densities for patien age broken down by group. Color
# the treatment group red and the control group black. Label the x-axis
# "age". Give your plot the title "Patient age by group". Set the range of
# your x-axis to be 20, 100. You will need to read in the data to do this.

# your code here *
cancer.data <- read.csv('Cancer.csv')
trt.idcs <- cancer.data$TRT == 1
control <- cancer.data[!trt.idcs, ]
treatment <- cancer.data[trt.idcs, ]
plot(density(control$AGE), main ='Patient age by group', xlab='age',
     xlim=c(20, 100))
lines(density(treatment$AGE), col='red')

# Produce four plots in the same window comparing the control and treatment
# group oral conditions at each period (initial, 2wk, 4wk, 6wk). These
# plots should be titled "initial", "2wk", "4wk", and "6wk"
# respectively. The x-axis should range from 1 to 20 in each plot, and your
# y-axis from 0 to 0.6. The y
# axis should be labeled as "condition" in each plot. Please color the
# control group black and the treatment group red.
# NOTE: These plots wil look super wierd, don't worry about that.

# your code here ***
names <- c("initial", "2wk", "4wk", "6wk")
trt.idcs <- cancer.data$TRT == 1
par(mfrow=c(2, 2))
mapply(function(data, name) {
    plot(density(data[!trt.idcs]), main=name, xlim=c(1, 20), ylim=c(0, 0.6),
         xlab="condition")
    lines(density(data[trt.idcs]), col='red')
    }, cancer.data[, 6:9], names) 


# Load in the "babies.csv" dataset for this problem. Implement the function
# "testGroupsGestation" that takes the following arguments:
#
# <data>: any subset of the babies.csv dataset
# <group1.idcs>: a numeric vector giving the indices of some subset of
#   <data>
# <group2.idcs>: a numeric vector giving the indices of some other subset
#   of <data>. NOTE: the two idcs vectors should contain no overlapping
#   elements.
# <test.alternative>: a character string equal to one of the following
#   c("two.sided, "less", "greater") specifying the directionalty of the t.test
#
# Your function should return the following output:
#
# <t.test.output> the entire output of the t.test comparing gestation
#   period for the two given groups. Use the group1 subset the first
#   argument, group 2 as the second argument and the alternative direction
#   specified by <test.alternative>

testGroupsGestation <- function(data, group1.idcs, group2.idcs,
                                test.alternative='two.sided') {

    stopifnot(!any(group1.idcs %in% group2.idcs))

    # your code here **
    group1 <- data[group1.idcs, ]
    group2 <- data[group2.idcs, ]
    t.test.output <- t.test(group1$gestation, group2$gestation, alternative=test.alternative)
    return(t.test.output)
}

tryCatch(checkEquals(test.groups.gestation.t,
                     testGroupsGestation(test.data, g1, g2,
                                         test.alternative='greater')),
         error=function(err) errMsg(err))

# Use your function to perform a one-sided t-test comparing the gestation
# period for babies of smoking mothers and non-smoking mothers. Store this
# variable as <smoking.test>

# your code here *
#smoke.idcs <- your code here
#non.smoke.idcs <- your code here
#smoking.test <- your code here


#smoke.idcs <- which(babies.data$smoke==1)
#non.smoke.idcs <- which(babies.data$smoke==0)
#smoking.test <- testGroups(babies.data, smoke.idcs, non.smoke.idcs, 'less')
