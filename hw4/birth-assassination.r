library(RUnit)
errMsg <- function(err) print(err)
load('ba-tests.rda')

# The pdf in this folder contains detailed information of the process we
# are asking you to simulate. For this assignment, we have already
# implemented parts of functions for you. Pay close attention to where we
# ask for additional code and make sure your code returns the appropriate
# classes and variable names.  Good luck!

# Add the necessary code to implement the function "generateKids". Your
# function should take the following arguments:
#
# <lambda>: the rate parameter (per unit of time) for the Poisson process
#   describing child birthrate
# <kappa>: the parameter for the exponential distribution describing
#   assassination rate
# <parent>: a single row from a dataframe with variables <parent.id>,
#   <child.id>, <birth.date>, and <assassination.date>
#
# Your function should return:
#
# <kids>: a dataframe with the same variables as <parent>. The number of
#   children (i.e. rows of <kids>) should be generated from a  Poisson
#   process with rate proportional to the length of <parent>'s life (NOTE:
#   because children are born according to a Poisson process, their
#   <birth.date> also follows a specific distribution). Please generate the
#   variables for each row of <kids> as follows:
#   
#   1) <parent.id> for each child should be equal to the <child.id>
#   variable of <parent>
#   2) <child.id> should range from run from 1-nrow(<kids>) (we will change
#   this later in our simulation)
#   3) <birth.date> should follow the distibution of event times over a
#   fixed interval in a Poisson process
#   4) <assassination.date> should occur X time units after <parent>'s
#   assassination date where X comes from an exponential distribution with
#   rate <kappa>


generateKids <- function(lambda, kappa, parent) {

    # Generate the random variable <n.kids> giving the number of children
    # born to <parent>. This value should come from a Poisson distribution
    # whose rate is proportional to the length of <parent>'s life

    # your code here
    # n.kids <- your code here
    
    if (n.kids) {

        # If any n.kids is non-zero, generate the dataframe <kids> containing
        # the necessary variables for each child. To do this, you will need to
        # randomly generate the following variables:
        # <kid.birthdays>: a random numeric vector giving the date of birth
        # for each child
        # <life.lengths>: a random numeric vector indicating the length of
        # each child's life

        # kid.birthdays <- your code here
        # life.lengths <- your code here
        # your code here

        return(kids)
    } else return(NULL) #return null if <parent> has no kids
}

set.seed(47)
tryCatch(checkEquals(generate.kids.t, generateKids(0.5, 1, test.df),
                     tolerance=1e-5), error=function(err) errMsg(err))


# Your above function generates children for one parent, storing them as a
# dataframe. We would like to combine the children born to all parents of
# one generation into a single dataframe that represents the next
# generation. Please implement the function "nextGeneration". Your function
# should take the following arguments:
#
# <lambda>: the rate parameter (per unit of time) for the Poisson process
#   describing birthrate
# <kappa>: the arameter for the exponential distribution describing
#   assassination rate  
# <parents>: a dataframe for which each row can be supplied as an argument
#   to the function "generateKids"
#
# Your function should return the following:
#
# <next.gen>: a single dataframe containing the children for each row of
#   <parents>. The <child.id> variable for this dataframe should be reset
#   to range from 1:nrow(next.gen) (i.e. no two children in a given
#   generation will have the same id)

nextGeneration <- function(lambda, kappa, parents) {

    # please create a list whose length is equal to the number of rows of
    # <parents>. Each element of this list should be a dataframe of the
    # kids born to the corresponding parent. Call this list <next.gen>

    #next.gen <- your code here


    # The following code removes any list element that has no entries
    # (i.e. those for which the parent had no children), and combines the
    # children into one dataframe. You do not need to add anything.
    no.births <- sapply(next.gen, is.null)
    next.gen <- next.gen[!no.births]
    next.gen <- do.call(rbind, lapply(next.gen, '['))

    # If <next.gen> is not null, change the <child.id> variable so that it
    # ranges from 1 to nrow(next.gen)

    # your code here
    
    return(next.gen)
}

set.seed(47)
tryCatch(checkEquals(next.generation.t, nextGeneration(0.5, 1,
                                                       generate.kids.t),
                     tolerance=1e-5), error=function(err) errMsg(err))


bAGen <- function(lambda, kappa, max.gen) {

    # The following creates an empty list whose elements represent each
    # generation 
    generation.list <- vector('list', length=(max.gen + 1))

    # Please initialize the first generation. To do this, set the first
    # element of <generation.list> to be a dataframe with one row and the
    # following variables:
    # <parent.id> = 0
    # <child.id> = 0
    # <birth.date> = 0
    # <assassination.date> a random variable from an expoenetial
    # distribution with rate <kappa>

    # your code here

    # Simulate the birth assassination process using your "nextGeneration"
    # function for up to <max.gen> generations. If the family dies out
    # (i.e. the object generated by "nextGeneration" is NULL), stop
    # simulating.

      
    # your code here


    # This removes extra list elements if the family died out. You do not
    # need to add anything here
    died.out <- sapply(generation.list, is.null)    
    return(generation.list[!died.out])
}

# Please run three simulations of bAGen (1000 iterations each). Set
# <lambda> = 0.1, <max.gen> = 10 for each and use <kappa> = 0.75, 0.5, and
# 0.2 for <sim.1>, <sim.2>, and <sim.3> respectively

set.seed(47)
# sim.1 <- your code here
# sim.2 <- your code here
# sim.3 <- your code here


# Implement the function gensSurvived. Your function should take the
# following argument:
#
# <bagen.simulation>: a list whose elements correspond to one run of bAGen
#   (i.e. the outputs <sim.1>, <sim.2>, <sim.3>)
#
# Your function should return:
#
# <generations>: a numeric vector with the same length as
#   <bagen.simulation>. Each entry should give the number of generations
#   that survived in the corresponding simulation (so that the maximum
#   value will be <max.gen>)

gensSurvived <- function(bagen.simulation) {

    # your code here

}

# Use your gensSurvived function to create the following plot:
# In the same plot (not just the same window), plot densities for the
# number of surviving generations. The densities should be colored red (1),
# green (2), and blue (3) and a legend should be placed in the top
# right. The bandwidth for each density should be set to 
# 0.35. The x-axis should be labeled "generations survived" and the main
# title should be "B-A Simulation". The x-axis should range from 0 to 11.

# your code here
