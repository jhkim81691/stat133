library(RUnit)
errMsg <- function(err) print(err)
load('ex1-tests.rda')

# Implement the function "returnFalse". Your function should take the
# following arguments:
#
# <data.vector>: some vector of observations of any typeg
# <logical.vector>: a logical vector whose length is equal to
#   <data.vector>
#   
#
# Your function should return:
#
# <false.oservations>: all observations in data.vector for which
#   <logical.vector> is false

returnFalse <- function(data.vector, logical.vector) {

    # your code here *
    false.observations <- data.vector[!logical.vector]
    return(false.observations)
}

tryCatch(checkEquals(return.false.t, returnFalse(ex1.test1.data,
                                                 ex1.test1.bool)),
         error=function(err) errMsg(err))


# Implement the function "returnNonIdcs". Your function should take the
# following arguments:
#
# <data.vector>: some vector of observations of any type
# <idcs>: a numeric vector of indices whose length is equal to <data.vector>
#
# Your function should return the following:
#
# <non.idcs.observations>: all observations in <data.vector> whose indices
#   are not in <idcs>

returnNonIdcs <- function(data.vector, idcs) {

    # your code here *
    non.idcs.observations <- data.vector[-idcs]
    return(non.idcs.observations)
}

tryCatch(checkEquals(return.non.idcs.t, returnNonIdcs(ex1.test1.data,
                                                      ex1.test1.idcs)),
                     error=function(err) errMsg(err))


# Implement the function "subsetDfIdcs". your function should take the
# following arguments:
#
# <data>: a dataframe of any dimension
# <observations.idcs>: a numeric vector corresponding to the rows of the
#   dataframe that will be included in the subset
# <variable.idcs>: a numeric vector corresponding to the columns of the
#   dataframe that will be included in the subset
#
# Your function should return the following:
#
# <data.subset>: a subset of <data> including only the rows from
#   <observation.idcs> and columns from <variable.idcs>

subsetDfIdcs <- function(data, observation.idcs, variable.idcs) {

    # your code here *
    data.subset <- data[observation.idcs, variable.idcs]
    return(data.subset)
}

tryCatch(checkEquals(subset.df.idcs.t, subsetDfIdcs(iris, 1:100, 1:3)),
         error=function(err) errMsg(err))


# Implement the function "subsetDfName". Your function should take the
# following arguments:
#
# <data>: a dataframe of any dimension
# <non.observations>: a numeric vector corresponding to the
#   observations that should not be included in the subset.
# <non.names>: a character vector corresponding to the names of the
#   variables that should not be included in the dataset
#
# Your function should return the following:
#
# <data.subset>: a subset of <data> where the rows given by
#   <non.observations> and the columns given by <non.names> have been
#   removed

subsetDfName <- function(data, non.observations, non.names) {

    # your code here **
    var.idcs <- match(non.names, names(data))
    data.subset <- data[-non.observations, -var.idcs]
    return(data.subset)
}

tryCatch(checkEquals(subset.df.name.t, subsetDfName(iris, 1:50, "Species")),
         error=function(err) errMsg(err))
