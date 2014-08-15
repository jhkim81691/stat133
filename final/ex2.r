load('ex2.rda')

# We have loaded the data frame "grades" into your workspace. It contains the
# following variables:
#
# > names(grades)
# [1] "hw"    "labs"  "final"
#
# The three variables represent 100 student's scores for homework, labs, and final.

# (2 points)
# Please fit two linear models. One that predicts a student's final grade from
# their homework grade and another that predicts a student's lab grade from
# their homework grade. Store these as the variables <fit.final> and <fit.labs>
# respectively

# fit.final = your code here
# fit.labs = your code here


# (2 points)
# Please use the diagnostic methods discussed in class to determine which of the
# models satisfies the assumption of constant variance. Store this as the
# variable <contant.var.model>. Your answer should be one of the following
# strings:
#
# "final" or "labs"

# contant.var.model = your code here


# (3 points)
# What is the slope of the regression line in your <fit.final> model? Store this
# as the variable <final.slope>. What is the intercept of the of the regression
# line in the <fit.labs> model? Store this as the variable
# <labs.intercept>. What is the r-squared value for the <fit.final> model (this
# will need to be accurate up to 5 decimal places)? Store
# this as the variable <final.r.sq>

# final.slope = your code here
# labs.intercept = your code here
# final.r.sq = your code here

# (2 points)
# Consider a model that predicts an individual's final score using the following
# formula: y.hat = beta*hw, where
#
# y.hat = individual's predicted final score
# beta = 2*<final.slope>
# hw = individual's actual hw score
#
# Please compute the squared residuals for this model (this should be a length
# 100 numeric vector). Store this as the variable <sq.residuals>.

# sq.residuals = your code here


# (3 points)
# Using your <fit.final> model, generate a 90 percent prediction interval for
# each of the fitted values (this should be a 100 x 2 matrix). Store this as the
# variable <final.pi>. What fraction of the final scores fall within their
# respective prediction interval?  Store this as the variable <prop.within>.

# final.pi = your code here
# prop.within = your code here
