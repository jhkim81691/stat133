# (1 point) Create a vector of length 1000, the first 500 values are 1 and the
# last 500 are 2.  Call this vector a.

a = c(rep(1,500), rep(2,500)) 

# (1 point) Create a vector with values that start at -122.2 with each
# consecutive element decreasing by 1 until the last number -1.2. In other
# words, the following sequence -122.2, -121.2, -120.2, ..., -2.2, -1.2 where
# the ... is filled in with the obvious pattern. Call this vector b.

b = -122.2:-1.2

# (1 point) Create a sequence of 100 numbers from 10 to 50. Call this vector c.

c = seq(10, 50, length=100)

# (1 point) Create a reverse sequence of the lower case letters.  Remember that
# letters is a variable in R, which is the sequence of the lower case letters
# in normal order. Call this vector d.

d = rev(letters)

# (1 point) For this next question, you will use the variable e
e = 1:1000 # DO NOT EDIT THIS LINE
# created above.  Your task is to write a one-line statement below this comment
# that sets all the elements of e to 0

e[] = 0

# (1 point) For this next question, you will use the variable f
f = 1:1000 # DO NOT EDIT THIS LINE
# created above.  Your task is to write a one-line statement below this comment
# that sets the values of f less than 20 to NA

f[f<20] = NA

# (1 point) For this next question, you will use the variable g
g = 1:1000 # DO NOT EDIT THIS LINE

# created above.  Your task is to write a one-line statement below this comment
# that sets the 5th, 7th, and 9th elements of g to 10

g[c(5,7,9)] = 10

# (2 point) For this next question, you will use the variable e
m = matrix(1:100, 25) # DO NOT EDIT THIS LINE
# Create a vector containing the sum of the squared entries in each row of m.
# Call this vector h.

h = apply(m, 1, function(x) sum(x^2))
