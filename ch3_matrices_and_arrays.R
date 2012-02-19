# *************************************************************************
# *************************************************************************
# ****************** Ch 3: Matrices and Arrays ****************************
# *************************************************************************
# *************************************************************************

# A matrix is a vector with two additional attributes: row and column length.
# On the other hand, vectors are not 1-dim matrices.
# You can have higher dimensional arrays, but why do that to yourself.

# Matrices are a special case of a more general R type object: arrays.

# *************************************************************************
# ****************** Ch 3.1 Creating Matrices and Arrays ******************
# *************************************************************************

# Matrix row and column indices begin with 1.
# The internal storage of a matrix is in column-major order, meaning all of col 1
# is stored, then col 2, ... then col n

y <- matrix(c(1,2,3,4), nrow=2)
y <- matrix(nrow=2, ncol=2)  # all NA at this point

m <- matrix(1:6, nrow=2, byrow=TRUE)  # populates across rows

# *************************************************************************
# ****************** Ch 3.2 General Matrix Operations *********************
# *************************************************************************

# ****************** Ch 3.2.1 Performing Linear Algebra Operations ********
# See 8.4 for more about matrix operations

# matrix multiplication (not element-wise)
y %*% y
y * 3  # matrix multiplied by scalar
y + y  # mathematical matrix addition

# ****************** Ch 3.2.2 Matrix Indexing *****************************
z <- matrix(c(1:4, 1,1, 0,0,1,0,1,0), nrow=4)
z[, 2:3]  # all rows, 2nd and 3rd column

y <- matrix(c(1:3 * 10 + 1, 1:3 * 10 + 2), nrow=3)
y[2:3, ]  # take 2nd and 3rd row, all columns

# Assign values to submatrices
y <- matrix(1:6, nrow=3)

# assign 1st and 3rd rows new values
y[c(1, 3), ] <- matrix(c(1, 1, 8, 12), nrow = 2)  

x <- matrix(nrow = 3, ncol = 3)
y <- matrix(c(4, 5, 2, 3), nrow = 2)
x[2:3, 2:3] <- y  # set bottom right hand corner of x to y

# negative subscripts exclude elements
y <- matrix(1:6, nrow=3)
y[-2, ]  # exclude 2nd row


# *************** Ch 3.2.3 Extended Example: Image Manipulation ***********

# Image files are inherently matrices
# If we have a grayscale image, for each pixel, we storee the intensity, ie the
# brightness of the image at the pixel

# For a color image, the matrices are stores, one for each of RGB, but here 
# we're just going to deal with grayscale

require(pixmap)
# is pic file included in library?
ml <- read.pnm('mona_lisa.pgm')
plot(ml)
str(ml)  # this is a S4 type, as you can tell from the @ instead of +
# More on Ch 9 of s3 v s4

# This is a matrix
ml@grey[100, 200]
plot(ml)
# locator()  # doesnt work because the windowing system isnt X11 or ???

ml@grey[100:110, 100:110] <- 1
plot(ml)

blurpart <- function(img, rows, cols, q) {
    # adds random noise to img, at the range rows, cols of img; img and the
    # reutrn value are both objects of class pixmap; the parameter q controls
    # the weight of the noise, with the result being 1-q times the original
    # image plus q times the random noise
    lrows <- length(rows)
    lcols <- length(cols)
    newimg <- img
    # TODO: send typo correction to author
    # TODO: send function correction to author
    randomnoise <- matrix(nrow=lrows, ncol=lcols, runif(lrows*lcols))
    newimg@grey[rows, cols] <- randomnoise
    newimg@grey <- (1-q)*img@grey + q*newimg@grey
    return(newimg)
}

# origin is at top left


# give Ms. Lisa some anonymity
ml <- read.pnm('mona_lisa.pgm')
rowstart <- 40
rowsize <- 80
colstart <- 93 
colsize <- 55
row <- rowstart:(rowstart + rowsize)
col <- colstart:(colstart + colsize)
q <- 0.6
blur_ml <- blurpart(ml, row, col, q)
plot(blur_ml)

# *************** Ch 3.2.4 Filtering on Matrices **********************
# Filtering can be done with matrices, just as vectors. Be careful with syntax
x <- matrix(c(1:3, 2:4), nrow=3)

# return full row of x if the 2nd column of x is greater than or equal to 3
x[x[, 2] >= 3,]

# The filtering can be defined on a variable separate than the one to be
# filtered

z <- c(5, 12, 13)

# return rows of x where rows of z is odd
# 1 2 
# 3 4

# TODO: send author correction of example on top of page 68
x[z %% 2 ==1, ]
z %% 2==1

m <- matrix(1:6, nrow=3)
# return rows where value in first column is greater than 1 and value in 2nd
# column is greater than 5
# returrnly only last row

# must use &, the vector Boolean and operator, && wouldn't work 
m[m[, 1] > 1 & m[,2] > 5,]

# funky weird output
m[m[, 1] > 1 && m[,2] > 5,]

# matrices are also vectors, so we can do vector like things to it. The matrix 
# is column oriented
m <- matrix(c(5, 2, 9, -1, 10, 11), nrow=3)
which(m > 2)

# *************** Ch 3.2.5 Extended Ex: Generating a Covariance Matrix *******
# Demonstrates R's row() and col() functions
# row(a[2,8]) will return the row number of that element a, which is 2
a <- matrix(1:100, nrow=10)


# TODO: this does not work. tell author, top of page 69
# row(a[2, 8])
row(a)

# Lets consider an example. When writing simulatinon code for multi-variate
# normal distributions - for instance, using mvrnorm() from the MASS library -
# we need to specify a covariance matrix. The key point for our purposes here is
# that the matrix is symmetric; for example the element in row1 , col2 is equal
# to the element in row2, col1

# Suppose we are working with a n-variate normal distribution. Our matrix will
# have n rows and n columns, and we wish each of the n variables to have
# variance 1, with correlation ρ (rho) between pairs of variabeles. 
# For n=3, and ρ=0.2, for example, the desired matrix is as follows

# ( 1 0.2 0.2
# 0.2   1 0.2
# 0.2 0.2   1)

# Here's the code to generate this kind of matrix

makecov <- function(rho, n) {
    m <- matrix(nrow=n, ncol=n)
    m <- ifelse(row(m) == col(m), 1, rho)
    return(m)
}

cov <- makecov(0.2, 3)

# *************************************************************************
# ************* Ch 3.3 Applying Functions to Matrix Rows and Cols *********
# *************************************************************************

# One of the most *famous* and most used functions of R is the *apply() family
# of functions, such as apply(), tapply(), and lapply(). 

# Here we'll look at apply().


# *************** Ch 3.3.1 Using the apply() Function ************************
# General form for matrices
# apply(m, dimcode, f, fargs)

# m is the matrix
# dimcode is the dimension, 1 for rows, 2 for column, work for higher levels?
# f is the function to be applied
# fargs is an optional set of arguments to be supplied to f

z <- matrix(1:6, nrow=3)

# get mean of columns
apply(z, 2, mean)

# get mean of rows
apply(z, 1, mean)

# Our function f divides a two-element vector by the vector (2, 8)
# Recycling would be used if x had a length longer than 2
f <- function(x) x/c(2, 8)

# Send each row of z to f
y <- apply(z, 1, f)
dim(z)  # 3x2
dim(y)  # 2x3

# A bit surprised to get back a 2x3 matrix? The first computation ends up in the
# first column, the 2nd computation in the 2nd column, etc.

# If the function to be applied returns a vector of k components, then the
# result of apply() will have k rows. You can transpose the result if you want
# with t()
yt <- t(apply(z, 1, f))

# If the function returns a scalar, apply will return a vector not a matrix

# sometimes you need to pass additional arguments to your function in apply.
# suppose we have a matrix of 1s and 0s and want to create a vector as follows:
# For each row of the  matrix, the corresponding element of the vector will be
# either 1 or 0, depending on whether the majority of the first d elements in
# that row is 1 or 0. Here, d will be a parameter that we may wish to vary.

# TODO: top of page 72. Not a typo, but could be confusing because the text
# hasn't yet declared the function so a newb might wonder where the function
# came from.

copymaj <- function(rw, d) {
    maj <- sum(rw[1:d]) / d
    return(if(maj>0.5) 1 else 0)
}
x <- matrix(c(1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 1 ,1, 0, 0, 1, 0), nrow=4)
apply(x, 1, copymaj, 2)

# Using apply will not generally speed up your code. The benefit is that it
# makes for very compact code, which'll be easier to read and modify and all
# that. And as R moves closer and closer to parallel processing, functions like
# apply() become more important. 

# For example, the clusterApply() function in the snow package gives R some
# parallel-processing capability by distributing the submatrix data to various
# network nodes, with each node basically applying the given function on its
# submatrix.

# *************** Ch 3.3.2 Extended Ex: Finding Outliers ***********************
# we have retail data in matrix rs.  Each row of the data is for a different
# store, and obs in the row are daily sales figures.

# As a simple approach, let's write code to identify the most deviant
# observation for each store. We'll define that as the observation furthest from
# the median value for that store. 

findols <- function(x) {
    # nested functions common if inner function is short
    findol <- function(xrow) {
        mdn <- median(xrow)
        devs <- abs(xrow - mdn)
        return(which.max(devs))  # tells us where the max occurs
    }
    return(apply(x, 1, findol))
}

# *************************************************************************
# ************* Ch 3.4 Adding and Deleting Matrix Rows and Columns ********
# *************************************************************************
# Technically matrices are of fixed length and dimensions so we can't add or
# delete rows or columns. However, matrices can be reassigned, and thus we can
# achieve the same effect as if we had directly done additions or deletions.

# *************** Ch 3.4.1 Changing the size of a matrix ***********************
# use rbind and cbind
one <- rep(1, 4)
z <- matrix(c(1:4, 1, 1, 0, 0, 1, 0, 1, 0), nrow=4)

# Puts one on top of z
cbind(one, z)

# Can also do recycling
cbind(1, z)

# Be careful with rbind and cbind. They both reassign matrices, which is
# expensive

# If you're going to do a lot of binds, its best to pre-allocate the matrix, and
# then do assignment

# ***** Ch 3.4.2 Extended Ex: Finding closest pairs of vertices in a graph ****
# Finding the distance between vertices in a graph is a common thing in comp
# sci, stats and data science. This problem arises in some clustering algos and
# genomics. 

# Here we look at finding distances between two cities
# Suppose we need a function that inputs a distance matrix, where the element in
# row i, col j gives the distance between city i and city j and the outputs the
# minimum one-hop distance between cities and the pair of cities that achieves
# that minimum.

# returns the minimum value of d[i, j], i != j, and the row/col attaining that
# minimum, for square symmetric d; no special policy on ties

# basically all this does is find the min in the square matrix and return its
# index

mind <- function(d) {
    n <- nrow(d)
    # add a column to identify row number for apply()
    dd <- cbind(d, 1:n)
    wmins <- apply(dd[-n,], 1, imin)
    # wmins will be 2xn, 1st row being indices and 2nd being values
    i <- which.min(wmins[2, ])
    j <- wmins[1, i]
    return(c(d[i, j], i, j))
}

# finds the location, value of the minimum in a row of x
imin <- function(x) {
    lx <- length(x)
    i <- x[lx]  # original row number
    j <- which.min(x[(i+1):(lx-1)])
    k <- i + j
    return(c(k, x[k]))
}

q <- matrix(c(0, 12, 13, 8, 20, 12, 0, 15, 28, 88, 13, 15, 0, 6, 9, 8, 28, 6, 0,
              33, 20, 88, 9, 33, 0), nrow=5)
# test for diagonal symmetry
all(q == t(q))

mind(q)

# TODO: typo page 77, number one should be letter l. see notes in book
# if minimal element in the matrix is unique, there is an alternate approach
# that is far simpler
minda <- function(d) {
    smallest <- min(d)
    ij <- which(d == smallest, arr.ind=TRUE)
    return(c(smallest, ij))
}

# arr.ind =TRUE indicates index will be a matrix index
minda(q)

# *************************************************************************
# ************* Ch 3.5 More on Vector/Matrix Distrinction *****************
# *************************************************************************

z <- matrix(1:8, nrow=4)
length(z)
class(z)
attributes(z)
# a matrix unlike a vector has the dim attribute
# the $ sign shows you that it is a S3 class
nrow(z)
ncol(z)

# *************************************************************************
# ************* Ch 3.6 Avoiding Unintended Dimension Reduction ************
# *************************************************************************

z <- matrix(1:8, nrow=4)
r <- z[2, ]  # in vector format, not matrix
attributes(z)
attributes(r)
class(z)  # matrix
class(r)  # integer
str(z)
str(r)

# This could cause weird exceptions when doing matrix work. If your code for
# some reason extracts a one row matrix, R will automatically make it a vector. 
# The way around this is as follows:
r <- z[2, , drop=FALSE]
dim(r)  # matrix

# For this reason, get in the habit of using drop=FALSE
# you can pass drop because [ is just a function
"["(z,3,2)

# If you have a vector that you want to treat as a matrix, use as.matrix
u <- 1:3
v <- as.matrix(u)
attributes(u)
attributes(v)

# *************************************************************************
# ************* Ch 3.7 Naming Matrix Rows and Columns *********************
# *************************************************************************

# The natural way to refer to row and is by index, but you can also use names
z <- matrix(1:4, nrow=2)
colnames(z)
colnames(z) <- c('a', 'b')
z[, 'a']

# *************************************************************************
# ************* Ch 3.8 Higher Dimensional Arrays **************************
# *************************************************************************

# In a stats context, a matrix is 2 dimensional. Higher than that and you have
# an array. 
# 3 students, each test consists of two scores
firsttest <- matrix(c(46, 21, 50, 30, 25, 50), nrow=3)
secondtest <- matrix(c(46, 41, 50, 43, 35, 50), nrow=3)

# Now lets both of those together into one data structure that'll have two
# layers, one layer per test.

# dim arg specifies 3 rows, 2 columns, 2 layers
tests <- array(data=c(firsttest, secondtest), dim=c(3, 2, 2))
attributes(tests)
# 3rd student, 2nd part of test, 1st test
tests[3, 2, 1]
