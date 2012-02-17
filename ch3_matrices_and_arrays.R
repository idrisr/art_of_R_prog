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
mtrush1 <- read.pnm('mtrush.pgm')
