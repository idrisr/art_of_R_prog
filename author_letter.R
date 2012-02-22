# Hello Prof. Matloff,

# I have stared making my way through The Art of R Progamming. I've been
# programming R for one year, and your book is exactly what I want to read right
# now to get better at R and data analysis. 

# As I go through the chapters, I type out all the code in the text. While doing
# so, I've found a few lines of codes that I *believe* are typos.  

# I plan to do the same for subsequent chapters. Please let me know if there is a
# format that I could send these to you which is more convenient for you.

# Best,
# Idris

# Chapter 3
# Page 65
# In function blurpart
# ncols not defined. Should be lcols
randomnoise <- matrix(nrow=lrows, ncol=ncols, runif(lrows*lcols))
randomnoise <- matrix(nrow=lrows, ncol=lcols, runif(lrows*lcols))


# Also in the function blurpart, we need to insert this line to prevent a matrix
# dimension size mismatch
newimg@grey[rows, cols] <- randomnoise
newimg@grey <- (1-q) * img@grey + q * randomnoise

# Chapter 3
# Page bottom of page 67 to top of page 68
x <- matrix(c(1:3, 2:4), nrow=3) # current value of x as shown in text
z <- c(5, 12, 13)
x[z %% 2 == 1, ]
# Output as shown
     # [,1] [,2]
# [1,]   1    4
# [2,]   3    6

# Output should be:
     # [,1] [,2]
# [1,]   1    2
# [2,]   3    4

# First paragraph section 3.2.5, page 69
# The following expression is invalid
a <- matrix(1:50, nrow=5)
row(a[2, 8])
# Error in row(a[2, 8]) : 
# a matrix-like object is required as argument to 'row/col'

# Page 77, the paragraph starting "Line 19 is noteworthy..."
# The statement 
(i+1):(1x-1) # should be
(i+1):(lx-1) # replace the 2nd '1' with the letter l
# This mistake also happens again in the paragraph


