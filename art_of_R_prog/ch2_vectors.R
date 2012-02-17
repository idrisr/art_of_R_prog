findruns <- function(x, k)
{
    #preallocate vector size
    n <- length(x)
    runs <- vector(length = n)
    count <- 0
    for (i in 1:(n-k+1)) { 
        if (all(x[i:(i+k-1)]==1)) {
            count <- count + 1
            runs[count] <- i
        }
    }
    if (count > 0) {
        runs <- runs[1:count]
    } else runs <- NULL
    return(runs)
}

x <- runif(100, 0, 1)
x <- as.integer(x>0.5)
findruns(x, 5)

#**************************************************************
#  2.5.2  Extended Example: Predicting Discrete-Valued Time Series
#**************************************************************
#if the number of 1s in the previous k time periods is at least k/2, we'll
#predict the value to be 1, otherwise our prediction is 0.
preda <- function(x, k)
{
    n <- length(x)
    k2 <- k/2
    # the vector pred will contain our predicted values
    pred <- vector(length=n-k)
    for (i in 1:(n-k)) { 
        if (sum(x[i:(i+k-1)]) >= k2){ 
            pred[i] <- 1 
        }
        else { 
            pred[i] <- 0
        }
        }
    # pred has predicted values
    # x[(k+1):n] has actual values
    return(mean(abs(pred-x[(k+1):n])))
}
preda(x, 4)

#**************************************************************
#**************************************************************
#               2.6  Vectorized Operations
#**************************************************************
#**************************************************************


#**************************************************************
#               2.6.1  vector in, vector out
#**************************************************************
# nothing too interesting

#**************************************************************
#               2.6.2  vector in, matrix out
#**************************************************************
z12 <- z12 <- function(z) return(c(z, z^2))
x <- 1:8
z12(x)
matrix(z12(x), ncol = 2)

sapply(1:8, z12)

#**************************************************************
#**************************************************************
#               2.7 NA and NULL Values
#**************************************************************
#**************************************************************
# NA means missing value
# NULL means value doesn't exist, as opposed to being existent but unknown

#**************************************************************
#               2.7 NA and NULL Values
#**************************************************************

#************* 2.7.1 using na ********************************* 
x <- c(88, NA, 12, 168, 13)
mean(x)  # no answer
mean(x, na.rm=TRUE)  # answer

x <- c(88, NULL, 12, 168, 13)
# R automatically skips over the NULL values
mean(x)  # answer

#************* 2.7.1 using NULL ********************************* 
# One use of NULL is to build up vectors in loops, in which each iteration adds
# another element to the vector. In this simple example, we build up a vector of
# even numbers

z <- NULL
for (i in 1:10) 
    if (i %% 2 == 0) 
        z <- c(z,i)

length(NA)  # 1
length(NULL)  # 0

#*********************************************************************
#               2.8 Filtering
#*********************************************************************

#************* 2.8.1 Generating Filtering Indices ******************** 
z <- c(5, 2, -3, 8)
w <- z[z*z > 8]
# extract all elements of z whose elements squared are greated than 8, then
# assign that subvector to w

x <- c(1, 3, 8, 2, 20)
x[x > 3] <- 0
x

#************* 2.8.2 Filtering with the subset() function ************ 
x <- c(6, 1:3, NA, 12)
x[x > 5]

# When you dont want NAs, subset saves you the trouble of removing it yourself
subset(x, x > 5)


#************* 2.8.3 The Selection Function which() ******************
z <- c(5, 2, -3, 8)
which(z*z > 8)  # gets the indices you want

z * z > 8

#*********************************************************************
#            2.9 A Vectorized if-then-else: The ifelse() function
#*********************************************************************

# ifelse(b, u, v) - vectorized if else
# b is a Boolean vector
# u and v are vectors
x <- 1:10
y <- ifelse(x %% 2 == 0, 5, 12)

x <- c(5, 2, 9, 12)
ifelse(x > 6, 2*x, 3*x)

#************* 2.9.1 Extended Example: A Measure of Association ******
# In assessing the statistical relation of two variables, there are many
# alternatives to the standard correlation measure (Pearson product-moment
# correlation). Also, Spearman rank correlation.

# Here, let's propose a new such measure, not for novel stats measure, but to demo
# some of the shites we can do with R

# findud() converts vectors v to 1s, 0s, representing an element increasing or
# not, relative to the previous one; output length is 1 less than input
findud <- function(v) {
    # stagger v against itself and then delete
    # vud <- v[-1] - v[-length(v)]
    # Also can be done like this
    # v <- diff(v)
    # return(sign(v))

    return(ifelse(v > 0, 1, -1))
}

udcorr <- function(x, y) {
    # apply function to each element in list
    ud <- lapply(list(x, y), findud)
    # how often are both increasing / decreasing at same time
    return(mean(ud[[1]] == ud[[2]]))
}

x <- c(5, 12, 13, 3, 6, 0, 1, 15, 16, 8, 88)
y <- c(4, 2, 3, 23, 6, 10, 11, 12, 6, 3, 2)

udcorr(x, y)

#************* 2.9.2 Extended Example: Recoding an Abalone Data Set *********
# Due to the vector nature of the arguments,  you can nest ifelse() operations.
# In the following example, gender is coded as M, F, or I (infant). We wish to
# recode those characters as 1, 2, or 3. The real data set consists of more than
# 4000 observations, but we'll just use a few

g <- c('M', 'F', 'F', 'I', 'M', 'M', 'F')

ifelse(g == "M", 1, ifelse(g == 'F', 2, 3))
args(ifelse)

# form subgroups according to gender
grps <- list()
for (gen in c('F', 'M', 'I')) grps[[gen]] <- which(g==gen)
