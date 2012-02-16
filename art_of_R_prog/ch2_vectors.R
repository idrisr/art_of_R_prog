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
#   Extended Example: Predicting Discrete-Valued Time Series
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
        if (sum(x[i:(i+k-1))]) >= k2 pred[i] <- 1 else pred[i] <- 0
        }
    # pred has predicted values
    # x[(k+1):n] has actual values
    return(mean(abs(pred-x[(k+1):n])))
}
