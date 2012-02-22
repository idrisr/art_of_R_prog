# ****************************************************************************
# ********** 4.2.4 Extended Ex: Text Concordance *****************************
# ****************************************************************************

findwords <- function(tf) {
    # read in the words from the file, into a vector of mode character
    txt <- scan(tf, "")
    wl <- list()
    for(i in 1:length(txt)){
        wrd <- txt[i]  # ith word in input file
        wl[[wrd]] <- c(wl[[wrd]], i)
    }
    return(wl)
}

tf <- 'author_letter.R'
wl <- findwords(tf)

# ****************************************************************************
# ********** 4.3 Accessing List Components and Vectors ***********************
# ****************************************************************************

j <- list(name='joe', salary=55000, union=TRUE)
names(j)
ulj <- unlist(j)  # vector -- all come back as same type

z <- list(a=5, b=12, c=13)
ulz <- unlist(z)
names(ulz) <- NULL
ulz <- unname(ulz)

# ****************************************************************************
# ********** 4.4 Applying Functions to List **********************************
# ****************************************************************************

# ********** 4.4.1 lapply() and sapply() *************************************
# lapply works like apply, calling a function for each element in a list
lapply(list(1:3, 25:29), median)  # returns a list
sapply(list(1:3, 25:29), median)  # returns a vector

# ********** 4.4.2 Extended Ex: Text Concordance Continued *******************
# sorts wrdlist, the output of findwords() alphabetically by word
alphawl <- function(wrdlist) {
    nms <- names(wrdlist)  # the words
    sn <- sort(nms)
    return(wrdlist[sn])  # return rearranged version
}

swl <- alphawl(wl)

# We can sort by word frequency in a similar way

freqwl <- function(wl) {
    freqs <- sapply(wl, length)
    sfreqs <- sort(freqs, decreasing=TRUE)
    return(wl[names(sfreqs)])
}

sfwl <- freqwl(wl)

# order returns the indices of a sorted vector with respect to the original
# vector
x <- c(12, 5, 13, 8)
order(x)  # returns 2 4 1 3

doc <- '~/learning/model_thinking/1.txt'
lecture <- findwords(doc)
sdoc <- freqwl(lecture)
nwords <- length(sdoc)
# TODO: i think this is an error. need to apply sapply first
barplot(sdoc[round(0.9 * nwords):nwords])
barplot(sapply(sdoc[0:round(0.1 * nwords)], length))

# ********** 4.4.3 Extended Ex: Back to the Abalone Data *******************
g <- c('M', 'F', 'F', 'I', 'M', 'M', 'F')
lapply(c('M', 'F', 'I'), function(gender) which(g==gender))

# ****************************************************************************
# ********************** 4.5 Recursive Lists *********************************
# ****************************************************************************

# list of lists. oh lord
b <- list(u=6, v=12)
c <- list(w = 13)
a <- list(b, c)

# Make a into a 2-component list, with each component itself also being a list
c(list(a=1, b=2, c=list(d=5, e=9)))

# Recursive controls whether flattenting occurs, default FALSE
c(list(a=1, b=2, c=list(d=5, e=9)), recursive=TRUE)
