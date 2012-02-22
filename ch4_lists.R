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
l <- findwords(tf)

# ****************************************************************************
# ********** 4.3 Accessing List Components and Vectors ***********************
# ****************************************************************************

j <- list(name='joe', salary=55000, union=TRUE)
names(j)
ulj <- unlist(j)  # vector -- all come back as same type

z <- list(a=5, b=12, c=13)
ulz <- unlist(z)
