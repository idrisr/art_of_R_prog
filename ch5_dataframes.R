# On an intuitive level, a data frame is like a matrix, with a two dimensional
# rows-and-columns structure
# its different than a matrix in that each column can have a different mode
# On a technical level a dataframe is list. Each column of a dataframe does not
# have to be a vector, but this is rare in practice. So essentially each column
# of the dataframe is going to be a vector. 

# ****************************************************************************
# ********************** 5.1 Creating Data Frames ****************************
# ****************************************************************************

kids <- c('Jack', 'Jill')
ages <- c(12, 10)
d <- data.frame(kids, ages, stringsAsFactors=FALSE)


# ********************** 5.1.1 Accessing Data Frames *************************
# list a list
d[[1]]
d$kids

# like a matrix
d[, 1]
str(d)

# ******* 5.1.2 Extended Ex: Regression Analysis of Exam Gres ****************
examsquiz <- read.csv('exams')
head(examsquiz)

# ****************************************************************************
# ******************** 5.2 Other Matrix-Like Operations **********************
# ****************************************************************************

# ****************** 5.2.1 Extracting Subdata Frames *************************
examsquiz[2:5, ]
examsquiz[2:5, 2]
class(examsquiz[2:5, 2])

# drop: vector or dataframe form
examsquiz[2:5, 2, drop=FALSE]

examsquiz[examsquiz$Exam.1 >= 3.8, ]

# ****************** 5.2.2 More on Treatment of NA Values ********************
x <- c(2, NA, 4)
mean(x)
mean(x, na.rm=TRUE)

subset(examsquiz, Exam.1 >= 3.8)

# use only rows where all data not NA
complete.cases

# ****** 5.2.3 Using the rbind() and cbind() Functions and alternatives *****
rbind(d, list("Laura", 19))
eq <- cbind(examsquiz, examsquiz$Exam.1 - examsquiz$Exam.2)
examsquiz$ExamDiff <- examsquiz$Exam.2 - examsquiz$Exam.1

# *********************** 5.2.4 Applying apply() ****************************
# You can use apply() on data.frames.
# max grade of each student
apply(examsquiz, 1, max)
