#fail fast demos
library(tidyverse)
library(magrittr)

#Fail Fast principles
myVec <- c("a", "bcd", "efgh")
if(length(unique(nchar(myVec))) != 1) {
  stop("Error: Elements of your input vector do not have the
  same length!")
}


stopifnot(length(unique(nchar(myVec))) != 1,
          "Error: Elements of your input vector HAVE the same length!")

stopifnot(1 == 1, all.equal(pi, 3.14159265), 1 < 2) # all TRUE
stopifnot(1 == 2, all.equal(pi, 3.14159265), 1 < 2) # first is FALSE
stopifnot(1 == 1, all.equal(pi, 3.14159265), 1 > 2) # third is FALSE
stopifnot(1 == 2, all.equal(pi, 3.14159265), 1 > 2) # first & third is FALSE, only see first



### Functionals
#ex df
myDataFrame_01 = data.frame(A = c(1,10,7,2,1,6),
                            B = c(6,4,9,9,10,2),
                            C = c(1,4,5,3,5,1),
                            D = c(5,-99,4,8,9,3),
                            E = c(-99,9,1,6,8,8),
                            F = c(1,3,4,8,6,5))

#goal: replace all -99s with NA

#why is this bad?
runbadex = FALSE
if(runbadex == TRUE){
  myDataFrame_01$A[myDataFrame_01$A == -99] <- NA
  myDataFrame_01$B[myDataFrame_01$B == -99] <- NA
  #...
  myDataFrame_01$F[myDataFrame_01$F == -99] <- NA
}

#DRY principle - don't repeat yourself 

#will this work?
fix99s_byCol <- function(myCol) {
  myCol[myCol == -99] <- NA
}
#how to fix it?




fix99s_byCol <- function(myCol) {
  myCol[myCol == -99] <- NA
  myCol
}

#how to apply to each col?




#
#
#
#
#
#functionals avoid loops
#is this what we want?
myDataFrame_01 = lapply(myDataFrame_01, fix99s_byCol) 




#to df 
myDataFrame_01 = lapply(myDataFrame_01, fix99s_byCol) %>% 
                 as.data.frame()


#What if different columns employed different coding schemes for
#missing values, e.g., -99, -999 and -8888888?

myDataFrame_02 = data.frame(A = c(1,10,-999,2,1,6),
                            B = c(6,4,9,9,10,2),
                            C = c(1,4,5,-8888888,5,1),
                            D = c(5,-99,4,8,9,3),
                            E = c(-99,9,1,6,8,8),
                            F = c(1,3,4,8,6,5))








####
fixMissing <- function(myCol, myValue) {
  myCol[myCol == myValue] <- NA
  myCol
}

missingval_key = c(-999,-999,-8888888,-99,-99,-99)


mapply(myDataFrame_02, 
       FUN = fixMissing, 
       myValue = missingval_key) %>% 
  as.data.frame()
#Notice that since the function needed another argument (myValue)
#we had to provide that to apply. Because of that, I had to
#specify later arguments with exact matching to prevent misparsing.
#Also I had to use mapply() instead of lapply()
#to allow myValue to range over itselements as a vector.

?apply




### Anonymous Functions

#sometimes you only need to use a function once and it's really simple.
#in that case anonymous functions can be useful.



#########################
#anonymous
lapply(myDataFrame_01, function(x) x + 3) %>% 
  as.data.frame()

#named version
named_fun <- function(myCol) {
  myCol <- myCol + 3
  myCol
}

lapply(myDataFrame_01, named_fun) %>% 
  as.data.frame()


###LAB EXERCISE###
#Create a compact and robust function which, 
#when passed an n × m numeric data frame, 
#returns, for each numeric column, the
#1. Mean
#2. Median
#3. Standard Deviation
#4. Variance
#5. Quantiles
#6. IQR.
#If the column is non-numeric, return a vector of NAs.
#Try to make it as efficient and nonredundant as possible. 

#To test it out, use the Palmer penguin data. 
library(palmerpenguins)
penguins





####Benchmarking
library(microbenchmark)

set.seed(100)
x = runif(min = 0, max = 1, n = 100)
micro = microbenchmark(sqrt(x), x^0.5, times = 1000)
micro

#Interpretation:
#It takes roughly 271.133 ns on avg to compute the square root of 100 numbers 
#using sqrt(). That means that if you repeated that operation a 
#million times, i.e., compute the square root on 10,000,000 numbers,
# it would take 0.271 seconds.

#On the other hand, it takes roughly 1564.396 ns on avg to compute 
#the square root of 100 numbers using x^0.5. That means that if you 
#repeated that operation a million times, i.e., compute the square root 
#on 10,000,000 numbers, it would take ~1.564 seconds.

#On average, sqrt() is about 1564.396/271.133 = 5-6x faster than x^0.5.
1564.396/271.133

