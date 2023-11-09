##Writing Functions

#Easy example
f = function(x) x^2

#the above is equivalent to the following multiline form
f = function(x){
  x^2
}

#applying the function
f(10)


g = function(x, y){
  x + y
}


g(1,2)


formals(g)
body(g)
environment(g)


#more sophisticated example: Downloading a package from github
install.packages("devtools")
library(devtools, quietly = TRUE)

#install and load dev version of fdapace
devtools::install_github("functionaldata/tPACE")

library(fdapace, quietly = TRUE)


#check out the documentation at the top of the file on
#github - we'll use this later to write packages

?CreatePathPlot #gives help from a custom function


mtcars


#define a function which takes rows a thru b of mtcars

#Exercise:
#1. define a function which takes rows a thru b of mtcars
#2. define a function which takes rows a thru b of a general data frame `df`


rows_atob = function(a,b){
  mtcars[a:b,]
}

mtcars

rows_atob(1, 5)

rows_atob(2, 4)

rows_atob(b = 4, a = 2) #be aware of naming formals 


rows_df_atob = function(df, a, b){
  df[a:b,]
}

rows_df_atob(iris, 1, 5)
rows_df_atob(mtcars, 1, 5)

vector_f = function(vec){
  vecsq = vec^2
  df = data.frame(vec = vec, vecsq = vecsq)
  df[1:10,]
}

vector_f(1:20)


rm(list=ls())
myFunc_01 <- function() {
  x <- 1
  y <- 2
  c(x,y) }
myFunc_01()



#x <- 2


rm(list=ls())

myFunc_02 <- function() {
  y <- 1
  c(x,y) 
}

myFunc_02()


rm(list=ls())
x <- 1
myFunc_03 <- function() {
  y <- 2
  z <- 4
  myFunc_04 <- function() {
    z <- 3
    c(x,y,z) }
  myFunc_04()
}

myFunc_03()

myFunc_04()

install.packages("codetools")
library(codetools)
rm(list=ls())

myFunc_08 <- function() x + 1
# NOTE myFunc_08 is not self-contained
codetools::findGlobals(myFunc_08)

myFunc_08()


g = function(){
  a = seq(1, 11, by = 2)
  b = 3 * a
  df = data.frame(a = a, b = b)
  return(df)
}

findGlobals(g)

g2 = function(a){
  b = 3 * a
  df = data.frame(a = a, b = b, c = c)
  return(df)
}

findGlobals(g2)

g2(a = seq(1,11,by = 2))


?colnames


df = iris
colnames(x = df)


myFunc_11 <- function(a, b) {
  c(a, b) 
}

myFunc_11()



##--in class lab 

#return rows a through b of a given df (e.g. mtcars)
rows.df = function(a, b, df){
  #-------------- checks on input ----------------
  #first, we need these values to be integers
  if(!is.integer(a) | !is.integer(b)){
    return("a and b must be integers!")
  }
  #second, we need 0 < a <= b <= 32 because of the dimensions of the
  #data set
  if(a > b){
    return("a is greater than b. Please make sure that a <= b.")
  }
  if(a <= 0){
    return("a must be a positive integer.")
  }
  #checking whether b is greater than the number of rows in df
  if(b > dim(df)[1]){
    return("b must be an integer less than or equal to the number of rows in df.")
  }
  
  #check that df is a data frame or a matrix
  if(!is.data.frame(df) & !is.matrix(df)){
    return("df must be a data frame or a matrix")
  }
  
  #-------------- main expression ----------------
  return(df[a:b, ])
}

rows.df(1L, 33L, mtcars)

#square function 1 - note I will not do all of the checks as we did above.
#but in general we'd need to check that the arguments satisfy what we'd like them to be.
square.fun1 = function(vec){
  #vec - a numeric vector
  vec.squared = vec^2
  df = data.frame(Original = vec, Squared = vec.squared)
  #let's use our function from above
  #let's make sure we always get a printout from the data frame
  #if there are not 10 rows, we'll print out all rows
  if(dim(df)[1] < 10){
    return(df)
  }else{
    return(rows.df(a = 1L, b = 10L, df))
  }
}

square.fun1(runif(15))

square.fun1(runif(5))
#square function 2

#Example of Name Masking and searching for variables

x = 1
my.func1 = function(){
  x = 1000
  y = 2
  my.func2 = function(){
    x = 99
    z = 3
    return(c(x, y, z))
  }
  my.func2()
}


my.func1()

##Default arguments in a function

function.default = function(a = 1, b = 2){
  a*b + 2
}

#assigning values to a and b sets the default
function.default()

function.default(2) #treats a to be 2 and b = 2

function.default(1, 5) #a = 1, b = 5

function.default(b = 3) #a = 1, b = 3

##writing arguments in terms of other arguments

function.default2 = function(a = 1, b = 5*a){
  b/a
}

function.default2(10)
function.default2(100) #always gives 5 if b is not changed

function.default2(10, 20)


#what if arguments are missing?
#1) 
function.missing1 = function(arg1, arg2){
  arg1 + arg2
}
function.missing1(2) #throws an error (argument "arg2" is missing)

#2)
function.missing2 = function(arg1, arg2){
  print(c(missing(arg1), missing(arg2)))
}

function.missing2(2) #key thing is that the "missing" statement
#will still be run. So, you can use this to ensure that your function will run



##argument calls are local!

function.5 = function(a = ls()){
  z = 10
  a
}

ls()


function.5 = function(a = ls()){
  force(a)
  z = 10
  a
}

function.5()



function.5() #will call all variables inside the function
function.5(a = ls()) #will store all variables in your environment first
