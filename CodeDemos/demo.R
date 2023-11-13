iris

x=11


rowslicer = function(firstRow, lastRow){
  return(iris[firstRow:lastRow, ])
}


library(codetools)
findGlobals(rowslicer) #what do I need to run, 
#what are the not local variables
#what are the variables not in scope



rowslicer(1,3)

rowslicer(firstRow = 1, lastRow = 3)

rowslicer(lastRow = 3, firstRow = 1)

rowslicer(3, 1)

rowslicer(1:3, 5)
# gonna give a warning
# but funcitons like
rowslicer(1,5)

(1:3):5
1:5


#formal and calling arguments??
rowslicer(firstRow = 1, lastRow = 3)

#formals:
#firstRow and lastRow

#calling:
#1 and 3




rowslicer_vectorized = function(firstRow_vec, lastRow_vec){
  l = list()
  for(i in 1:length(firstRow_vec)){
    l[[i]] = iris[firstRow_vec[i]:lastRow_vec[i], ]
  }
  return(l)
}

rowslicer_vectorized(firstRow_vec = c(1, 10), lastRow_vec = c(3, 13))

#take df as an arg
rowslicerdf = function(myDataFrame, firstRow, lastRow){
  return(myDataFrame[firstRow:lastRow, ])
}

data()

rowslicerdf(myDataFrame = ChickWeight, firstRow = 1, lastRow = 5)


rowslicerdf_force = function(myDataFrame, firstRow, lastRow){
  force(myDataFrame)
  force(firstRow)
  force(lastRow)
  return(myDataFrame[firstRow:lastRow, ])
}


x = 1:5
mean(x)
x = c(1, 4, NA, 7, 10)
mean(x)
mean(x, na.rm = TRUE)

#Name
mean()
#Description
takes averages
#arguments
a vector x and an option to remove nulls
#example
x = 1:5
mean(x)
x = c(1, 4, NA, 7, 10)
mean(x)
mean(x, na.rm = TRUE)


take_avg = function(x){
  (x[1]+x[2]+x[3])/3
}

take_avg(x = 1:3)
take_avg(x = 1:2)

take_avg("asdjhasjdha")



