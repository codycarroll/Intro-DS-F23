#Lists are useful because we can store **heterogeneous** data:
# i.e., data of different types, and of different lengths

my_list = list(c(1, 2, 3), c(T, F), c(3.1415, 9), "abc")
my_list

#note that entries in a list are indexed with double brackets [[ ]]

##calling the first entry (which is a vector c(1, 2, 3))
my_list[[1]]
my_list[[4]]

str(my_list[[1]])
#what happens if we use a single bracket?
my_list[1]

str(my_list[[1]]) #numeric
str(my_list[1])

##try calling matrix rows in a list. Example of recursive
## calling

my_list2 = list(matrix(1:9, nrow = 3), c(T, F, T), "ade")
#let's get the second row of the matrix in the first entry
my_list2[[1]][2,]

#equivalent to:
mat = my_list2[[1]]
mat[3,,]


#getting the first two entries in the list
my_list2[1:2] #this will be returned as a list

##naming and calling entries of a list using its name
my_list3 = list(Matrix = matrix(1:9, nrow = 3), 
                Logicals = c(T, T, F),
                Numeric = c(3.145, 3/4))


#calling the Logicals vector
my_list3[[2]]
#this is equivalent to...
my_list3$Logicals


##The unlist() function - this function will "unpack" a list by its
# components one at a time and concatenate them all to be a single vector.
# Note - this will cause coercion. Also, unlisting matrices takes entries
# according to columns
vec = unlist(my_list3)
str(vec)
typeof(vec)


diff_types = list("abc", matrix(1:4, nrow = 2), c(TRUE, FALSE))
typeof(unlist(diff_types))


#as an example of unlisting a matrix
unlist(matrix(1:9, ncol = 3)) #this didn't work! so, the input object must 
# be a list!
unlist(list(matrix(1:9, ncol = 3)))

as.numeric(matrix(1:9, ncol = 9))

##########
#Playing around with Data Frames in R
?ToothGrowth
ToothGrowth

#Information - data frames are stored in the following way:
# rows = observations or samples
# columns = measurements / features / variables 
# this is the standard way of storing data for the use of Machine Learning and
# regression modeling


#In general, we name the variables (i.e. the columns) and can easily call them 
# using the $ argument. This is the same as in lists
ToothGrowth$dose

#We can also subset in the same way that we subset matrices

#get the second row
ToothGrowth[2, ] #this is still stored as a data frame since each column may have
# different data types

ToothGrowth[2, 2]

#bracket order:
#ToothGrowth[rows,cols]


#Practice:

#1. Give me all the data for the guinea pigs who got VC
#Save it in a df called vc_guineapigs

ToothGrowth

idx = ToothGrowth$supp == "VC"
vc_guineapigs = ToothGrowth[idx,]

vc_guineapigs = ToothGrowth[ToothGrowth$supp == "VC",]

#special solution: 
vc_guineapigs = ToothGrowth[1:30,] #hard coded solution


#2. Give me all the guinea pigs who had teeth longer
# than 10 mm. Save it in a df called long_teeth

index = (ToothGrowth$len >= 10)
long_teeth = ToothGrowth[index, ]
nrow(long_teeth)

#3. Return all the guinea pigs who received a dose of 
# 2 mg/day of OJ. What's the average tooth length of these 
# guinea pigs? 

index1 = (ToothGrowth$supp == "OJ")
subset1 = ToothGrowth[index1, ]
index2 = (subset1$dose == 2)
subset2 = subset1[index2, ]

subset2

ToothGrowth[(ToothGrowth$supp == "OJ")&(ToothGrowth$dose == 2),]

index1 = (ToothGrowth$supp == "OJ")
index2 = (ToothGrowth$dose == 2)
final = ToothGrowth[(index1)&(index2), ]

sum(final$len)/nrow(final)
mean(final$len)

##Creating our own data frame
df1 = data.frame(Logicals = c(T, F, T, F), Age = c(21, 17, 32, 81))
str(df1)


#Caution about data frames:
# - whenever strings are input as a variable, data frames will automatically
# store them as factors (categorical data with a certain number of levels).
# to avoid this, use the argument stringsAsFactors = FALSE.

#Example - 
dffactors = data.frame(Logicals = c(T, F, T, F), Age = c(21, 17, 32, 81), 
                         Strings = c("my", "name", "is", "bob"))
str(dffactors) #key point - this treats the Strings variable as a factor

dfstrings = data.frame(Logicals = c(T, F, T, F), Age = c(21, 17, 32, 81), 
                         Strings = c("my", "name", "is", "bob"),
                         stringsAsFactors = FALSE)
str(dfstrings) #yay!

#Appending two data frames
#rough example - appending Strings as a new variable for df1
Strings = c("my", "name", "is", "bob")

df2trial = cbind(df1, Strings)
str(df2trial)
#note - the strings were treated again as factors

#trial to see if we can avoid that
df2trial2 = cbind(df1, Strings, stringsAsFactors = FALSE)
#success!!

##Adding a new observation to the data frame dfstrings
# the new observation is FALSE, 19, and "lucy"

##First, the wrong way of doing this
newdf_wrong = rbind(dfstrings, c(FALSE, 19, "lucy"))
str(newdf_wrong)
#the issue is that the first vector all got coerced to be a character,
# and then each column of the data frame subsequently got coerced to characters!


#To avoid coercion, rbind data frames instead
newdf_right = rbind(dfstrings, 
                      data.frame(Logicals = FALSE, Age = 19, 
                                 Strings = "lucy"))

str(newdf_right)


#one more note, data frames that are rbinded together must have the same 
#variable names
wont_work = rbind(dfstrings, 
                   data.frame(Truth = FALSE, Age = 19, 
                   Strings = "lucy"))

maybe_work = rbind(dfstrings, 
                    data.frame(FALSE, 19, 
                               "lucy"))
#also does not work - we must name each new variable to be the same as the 
# original data frame

######################
#What can happen when adding a new column of the wrong length to dfstrings
vec1 = -1
vec2 = -1:-2
vec3 = -1:-99
vec4 = c("-1", -2)
vec5 = c("a", -1, -2)

cbind(dfstrings, vec1) #repeats -1 to fill in missing entries
cbind(dfstrings, vec2) #repeats the vector to fill in missing entries
cbind(dfstrings, vec3) #throws an error as it doesn't automatically repeat the
                         #original data frame
cbind(dfstrings, vec4) #repeats the last values, and coerces to be factors
                         #since we did not specify stringsAsFactors = FALSE

cbind(dfstrings, vec5) #throws an error - did not repeat character values

#swapping the columns of a data frame
dfstrings
dfstrings[, c(1, 3, 2)]

