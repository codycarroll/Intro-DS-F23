#Data Structures 1

#double vector
double_vector = c(1, 3.14, 99.999)

#integer vector -- L is used to indicate that the preceding number
#should be stored as an integer
integer_vector = c(1L, 14L, 23L)

#logical vector -- must be written in caps (TRUE/FALSE) or 
#shorthand (T/F), normally long form is preferred stylistically
logical_vector = c(TRUE, FALSE, T, F)

#character vector -- entries must be surrounded by quotations ""
char_vector = c("this", "is a", "string")

#checking the types of each of these vectors
typeof(integer_vector)
typeof(logical_vector)
typeof(char_vector)

#str displays the structure of the object
str(integer_vector)
str(logical_vector)
str(char_vector)

#viewing the vectors
char_vector #output is shown with spaces

#length of the vector
length(char_vector)

exnum = 3/4
#note the above does the division, but if you want to keep 
#its fractional form, store as a string
exnum = "3/4" 
#or
exnum = c("3/4")

num_vec = c(3.14, 4, 15, 23, 1.49, 17.5, 13)

first_names = c("Austria", "Vi")

#first attempt is to input text and then a number
vec = c("Cody", 30)

vec #this automatically coerces 30 to be a string
#we need to be careful about paying attn to coercion in R! (there were no
# errors thrown.)


##Vector subsetting
atomic_vector = c(1, 2, 3, 4, -99, 5, NA, 4, 22.233)

typeof(atomic_vector)

#calling the 3rd entry. use brackets []. 
#Note to Python/C/C++/... users -- counting starts with 1 not 0!
atomic_vector[3]

#in general, we can put any length of an index inside brackets
#for subsetting
indx = c(1, 3, 7)
atomic_vector[indx]

#calling an index that doesn't exist. In our example the 10th entry
atomic_vector[10]

#calling all values except for the 7th term (which is NA)
atomic_vector[-7]

#calling all non-NA values
#1 - which values are NA?
logical.of.nas = is.na(atomic_vector) #logical vector TRUE if NA

#2 - create index of which values above are TRUE
indx.of.nas = which(logical.of.nas)

#3 - keep everything that is not NA
atomic_vector[-indx.of.nas]

#------------------------------
##Operations on vectors
vec1 = c(1, 3, 5)
vec2 = c(3.14, 1.49, 23)

#1 adding two vectors of the same length - entry-wise addition
vec_sum = vec1 + vec2

#2 subtraction of two vectors
vec_diff = vec1 - vec2

#3 entry-wise multiplication of two vectors
vec_mult = vec1 * vec2

#4 entry-wise quotients of two vectors
vec_quotient = vec1 / vec2

#5 exponentiation
vec_power = vec1 ^ vec2
vec1_exp = exp(vec1)


###each of these can also be done for scalar operations
#example
vec_scalar = vec1 * 3

##dot products (x^T x)
vec_dot = vec1 %*% vec2

##using standard functions on a vector
#squareroot of each value
vec_sqrt = sqrt(vec1)
vec_sqrt

#exponentiation
vec.exp2 = exp(vec1)

#-----------------------------
#summing every value in a single vector
vec1_sum = sum(vec1)

#average every value in a single vector
vec1_mean = mean(vec1)

#standard deviation of a vector
vec1_sd = sd(vec1)

#extremes of vector
vec1_min = min(vec1) #the minimum value
vec1_min_indx = which.min(vec1) #the index of the minimum value

vec1_max = max(vec1) #the maximum value
vec1_max_indx = which.max(vec1)


#---------------
#getting rid of NA's when doing operations
vec3 = c(99.7, 99.2, 91, NA, 92, NA)
#sum the vector ignoring NA's 
sum(vec3, na.rm = TRUE)

#note if we don't use na.rm = TRUE, then we will get NA 
sum(vec3)

#mean of vector ignoring NA's
mean(vec3, na.rm = TRUE) #note - this will completely ignore
#NA from any calculation

#if you instead want to treat NA's as zero (for calculating a 
#mean)
sum(vec3, na.rm = TRUE) / length(vec3)

