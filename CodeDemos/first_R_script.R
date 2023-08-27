# Before we start:
# ================
# * Participation is super important in this course
#   because it's the main way to get help -- and the main way
#   I'll know if you all are following what's going on!
#
# * Show your work (your code is your work). That's what you'll be evaluated on
#
# * Don't copy code. If you use another person/resource for an idea, cite it.
#
# * Label your plots and axes!
#
# * Recommended style guide: https://style.tidyverse.org/index.html


# Setup
# =====
# Download and install:
#
# * R <www.r-project.org>
#
# * RStudio <www.rstudio.com> 


#Making your first plot

# store x as 1 through 10
x  =  1:10

x

# store y as x squared
y  =  x^2


# very simple plot of y against x 
plot(y ~ x)

# more sophisticated plot with blue line and points
plot(y ~ x, type = "l", col = "blue", main = "y vs. x")

#check out the arguments
?plot


#what happens when I try to do something weird?
z  =  -3
sqrt(z)

z  =  complex(real = -3)
sqrt(z)

#sneak peek: data.frames
df = data.frame(x, y)
df

### Installing Packages
#ex: running a function that is not in any package currently downloaded in your R session

#say I wanted to start a "ggplot":
ggplot(df)

#First, let's install the package ggplot2
install.packages("ggplot2")
?ggplot

#note - the above could not find the function ggplot

??ggplot2


#so, I just need to load that package to my current R session
# for this, I can use library() or require()
library(ggplot2)

#don't worry about the syntax of ggplot2 yet (future lesson). 
#it's just an example to show you how to download new packages
ggplot(data = df, aes(x = x, y = y)) + 
  geom_line() + 
  geom_point() + 
  ggtitle("y vs. x")
  


#Examples of clearing environment

#clear just x
rm(x)

#ls: looks at everything
ls()

#clear everything
rm(list = ls())

#now what will ls() return?
ls()

#clear the console
cat("\014")


#install knitr

install.packages("knitr")

library(knitr)

install.packages("rmarkdown")

library(rmarkdown)






