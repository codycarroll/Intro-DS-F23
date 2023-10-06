##Plotting in R

#Load the Food data from Github

library(readr)
library(ggplot2)
library(magrittr)


#set wd to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

food = read_csv(file = "../Data/Food.csv")
head(food)

unique(food$pnns_groups_1)

#How can we combine/refactor our categories to consolidate duplicates?
fv_idx = (food$pnns_groups_1 == "fruits-and-vegetables")
food$pnns_groups_1[fv_idx] = "Fruits and vegetables"

newpnns1 = fct_collapse(food$pnns_groups_1,
                          other = c("unknown"),
                          `Cereals and potatoes` = c("Cereals and potatoes", 
                                                     "cereals-and-potatoes"),
                          `Sugary snacks` = c("Sugary snacks", 
                                              "sugary-snacks"),
                          `Salty snacks` = c("Salty snacks", 
                                             "salty-snacks"),
                          )

food$food_group = newpnns1


library(ggplot2)

#---------------------------
#Using ggplot
#Important point - these functions now work in layers. So, you have 
#to first specify a ggplot() object with aesthetics (i.e., aes()), and
#then add geometric objects, or other objects to the original plot

#Interesting (and useful) - ggplots are objects, so they can be saved and loaded
#easily

#plot a bar chart of pnns_groups_1
ggplot(food) + 
  geom_bar(aes(x = food_group, fill = food_group))

#text trainwreck on bottom; to rotate xaxis labels use this:
ggplot(food) + 
  geom_bar(aes(x = food_group, fill = food_group)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#plot in a new window
dev.new()
ggplot(food) + 
  geom_bar(aes(x = pnns_groups_1, fill = pnns_groups_1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#adding a few arguments for labels

ggplot(food) + 
  geom_bar(aes(x = food_group, fill = food_group)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  xlab("food type") + 
  ggtitle("Food Groups")


#------------------------
#Pairwise scatter plots
data = swiss
?swiss

#scatter plot of every variable against one another in a grid
plot(data)

#scatter plot of say the first 3 variables
plot(data[, 1:3])

#equivalent way of doing this with ggplot w/ ggally pkg:

library(GGally)
ggpairs(data, aes(alpha = 0.4))

#--------------------

##Histograms
dev.new()

gg1 = ggplot(food, aes(x = energy_100g)) + 
  geom_histogram(color="black", fill="white")
gg1

dev.new()
gg1


#same histogram but with more bars (100 of them)
gg2 = ggplot(food, aes(x = energy_100g)) + 
  geom_histogram(color="black", fill="white", bins = 100) 
gg2

#side-by-side plotting w/ ggarrange
library(ggpubr)
ggarrange(gg1, gg2)


#adding a density curve on top of a histogram

ggplot(food, aes(x = energy_100g)) + 
  geom_histogram(aes(y = after_stat(density)), 
                 color="black", 
                 fill="white", 
                 bins = 100) +
  geom_density(alpha=.2, fill="red")


#-------------------------------
#box plots
ggplot(food) + 
  geom_boxplot(aes(x = energy_100g))


#show boxplot of this variable across food groups in
#pnns_groups_1
ggplot(food) + 
  geom_boxplot(aes(x = energy_100g, fill = food_group))

ggplot(food) + 
  geom_boxplot(aes(x = energy_100g, fill = food_group)) + 
  coord_flip()

ggplot(food) + 
  geom_violin(aes(y = energy_100g, 
                  x = food_group, 
                  fill = food_group))

ggplot(food) + 
  geom_violin(aes(y = energy_100g, 
                  x = food_group, 
                  fill = food_group)) + 
  coord_flip()


#-------------------------------
#working with qplot and ggplot2 on the diamonds dataset

#first, read in the diamonds data set
diamonds = read.csv(file = "../Data/diamonds.csv",
                     header = TRUE)

#create a scatterplot of carat against price where points are colored 
#according to diamond color

plot1 = ggplot(diamonds, 
               aes(x = carat, y = price, colour = color))
plot1 #note this is blank because we haven't told it what kind of plot yet

plot2 = plot1 + 
  geom_point(aes(alpha = 0.25))
plot2

#say now we want to add shapes for the cut of the diamond
plot3 = plot2 + 
  geom_point(aes(shape = cut))
plot3

#adding a smooth line to the original plot
plot3 = plot3 + 
  geom_smooth()
plot3

plot3 

# the above gives a different smooth line to every color
# but what if we just want a single line
# for all data?

# to do this, we need to re-order our specifications.
plot1b = ggplot(diamonds, 
                aes(x = carat, y = price))

plot2b = plot1b + 
  geom_smooth() #line first

plot2b


plot3b = plot2b + 
  geom_point(aes(colour = color, 
                 shape = cut)) +  #then add colors and shapes  
  geom_smooth() #line first

plot3b


#use inherit.aes = FALSE to ignore previously defined aesthetics
plot4 = plot2 + 
  geom_point(aes(shape = cut)) +
  geom_smooth() + 
  geom_smooth(inherit.aes = FALSE, aes(x = carat, y = price))
plot4

#you can also store geometric objects and apply them to any plot! (super useful!!)
bestfitline = geom_smooth(method = "lm", 
                          se = TRUE, 
                          colour = alpha("steelblue", 0.5), 
                          linewidth = 2)

line = lm(price ~ carat, data = diamonds)
summary(line)

bestfitline$computed_geom_params

plot4 + 
  bestfitline

#changing axis and legend titles
plot4 + 
  xlab("Carat") + 
  ylab("Price") + 
  guides(fill=guide_legend(title="Color"))


#Exercise:

#1. Explore other plots you can make with ggplot and the diamond dataset. 
#Can you make a hypothesis about two characteristics and then evaluate it with a ggplot?

#2. Plot some data visualizations of your class survey data. Save the most interesting plots and interpret your findings. 


