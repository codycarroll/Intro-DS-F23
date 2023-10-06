library(tidyverse)
library(ggplot2)

#Let's look at a dataset about dogs!

dogs = read.csv("~/Desktop/dogs.csv")

###1. Data Cleaning
#Look at the data as it is now... it's a bit messy!

#First let's tidy it up a bit.

#deal with missing values
dogs[dogs == "no data"] = NA    # Replace particular value with NA
dogs[(dogs == "")|(dogs == "-")] = "unknown"

#What types of data are in each col? 
str(dogs)

#Hmmm... some of our numbers are registering as characters. 
#need to fix this if we want to plot them as quantities

### The columns that should be converted are:
#lifetime_cost
#intelligence_rank
#longevity
#price
#food_cost
#weight
#height
#ailments (# of genetic issues)

#Some of these are easy, though the missings will become NA by coercion:
dogs$intelligence_rank = dogs$intelligence_rank %>% as.numeric
dogs$ailments = dogs$ailments %>% as.numeric
dogs$height = dogs$height %>% as.numeric
dogs$weight = dogs$weight %>% as.numeric
dogs$longevity = dogs$longevity %>% as.numeric

#Some cols need to have the `$` and `,` stripped away before converting. 
#We can use our text analysis skills and the gsub function for this:

#remove commas or $ signs!
dogs$lifetime_cost = gsub("(\\$)|,", "", dogs$lifetime_cost)
dogs$food_cost = gsub("(\\$)|,", "", dogs$food_cost)
dogs$price = gsub("(\\$)|,", "", dogs$price)

#now convert to numeric!
dogs$lifetime_cost = dogs$lifetime_cost %>% as.numeric()
dogs$food_cost = dogs$food_cost %>% as.numeric()
dogs$price = dogs$price %>% as.numeric()


#These columns should be converted to factors:
#size
#group
#grooming (frequency: 1 = daily, 2 = weekly/biweekly, - = unknown)
#kids (suitability for kids: 1 = high, 2 = medium, 3 = low, - = unknown)
dogs$group = dogs$group %>% as.factor()
dogs$grooming = dogs$grooming %>% as.factor()
dogs$kids = dogs$kids %>% as.factor()
dogs$size = dogs$size %>% as.factor()

#Finally let's refactor them to be more transparent:
levels(dogs$grooming) = c("daily", "weekly", "monthly", "unknown")
levels(dogs$kids) = c("high", "medium", "low", "unknown")
  


### 2. Exploratory Data Analysis
#
# Q:
#   How does height vary for different groups of dogs?
#
# What plots can we use to see this?
#   * box plots
#   * violin plots
#   * density plots


library(ggplot2)
ggplot(dogs, aes(x = group, y = height)) + 
  geom_boxplot()

#Let's take out the unknowns - there's nothing useful there:
dogs_known = dogs %>% subset(group != "unknown")

#Let's also factor reorder by height & add fill colors!
ggplot(dogs_known, 
       aes(x = fct_reorder(group, height, .na_rm = TRUE), 
           y = height, 
           fill = group)) + 
  geom_boxplot() + 
  xlab("group")

# We could also add violin plots on top, which show the
# density sideways along the box plot.
ggplot(dogs_known, 
       aes(x = fct_reorder(group, height, .na_rm = TRUE), 
           y = height, 
           fill = group)) + 
  geom_boxplot() + 
  geom_violin() + 
  xlab("Group")


# If we use a density plot, how can we display the groups?
ggplot(dogs_known, 
       aes(color = group, 
           x = height)) + 
  geom_density()

# Cool! But maybe too many lines... can be hard to read.
# We can use a ridge plot using ggridges to show many densities at once.

install.packages("ggridges")
library(ggridges)

ggplot(dogs_known, 
       aes(x = height, 
           y = group, 
           fill = group)) + 
  geom_density_ridges() +
  coord_cartesian(clip = "off")


### Facets
#
# Can we plot the same info with side-by-side plots?
#
# Two useful "faceting" functions:
#   * facet_wrap()
#   * facet_grid()

ggplot(dogs_known, 
       aes(x = height)) + 
  geom_density() +
  facet_wrap( ~ group)

# Set scales = "free" to allow scales to vary. 

ggplot(dogs_known, 
       aes(x = height)) + 
  geom_density() +
  facet_wrap(~ group, scales = "free")

#Exercise: 
# Talk to your neighbor about the following 2 questions:
## 1. Why might we want to set scales to "free"? 
## 2. Why would we not want to do this?
# To get inspiration, try plotting different cols in the 
# dogs data with scales = "free" or not!

# Facets also work with ridge plots.
ggplot(dogs_known, 
       aes(x = height, y = 1)) + #note the y = 1 here - replace it by group and see what happens! 
  geom_density_ridges() +
  facet_wrap(~ group)

# Tuning parameter alert!! Use the `bw` parameter to change smoothness of densities.


# The facet_grid() function gives more control over facets.
#
# Syntax Idea:
# ROWS ~ COLUMNS
#

#size in diff rows
ggplot(dogs_known, 
       aes(x = height)) + 
  geom_density() +
  facet_grid(size ~ .)

#size in diff cols
ggplot(dogs_known, 
       aes(height)) + 
  geom_density() +
  facet_grid(. ~ size)

#grooming on rows, size on cols
ggplot(dogs_known, 
       aes(height)) + 
  geom_density() +
  facet_grid(grooming ~ size)

#make it small->med->large by reordering factors!
ggplot(dogs_known, 
       aes(height)) + 
  geom_density() +
  facet_grid(grooming ~ fct_reorder(size, height))

# When to use facets vs. aesthetics?
#   * facet: When aesthetics put tons of info on the plot
#   (too many lines, too many points, etc) - info density overload.
#
#   * aes: When there is less information to show.
#   Facets tend to use way more space than aesthetics.
#
# There is no rule that always holds when deciding 
# whether to use facets or aesthetics (or both).
# Think about:
# 1. what you want to convey,
# 2. who your audience is, and 
# 3. how they will best understand the message!

# For example, this is a case where it's not so
# necessary to use facets...
# They don't help us see more info than just using an aes!
ggplot(dogs_known, aes(size, fill = grooming)) + 
  geom_bar() + 
  facet_wrap(~ grooming)

# Q:  What to do with two continuous variables?
#
#     * Scatterplot
#     * Correlation

ggplot(dogs_known, 
       aes(height, weight)) + 
  geom_point()

#or 

library(GGally)

vars = c("height","weight")
ggpairs(dogs_known[,vars], 
        aes(alpha = 0.4))


#Q: What about three continuous variables??

# Ex: Height, Weight, Lifetime Cost
# How can we add lifetime cost to the scatterplot??
#
# We can't use facets bc lifetime_cost is continuous. 
# Instead we can use aesthetics that work with continuous variables.

ggplot(dogs_known, aes(height, 
                 weight, 
                 color = lifetime_cost)) + 
  geom_point()

# Warning! Small differences in color are hard to judge, 
# just like small differences in arc length (e.g. pie charts).
#
# We can double up on aesthetics to make the plot easier to read. 
# Let's use size.

ggplot(dogs_known, aes(height, 
                 weight, 
                 color = lifetime_cost, 
                 size = lifetime_cost)) +
  geom_point()

#btw you can turn the legends off for each aes with guides()
#and change the aes names in the legend with labs()
ggplot(dogs_known, aes(height, 
                 weight, 
                 color = lifetime_cost, 
                 size = lifetime_cost)) +
  geom_point() + 
  guides(size = FALSE) + 
  labs(color = "Lifetime Cost")

#We can also change the scale of colors to make it easier to discern differences:
ggplot(dogs_known, aes(height, 
                       weight, 
                       color = lifetime_cost, 
                       size = lifetime_cost)) +
  geom_point() + 
  guides(size = FALSE) + 
  labs(color = "Lifetime Cost") + 
  scale_color_gradient(low="blue", high="red")
  #scale_color_gradientn(colours = rainbow(5))


# Notice the points overlap -- this is called overplotting. 
# One way to fix this is to make points transparent.
#
# `alpha` aesthetic = transparency.

ggplot(dogs_known, aes(height, 
                 weight, 
                 color = lifetime_cost, 
                 size = lifetime_cost)) +
  geom_point(alpha = 0.5) + 
  guides(size = FALSE) + 
  labs(color = "Lifetime Cost") +   
  scale_color_gradient(low="blue", high="red")


# Sometimes it's also possible to make a plot clearer 
# by changing variable/aesthetic pairs.
#
# For instance, let's swap weight and lifetime_cost:

ggplot(dogs_known, aes(height, 
                 lifetime_cost, 
                 color = weight, 
                 size = weight)) +
  geom_point(alpha = 0.5)

# Downside: the plot emphasizes the relationship between
# height and lifetime_cost (which is weak), rather than the
# more salient relationship between height and weight.
#
# Takeaway: When you make your plot, think about what you're trying to
# show and if that matches what the reader will see!


# Accessibility and Graphic Design
#
# Always: Keep the reader in mind when you design your plots!!
#
# Questions to consider:
#   * Will colorblind people be able to read the plot?
#   * Will the plot be legible if printed in black and white?
#
# Put yourself in another's shoes:
# ColorOracle: http://www.colororacle.org/
#
# Colorblind safe palettes:
#   * ColorBrewer #scale_color_brewer(), scale_fill_brewer()
#   * Viridis #scale_color_viridis_ and scale_fill_viridis_



## Thanks to Nick Ulle at UC Davis for sharing the dogs dataset and idea for notes. 
