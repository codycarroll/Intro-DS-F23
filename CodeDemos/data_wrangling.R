my_df1 = data.frame(
  City = c("Austin", "Atlanta", "Vancouver"), 
  Fancy = c(35000, 43000, 106000), 
  Normal = c(30000, 44000, 77000)
)

my_df1

#difficulty w wide format:
ggplot(my_df1, aes(x = City, y = Fancy, fill = Fancy)) + 
  geom_bar(stat = "identity")


library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)
library(nlme)



?pivot_longer


my_tidy_df1 = pivot_longer(my_df1, 
                           cols = c(Fancy, Normal),
                           names_to = "lightSign", 
                           values_to = "Sales")
my_tidy_df1


ggplot(my_tidy_df1, 
       aes(x = City, 
           y = Sales, 
           group = lightSign, 
           fill = lightSign)) + 
  geom_bar(stat = "identity", 
           position = "dodge") +
  scale_y_continuous(labels = comma) + 
  ylab("Sales") + 
  xlab("\n City") + 
  theme(legend.title = element_blank()) 

#stat = "identity" means plot just the values of Sales without anything changed
#position = "dodge" means put bars side-by-side instead of stacked
#theme(legend.title = element_blank()) removes legend title

#Exercise:
#Experiment with removing arguments like position, legend.title, stat.
#What changes for each?


set.seed(1979)

(my_df2 = data.frame(
  uniqueId = 1:4,
  treatment = sample(rep(c('iOS', 'Android'), each = 2)),
  work_am = runif(4, 0, 1),
  home_am = runif(4, 0, 1),
  work_pm = runif(4, 1, 2),
  home_pm = runif(4, 1, 2)
))

my_tidy_df1 = pivot_longer(my_df1, 
                           cols = c(Fancy, Normal),
                           names_to = "lightSign", 
                           values_to = "Sales")
my_tidy_df1

my_tidy_df2 = pivot_longer(my_df2, 
                           cols = c(work_am, home_am, work_pm, home_pm),
                           names_to = "location_time_of_day", 
                           values_to = "time_spent")


my_tidy_df2 = pivot_longer(my_df2, 
                           cols = c(work_am, home_am, work_pm, home_pm), 
                           names_to = "sample",
                           values_to = "time")

# my_tidy_df2 = gather(my_df2, sample, time, -uniqueId, -treatment)
# my_tidy_df2 %>% arrange(uniqueId)

head(my_tidy_df2)

my_sep_tidy_df2 = separate(my_tidy_df2, 
                           sample, 
                           into = c("location", "time_of_day"), 
                           sep = "\\_")

head(my_sep_tidy_df2)


my_sep_tidy_df2$time_of_day = toupper(my_sep_tidy_df2$time_of_day)

head(my_sep_tidy_df2, 3)

#intense code but pretty simple plot
ggplot(my_sep_tidy_df2, aes(x = time_of_day, y = time)) + 
  aes(group = interaction(location, treatment), 
      colour = interaction(location, treatment)) +
  stat_summary(fun.y = max, geom = "line", size = 1) +
  stat_summary(fun.y = identity, geom = "point", size = 3) +
  labs(x = "\n Time of Day", y = "Time on Mobile Device \n") +
  theme(legend.title = element_blank(),
        axis.title = element_text(size = 20,face = "italic"),
        axis.text = element_text(size = 20, face = "italic"),
        legend.text = element_text(size = 14,face = "italic"),
        legend.position = "top") +
  scale_colour_brewer(palette = "Set1", 
                      labels=c("Android @ Home",
                               "Android @ Work",
                               "iOS @ Home",
                               "iOS @ Work"))

#Exercise:
#Dissect the code for the plot line by line. 
#For each line, say what that layer is doing to the plot.


#Exercise: 
#1. Install the package "nlme". 
#2. Look at the description of the dataset `Oxboys`
#3. Is the data in wide or long format? Is the data tidy?
#4. Add 1 to the "age" column. 
#5. Plot the height trajectories of the boys, separated by subject.
# Draw a point for each observation and connect observations 
# within the same subject with a line. Remove the legend using 
# theme() and the `legend.position` argument.
# x = age, y = height

head(Oxboys)

#btw this is called a spaghetti plot
ggplot(Oxboys, aes(x = age + 1, 
                   y = height, 
                   group = Subject, 
                   colour = Subject)) + 
  geom_line() + 
  geom_point() + 
  theme(legend.position = "none")

#or
ggplot(Oxboys, aes(x = age + 1, 
                   y = height , 
                   color = Subject)) + 
  stat_summary(fun.y = identity, geom = "point") + 
  stat_summary(fun.y = identity, geom = "line") + 
  theme(legend.position = "none")
#NOTE:
#longitudinal data (time-varying measurements) need to be in long format for ggplot to visualize



#reverse transformations: long to wide!
my_tidy_df1


(pivot_wider(my_tidy_df1, 
             names_from = lightSign, 
             values_from = Sales))

#Exercise:
# 1. Pivot my_tidy_df2 back into wide format. 
# 2. Figure out how to get my_sep_tidy_df2 back to original form of my_df2. 

#-----------------------

###Review on Pipes!

# Without magrittr

my_tidy_df1 = pivot_longer(my_df1, 
                           cols = c(Fancy, Normal),
                           names_to = "lightSign", 
                           values_to = "Sales")

ggplot(my_tidy_df1,
       aes(x = City, 
           y = Sales, 
           group = lightSign, 
           fill = lightSign)) +
  geom_bar(stat = "identity", 
           position = "dodge") + 
  labs(fill="Sign Type") 

#-----------------------
# With magrittr
library(magrittr)

my_df1 %>%
  pivot_longer(cols = c(Fancy, Normal),
               names_to = "lightSign", 
               values_to = "Sales") %>%
  ggplot(aes(x = City, 
             y = Sales, 
             group = lightSign, 
             fill = lightSign)) +
  geom_bar(stat = "identity", 
           position = "dodge") + 
  labs(fill="Sign Type") 


### dplyr functions!!
#super useful 

#toy df 
demo_df = data.frame(num = c(1,2,3,4,5), 
                      color = c("blue", "yellow", "yellow", "blue", "blue"))

# select a column from a data frame 
demo_df %>% select(color)
demo_df %>% select(num)
demo_df %>% select(c(num, color))

### using flights.csv data
flights = read.csv("~/Desktop/repos/Intro-DS-F23/Data/flights.csv")
head(flights)

#select demo
flights %>% select(c(dep, arr)) %>% head

flights %>% select(dep_delay:plane) %>% head#colon is start:stop

flights %>% select(ends_with("delay")) %>% head

flights %>% select(contains("dep")) %>% head

#arrange demo
head(flights)
?arrange

demo_df %>% arrange(color)
demo_df %>% arrange(-num)

flights %>% arrange(dist)
flights %>% arrange(dest)

#mutate demo (review)
demo_df %>% mutate(squared = num^2)

library(stringr)
flights$dep %>% 
  str_pad(width = 4, pad = "0") %>% 
  substr(start = 1, stop = 2)

flights = flights %>% mutate(dep_hr = dep %>% 
                               str_pad(width = 4, pad = "0") %>% 
                               substr(start = 1, stop = 2), 
                             dep_min = dep %>% 
                               str_pad(width = 4, pad = "0") %>% 
                               substr(start = 3, stop = 4))

flights = flights %>% mutate(arr_hr = arr %>% 
                               str_pad(width = 4, pad = "0") %>% 
                               substr(start = 1, stop = 2), 
                             arr_min = arr %>% 
                               str_pad(width = 4, pad = "0") %>% 
                               substr(start = 3, stop = 4))
flights %>% head

#Exercise: 
#Calculate expected duration of each flight using mutate. 

for(i in 15:18){
  flights[,i] = flights[,i] %>% as.numeric
}

flights %>% str

hour_diff = flights$arr_hr - flights$dep_hr
min_diff = flights$arr_min - flights$dep_min
negative_min = (min_diff < 0)

flights = flights %>% mutate(duration = str_c(((hour_diff %>% mod(24)) - negative_min), " hours and ", min_diff %>% mod(60), " minutes."))
head(flights)
#1 hr 10 min

hour_diff[3]
min_diff[3]


### review of group_by() and summarise()
demo_df %>% 
  group_by(color) %>% 
  mutate(avgnum = mean(num))


#Exercise:
#Calculate average expected departure delay grouped by airport. 
# Which airport has the longest delays on avg? Which has the shortest?
#Calculate average expected departure delay grouped by carrier.
# Which carrier has the longest delays on avg? Which has the shortest?
