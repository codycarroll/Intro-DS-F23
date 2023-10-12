my_df1 = data.frame(
  City = c("Austin", "Georgia", "Vancouver"), 
  Fancy = c(35000, 43000, 106000), 
  Normal = c(30000, 44000, 77000)
)

my_df1


library(tidyr)
library(dplyr)
library(scales)

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

set.seed(1979)

(my_df2 = data.frame(
  uniqueId = 1:4,
  treatment = sample(rep(c('iOS', 'Android'), each = 2)),
  work_am = runif(4, 0, 1),
  home_am = runif(4, 0, 1),
  work_pm = runif(4, 1, 2),
  home_pm = runif(4, 1, 2)
))


 
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
  stat_summary(fun.y = mean, geom = "line", size = 1) +
  stat_summary(fun.y = mean, geom = "point", size = 3) +
  labs(x = "\n Time of Day", y = "Time on Mobile Device \n") +
  theme(legend.title = element_blank(), 
        axis.title = element_text(size = 20,face = "italic"), 
        axis.text = element_text(size = 20, face = "italic"),
        legend.text = element_text(size = 14,face = "italic"),
        legend.position = "top") + 
  scale_colour_brewer(palette = "Set1", 
                      labels=c("Andriod @ Home",
                               "Android @ Work",
                               "iOS @ Home",
                               "iOS @ Work"))

#Exercise:
#Dissect the code for the plot line by line. 
#For each line, say what that layer is doing to the plot.




#reverse transformations:
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
demo_df <- data.frame(num = c(1,2,3,4,5), 
                      color = c("blue", "yellow", "yellow", "blue", "blue"))

# select a column from a data frame 
demo_df %>% select(color)
demo_df %>% select(num)
demo_df %>% select(c(num,color))

### using flights.csv data
flights = read.csv("~/Desktop/repos/Intro-DS-F23/Data/flights.csv")
head(flights)

#select demo
flights %>% select(c(dep, arr))

flights %>% select(arr_delay:dep_delay) #colon is start:stop

flights %>% select(ends_with("delay"))

flights %>% select(contains("dep_delay"))

#arrange demo
head(flights)
?arrange

demo_df %>% arrange(color)
demo_df %>% arrange(-num)

flights %>% arrange(dist)
flights %>% arrange(dest)

#mutate demo (review)
demo_df %>% mutate(squared = num^2)

flights = flights %>% mutate(dep_hr = dep %>% 
                     str_pad(width = 4, pad = "0") %>% 
                     substr(start = 1, stop = 2))

flights = flights %>% mutate(dep_min = dep %>% 
                     str_pad(width = 4, pad = "0") %>% 
                     substr(start = 3, stop = 4))

flights = flights %>% mutate(arr_hr = arr %>% 
                     str_pad(width = 4, pad = "0") %>% 
                     substr(start = 1, stop = 2))

flights = flights %>% mutate(arr_min = arr %>% 
                     str_pad(width = 4, pad = "0") %>% 
                     substr(start = 3, stop = 4))

#Exercise: 
#Calculate expected duration of each flight using mutate. 


### review of group_by() and summarise()
demo_df %>% 
  group_by(color) %>% 
  mutate(avgnum = mean(num))


#Exercise:
#Calculate average expected departure delay grouped by airport. 
# Which airport has the longest delays on avg? Which has the shortest?
#Calculate average expected departure delay grouped by carrier.
# Which carrier has the longest delays on avg? Which has the shortest?
