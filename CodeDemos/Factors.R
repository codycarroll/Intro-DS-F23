##Factors
library(tidyverse)
install.packages("forcats")
library(forcats) #for categorical data

##Creating a factor vector for months of the year
data = c("Jan", "Dec", "Apr", "Sep") 
#the data we'd like to make a factor

# there are 2 small issues with raw vectors for this kind of data
#1. there could be typos
#ex c("Jam", "Dec", "Apr")
#2. doesn't sort in a useful way
sort(data)

#Instead let's create a "Factor" which will help us with both:

#a vector of all possible levels that data can take on
month_levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                 "Jul","Aug", "Sep", "Oct", "Nov", "Dec")

#create the factor variable
mon_factor = factor(data, levels = month_levels)

#the above will treat the levels as defined by month_levels

#the following will just take unique values from the original vector
mon_factor_short = factor(data)


#making a factor from data that is not included in the possible levels
#will create an NA
data2 = c("Jam", "Dec", "Apr", "Maz")
mon_factor_NA = factor(data2, levels = month_levels)

mon_factor_no_NAs = factor(data2) #creates misspelled levels

##throw a warning whenever there is an unrecognized level in the data
parse_factor(data2, levels = month_levels)

##Automatically, factor() will order levels in alphabetical order if no
#levels are specified. If you'd like to order the levels according
#to when they first appear, set the levels to be the unique() values
#in the data.

mon_factor_short = factor(data) #alphabetical order
mon_factor_app = factor(data, levels = unique(data))

#if you want look at the levels of a factor, use levels() command
levels(mon_factor_app)

#overriding the current order of levels
levels(mon_factor_app) = c("Dec", "Jan", "Sep", "Apr")
sort(mon_factor_app)

##Playing around with a data set
dataset = gss_cat
?gss_cat
str(dataset)

#get an overall count of each level in a factor
#let's do this for the race factor in the dataset
count(dataset, race)

count(dataset, denom)

#alternatively use pipes:
dataset %>% 
  count(race)

#can sort by counts:
dataset %>% 
  count(race) %>% 
  arrange(desc(n))

dataset %>% 
  count(denom) %>% 
  arrange(desc(n))

### Intro to Visualization

#bar plot of race
ggplot(gss_cat, aes(race)) +
  geom_bar()

#relative frequency
ggplot(gss_cat, aes(race)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  ylab("relative frequency")


#creating a summary of religion according to tvhours
relig_summary = gss_cat %>%
  group_by(relig) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

#plot with factors not ordered
ggplot(relig_summary, aes(tvhours, relig)) + 
  geom_point()



#Re-ordering a factor according to another variable

#plot with re-ordered factors in religion
ggplot(relig_summary, aes(tvhours, fct_reorder(relig, tvhours))) +
  geom_point()

#in general it's better to do the reordering/other transformations outside 
# of the aes ... example below:


##re-naming levels using the fct_recode() function
#this is an important part of nice visualization 

#example with partyid
count(dataset, partyid)

#re-label these party ids to be a little more clear
mutate1 = mutate(dataset, partyid = fct_recode(partyid,
                              "Republican, strong"    = "Strong republican",
                              "Republican, weak"      = "Not str republican",
                              "Independent, near rep" = "Ind,near rep",
                              "Independent, near dem" = "Ind,near dem",
                              "Democrat, weak"        = "Not str democrat",
                              "Democrat, strong"      = "Strong democrat"
  ))

count(mutate1, partyid)


#collapsing a lot of levels to be more simple label names
#here, we collapse multiple levels into the levels "other", 
#"rep", "ind", and "dem"
newpartyid = fct_collapse(dataset$partyid,
                       other = c("No answer", 
                                 "Don't know", 
                                 "Other party"),
                       rep = c("Strong republican", 
                               "Not str republican"),
                       ind = c("Ind,near rep", 
                               "Independent", 
                               "Ind,near dem"),
                       dem = c("Not str democrat", 
                               "Strong democrat"))

dataset$party_summary = newpartyid



###revisiting religious refactoring by tv hours using mutate

relig_summary %>%
  mutate(relig = fct_reorder(relig, tvhours)) %>%
  ggplot(aes(tvhours, relig)) +
  geom_point()


#a similar plot focusing on income sorted by age:

rincome_summary <- gss_cat %>%
  group_by(rincome) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

##Income Range ordered by Age
ggplot(rincome_summary, aes(age, fct_reorder(rincome, age))) + 
  geom_point()

### Exercise
#1. Make the same plot with mutate and pipes instead 
# of transforming in the aes.

#2. Is sorting income ranges by age a good idea? Why or why not?

#3. What is the default order of the income range level?

#4. If we want to preserve income range level order, but move NA to the end,
 #. what would our plot look like? (Hint: you can use fct_relevel.)


rincome_summary %>% 
  mutate(rincome = fct_relevel(rincome, "Not applicable")) %>% 
  ggplot(aes(age, rincome)) +
  geom_point()



### Extra features if time:

by_age <- gss_cat %>%
  filter(!is.na(age)) %>%
  count(age, marital) %>%
  group_by(age) %>%
  mutate(prop = n / sum(n))

ggplot(by_age, aes(age, prop, colour = marital)) +
  geom_line(na.rm = TRUE)

ggplot(by_age, aes(age, prop, colour = fct_reorder2(marital, age, prop))) +
  geom_line() +
  labs(colour = "marital")



