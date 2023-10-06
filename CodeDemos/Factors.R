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
library(readr)
parse_factor(data2, levels = month_levels)

##Automatically, factor() will order levels in alphabetical order if no
#levels are specified. If you'd like to order the levels according
#to when they first appear, set the levels to be the unique() values
#in the data.

mon_factor_short = factor(data) #alphabetical order
mon_factor_app = factor(data, levels = unique(data)) #order in dataset

#if you want look at the levels of a factor, use levels() command
levels(mon_factor_app)

#overriding the current order of levels
levels(mon_factor_app) = c( "Jan","Apr",  "Sep", "Dec")
sort(mon_factor_app)

##Playing around with a data set
dataset = gss_cat
?gss_cat
str(dataset)
View(dataset)

levels(dataset$rincome)

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
library(ggplot2)

?ggplot
#bar plot of race
ggplot(gss_cat, aes(race)) +
  geom_bar()

#relative frequency
ggplot(gss_cat, aes(race)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  ylab("relative frequency")

race_tbl = dataset %>% 
  count(race) %>% 
  arrange(desc(n))

race_tbl$pct = race_tbl$n/sum(race_tbl$n)


#creating a summary of religion according to tvhours
relig_summary = gss_cat %>%
  group_by(relig) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

#plot with factors not ordered
ggplot(relig_summary, aes(x = tvhours, y = relig)) + 
  geom_point()

#plot with factors not ordered
library(ggrepel)
ggplot(relig_summary, aes(x = tvhours, y = age)) + 
  geom_point() + 
  geom_label_repel(mapping = aes(label = relig))


ggplot(gss_cat, aes(x = tvhours, y = age)) + 
  geom_point()

#Re-ordering a factor according to another variable

#unordered
ggplot(relig_summary, aes(x = tvhours, y = relig)) + 
  geom_point()

#plot with re-ordered factors in religion
ggplot(relig_summary,
       aes(x = tvhours,
           y = fct_reorder(relig, tvhours))) +
  geom_point() + 
  ylab("Religious Affiliation")

ggplot(relig_summary, 
       aes(x = tvhours, 
           y = fct_reorder(relig, tvhours))) +
  geom_point() +
  ylab("Religious Affiliation")

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
View(dataset)

drop_idx = (colnames(dataset) == "partyid")
View(dataset[,!drop_idx])

#hw for me: drop columns by name
dataset %>% select(year, marital, age)


###revisiting religious refactoring by tv hours using mutate

#piping strategy
relig_summary %>%
  mutate(relig = fct_reorder(relig, tvhours)) %>%
  ggplot(aes(x = tvhours, y = relig)) +
  geom_point()

default_relig_summary = relig_summary

reordered_relig_summary = relig_summary %>%
  mutate(relig = fct_reorder(relig, tvhours))

levels(default_relig_summary$relig)
levels(reordered_relig_summary$relig)

#a similar plot focusing on income sorted by age:

rincome_summary <- gss_cat %>%
  group_by(rincome) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

##Income Range ordered by Age
ggplot(rincome_summary, 
       aes(x = age, 
           y = fct_reorder(rincome, age))) + 
  geom_point() + 
  ylab("Income Range")

### Exercise
#1. Make the same plot with mutate and pipes instead 
# of transforming in the aes.

mutated_rincome = rincome_summary %>% 
  mutate(rincome = fct_reorder(rincome, age))

mutated_rincome %>% 
  ggplot(aes(age, rincome)) +
  geom_point()

#2. Is sorting income ranges by age a good idea? Why or why not?

No bc this breaks the natural order of the income ranges.

#3. What is the default order of the income range level?

levels(rincome_summary$rincome)

#4. If we want to preserve income range level order, but move NA to the end,
 #. what would our plot look like? (Hint: you can use fct_relevel.)

rincome_summary %>% 
  ggplot(aes(age, rincome)) +
  geom_point()


?fct_relevel
# 
# f <- factor(c("a", "b", "c", "d"), levels = c("b", "c", "d", "a"))
# fct_relevel(f)
# fct_relevel(f, "a")
# fct_relevel(f, "b", "a")



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

View(by_age)


ggplot(by_age, 
       aes(x = age, y = prop, colour = marital)) +
  geom_line(na.rm = TRUE) + 
  ggtitle("Marital Status Composition by Age")

ggplot(by_age, 
       aes(age, prop, colour = fct_reorder2(marital, age, prop))) +
  geom_line() +
  labs(colour = "marital")




