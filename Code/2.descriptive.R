
###########################################
# Project: POLS 095
# Purpose: Descriptive Statistics
# Author: Greg Wolf
# Edit date: March 2, 2022
# Data:  mar1.xlsx, anes.dta
# Packages Used: tidyverse, haven, tableone, readxl
###########################################

# this R script has examples to find summary
# statistics, specifically the mean, median, mode,
# variance, standard deviation, and range.

#step one:  set the working directory
setwd("~/Library/CloudStorage/OneDrive-DrakeUniversity/Documents/R/095")

library(package = "tidyverse")

#step two: read in the data
library(readxl) # the 'readxl' package allows you
# to open Excel files
mar1 <- read_excel("mar1.xlsx") # read in the file, call it 'mar1'

#step two: view the data set
view(mar1)

#step three: check variable types
# we use the 'class' function to check data types
# class(dataset$variable)
class(mar1$height)
class(mar1$starburst)
class(mar1$hubbell)

# R tells us the 'starburst' and 'hubbell' are
# both character data types. we want them to 
# be factors because they're categorical variables

#step four: change variable types
mar1_cleaned <- mar1 %>%                   # create 'cleaned' dataset
  mutate(starburst = as.factor(starburst)) %>%
  mutate(hubbell = as.factor(hubbell))

# check the class again to make sure it worked
class(mar1_cleaned$starburst)
class(mar1_cleaned$hubbell)

# okay, now we're good? for now we are!

#step five: descriptive statistics

# let's start with the starburst variable
# there are many ways to summarize a categorical variable

# the 'table' command
table(mar1_cleaned$starburst)
# the 'summary' command
summary(mar1_cleaned$starburst)
# both of the above commands return what we
# call 'frequencies,' or counts of each response

# what if we want to know percentages? 
# we can use the tidyverse for that
mar1_cleaned %>%
  group_by(starburst) %>%
  summarize(freq.starburst = n()) %>%
  mutate(perc.starburst = 100 * (freq.starburst / sum(freq.starburst)))

# now let's try the height variable
table(mar1_cleaned$height)

# ok, cool. but what's the typical height of the class?
# we need to calculate the mean, median, and mode!

# mean
mean(x = mar1_cleaned$height) # 67.5 
#median
median(x = mar1_cleaned$height)    # 67
#mode: there is no 'mode' function in R :(
names(x = sort(x = table(mar1_cleaned$height), decreasing = TRUE))[1]  # 65

# before we move on to the hubbell variable, let's try
# another numeric variable. let's create a vector of salaries

# create 'salaries' vector and find its mean
salaries <- c(25000, 62000, 41000, 96000, 41000)
salaries
mean(x = salaries)

# add Bill Gates
salaries.gates <- c(salaries, 11500000000)
salaries.gates
# find the mean of the vector with gates
mean(x = salaries.gates)
# median salary without Bill Gates
median(x = salaries)
# median salary with Bill Gates
median(x = salaries.gates)

# table showing salaries frequencies
table(salaries)

# table showing salaries frequencies in order most to least
sort(x = table(salaries), decreasing = TRUE)

# mean, median, and mode of salaries (remember no 'mode' function)
mean(x = salaries)
median(x = salaries)
names(x = sort(x = table(salaries), decreasing = TRUE))[1]

# we're done with our salary vectors, let's remove them
rm(salaries, salaries.gates)

# now let's go back to our mar1 dataset and check hubbell
# create a table of frequenceis for hubbell
table(mar1_cleaned$hubbell)
# okay, we get a table of frequencies, but
# those responses aren't in the right order, 
# and this is an ordinal variable
# we must fix this!


# let's pipe in our original data frame
# and remember to change the data types for
# starburst and hubbell. then we'll need to 
# mutate hubbell so the responses are ordered
mar1_cleaned <- mar1 %>%
  mutate(starburst = as.factor(starburst)) %>%
  mutate(hubbell = as.factor(hubbell)) %>%
  mutate(hubbell = recode_factor(.x = hubbell,
                                'very good' = "very good",
                                'good' = "good",
                                'okay' = "okay",
                                'bad' = "bad",
                                'very bad'  = "very bad"))
table(mar1_cleaned$hubbell)
# awesome, it worked!

# let's say we want to combine the 'very good' and 'good'
# categories and 'very bad' and 'bad' categories for simplicity
mar1_cleaned <- mar1 %>%
  mutate(starburst = as.factor(starburst)) %>%
  mutate(hubbell = as.factor(hubbell)) %>%
  mutate(hubbell = recode_factor(.x = hubbell,
                                'very good' = "Good",
                                'good' = "Good",
                                'okay' = "Okay",
                                'bad' = "Bad",
                                'very bad'  = "Bad"))
table(mar1_cleaned$hubbell)
# now we have three categories instead of five

# we can't find the mean value of an ordinal variable
# that doesn't make sense, there are no numbers
# but what if we want to know the median category?

# we can use the median function if we specify 
# our ordinal variable as a numeric instead of 
# a factor. so we'll use the 'median' command but
# tell R to treat 'hubbell' as a 'numeric' data type
median(as.numeric(mar1_cleaned$hubbell))
# it returns '3'. that means the median category is 
# the third category. what's the third category?
table(mar1_cleaned$hubbell)
# the third category is 'bad', so our median 
# category is 'bad'

# it also can be useful to see the distributions of our data
# let's create a bar chart (bar charts are for categorical
# data, histograms are for numeric data; they're similar)

#step six: bar charts and histograms
starburst.bar <- mar1_cleaned %>%  # created new object starburst.bar using mar1_cleaned dataset
  ggplot(aes(x = starburst)) +     # use ggplot, specify turnout as the variable to graph
  geom_bar()                     # geom_bar is the type of figure (a bar chart)
starburst.bar                      # calls the object 'starburst.bar' and shows the figure

# okay cool, but that looks boring, let's add color
starburst.bar <- mar1_cleaned %>%  
  ggplot(aes(x = starburst, fill = starburst)) +     
  geom_bar() +
  scale_fill_manual(values = c("orange", "pink", # we'll use 'fill = starburst' because we want to fill starbust with color
                               "red", "yellow"),
                    guide = FALSE)   # guide = FALSE means no legend
starburst.bar                      

starburst.bar <- mar1_cleaned %>%  
  ggplot(aes(x = starburst, fill = starburst)) +     
  geom_bar() +
  scale_fill_manual(values = c("orange", "pink",
                               "red", "yellow"),
                    guide = FALSE) +
  theme_minimal() +
  labs(x = "Starburst Flavor", 
       y = "Number of Responses")
starburst.bar

# Percent makes more sense than counts
starburst.bar <- mar1_cleaned %>%
  ggplot(aes(x = starburst, 
             y = 100 * (..count..) / sum(..count..),     # changes the plot of each value of the variable out of the total
             fill = starburst)) +
  geom_bar() +
  scale_fill_manual(values = c("orange", "pink",
                               "red", "yellow"),
                    guide = FALSE) +
  theme_minimal() +
  labs(x = "Starbust Flavor", 
       y = "Percent of Responses")
starburst.bar

# what if we want to capitalize our color labels?
# that's a function of the variable, so we need 
# to mutate the data
starburst.bar <- mar1_cleaned %>%
  mutate(starburst = recode_factor(.x = starburst,
                                   'orange' = "Orange",
                                   'pink' = "Pink",
                                   'red' = "Red",
                                   'yellow' = "Yellow")) %>%
  ggplot(aes(x = starburst, 
             y = 100 * (..count..) / sum(..count..),     # changes the plot of each value of x to no/(yes+no) and yes/(yes+no), and scale of Y axis to percentage
             fill = starburst)) +
  geom_bar() +
  scale_fill_manual(values = c("orange", "pink",
                               "red", "yellow"),
                    guide = FALSE) +
  theme_minimal() +
  labs(x = "Starbust Flavor", 
       y = "Percent of Responses")
starburst.bar

# We can create a similar bar chart for 'hubbell'
# let's use colorhexa.com to choose our colors
hubbell.bar <- mar1_cleaned %>%
  ggplot(aes(x = hubbell,
             y = 100 * (..count..) / sum(..count..),
             fill = hubbell)) + 
  geom_bar() +
  scale_fill_manual(values = c("#4dcc9d", "#ffbf00", "#ff748c"), guide = FALSE) + 
  theme_minimal() +
  labs(x = "Opinion about hubbell Food",
       y = "Percent of Responses")
hubbell.bar

# now let's try a histogram of 'height'
height.hist <- mar1_cleaned %>%
  ggplot(aes(x = height, fill = height)) +
  geom_histogram() +
  theme_minimal() +
  labs(x = "Height (Inches)",
       y = "Frequency")
height.hist

# each bar in a histogram is called a 'bin'
# we can redue the number of bins to make it look better

height.hist <- mar1_cleaned %>%
  ggplot(aes(x = height, fill = height)) +
  geom_histogram(bins = 6, fill = "#7463AC", color = "gray") + 
  theme_minimal() +
  labs(x = "Height (Inches)",
       y = "Frequency")
height.hist

# let's create a table that has information for
# all three variables. we'll use a package 
# called 'tableone'

# install and open tableone package
install.packages("tableone")
library(package = "tableone")

# create a basic table
CreateTableOne(data = mar1_cleaned)

# check the labels for the data frame
str(object = mar1_cleaned)

# add variable labels to print in table
labelled::var_label(x = mar1_cleaned) <- c("Height",
                                         "Favorite Starburst Flavor",
                                         "Hubbell Food Quality")
# check data frame for labels
str(object = mar1_cleaned)

# create a basic table as an object
mar1.table <- CreateTableOne(data = mar1_cleaned)

# use print to show table with labels
print(x = mar1.table, varLabels = TRUE)

# use print to show table with labels and percent
print(x = mar1.table,
      varLabels = TRUE,
      format = "p",
      explain = FALSE)

###############################################
# Now let's try to do all this with ANES data #
###############################################


# select variables for analysis
# use ` around variable names starting with underscores (not necessary here)

# we can create a smaller data set of the variables we need
# using the select() function

# ANES variables
# V201435 = religious denomination
# V201617x = income
# V201510 = education level
# V201507x = age 
# V201200 = ideology
# V201600 = sex

anes_2<-anes%>%
  select(V201435, V201617x, V201510,
         V201507x, V201200, V201600) 

# examine the data
summary(object = anes_2)

# look at a table of the distribution of the sex variable
table(anes_2$V201600)

# check data type for sex variable
class(anes_2$V201600)
# doesn't look good. needs to be a factor
# since it's a nominal categorical variable

# change variable to factor and name appropriately
anes_2_cleaned <- anes_2 %>%
  mutate(sex = as.factor(V201600))

# check data type again
class(anes_2_cleaned$sex)

# cleaning the sex variable so values match
# the codebook categories instead of numbers
anes_2_cleaned <- anes_2 %>%
  mutate(sex = as.factor(V201600))%>%
  mutate(sex = recode_factor(.x= sex,
                             '-9' = "Refused",
                             '1' = "Male",
                             '2' = "Female"))

##There are lots of ways to get the distribution of the sex variable.
##you only need to do one.

# table of sex  frequencies
table(anes_2_cleaned$sex)

# use summary for frequencies
summary(anes_2_cleaned$sex)

# use tidyverse to make table of frequency and percent
anes_2_cleaned %>%
  group_by(sex) %>%
  summarize(freq.sex = n()) %>%
  mutate(perc.sex = 100 * (freq.sex / sum(freq.sex)))


# table with frequencies from the age variable
table(anes_2_cleaned$V201507x)
# whoa! that's ugly!

# find mean, median, and mode of age ('V201507x')
mean(anes_2_cleaned$V201507x)
median(anes_2_cleaned$V201507x)
names(x = sort(x = table(anes_2_cleaned$V201507x), decreasing = TRUE))[1]
# tells us the mode is 80. there are problems with this
# since everyone 80 and older is coded '80'. We also have a 
# value of -9, which isn't an age someone can be. We'll deal
# with this issue a little later


# pipe in the original data frame
# recode the sex factor so it's easy to read
# for a anes_2_cleaned data set
# recode -9 and -5  on the income variable
# -9 and -5 are what we call "missing values"
# we want R to basically delete them and treat
# then as missing. we use the na_if() function for that
anes_2_cleaned <- anes_2 %>%
  mutate(sex = as.factor(V201600))%>%
  mutate(sex = recode_factor(.x= sex,
                             '-9' = "Refused",
                             '1' = "Male",
                             '2' = "Female"))%>%
  mutate(income = V201617x)%>%
  mutate(income = na_if(x=income, y=-5))%>%
  mutate(income = na_if(x=income, y=-9))

# examine income to check data management
summary(anes_2_cleaned$income)


# make a histogram for income
anes_2_cleaned %>%
  ggplot(aes(x = income)) +
  geom_histogram()

# get mean, median, mode
mean(x = anes_2_cleaned$income)   
median(x = anes_2_cleaned$income)
names(x = sort(x = table(anes_2_cleaned$income), decreasing = TRUE))[1]

# get mean, median, mode
mean(x = anes_2_cleaned$income, na.rm = TRUE)
median(x = anes_2_cleaned$income, na.rm = TRUE)
names(x = sort(x = table(anes_2_cleaned$income), decreasing = TRUE))[1]
# ehhh, not great. no mean and we want to do it all at once

# get mean, median, mode for 'income'
# we'll specify that is should remove missing values
# with 'na.rm = TRUE'
anes_2_cleaned %>%
  summarize(mean.income = mean(x = income,
                               na.rm = TRUE),
            med.income = median(x = income,
                                na.rm = TRUE),
            mode.income = names(x = sort(table(income),
                                         decreasing = TRUE))[1])


# variance of income
var(x = anes_2_cleaned$income, na.rm = TRUE)
sd(x=anes_2_cleaned$income, na.rm = TRUE)

# get mean, median, mode, and spread
anes_2_cleaned %>%
  summarise(mean.income = mean(x = income,na.rm = TRUE),
            sd.income = sd(x=income, na.rm = TRUE),
            var.income = var(x=income, na.rm=TRUE),
            med.income = median(x = income,
                                na.rm = TRUE),
            mode.income = names(x = sort(table(income),
                                         decreasing = TRUE))[1])

# range of income
range(anes_2_cleaned$income, na.rm = TRUE)

# note that the -9 and -5 are still there.
anes_2_cleaned <- anes_2 %>%
  mutate(sex = as.factor(V201600))%>%
  mutate(sex = recode_factor(.x= sex,
                             '-9' = "Refused",
                             '1' = "Male",
                             '2' = "Female")) %>%
  mutate(income = as.numeric(V201617x)) %>%
  mutate(income = na_if(x=income, y=-5)) %>% # will treat -5 as NA
  mutate(income = na_if(x=income, y=-9))    # will treat -9 as NA

# get descriptive statistics for income
anes_2_cleaned %>%
  summarise(mean.income = mean(x = income,na.rm = TRUE),
            sd.income = sd(x=income, na.rm = TRUE),
            var.income = var(x=income, na.rm = TRUE),
            med.income = median(x = income,na.rm = TRUE),
            iqr.income = IQR(x=income, na.rm = TRUE),
            mode.income = names(x = sort(table(income),decreasing = TRUE))[1])

# get descriptive statistics for income
anes_2_cleaned %>%
  drop_na(income)%>%
  summarise(mean.income = mean(x = income),
            sd.income = sd(x=income),
            var.income = var(x=income),
            med.income = median(x = income),
            iqr.income = IQR(x=income),
            mode.income = names(x = sort(table(income),decreasing = TRUE))[1])

# make a histogram for age (V201507x)
anes_2_cleaned %>%
  ggplot(aes(x = V201507x)) +
  geom_histogram()

# we need to get rid of that -9 value
anes_2_cleaned %>%
  mutate(age = V201507x)%>%
  mutate(age = na_if(x=age,y = -9))%>%
  drop_na(age)%>%
  ggplot(aes(x = age)) +
  geom_histogram()


# examine the descriptive statistics for the age variable
anes_2_cleaned %>%
  mutate(age = V201507x)%>%
  mutate(age = na_if(x=age,y = -9))%>%
  drop_na(age)%>%
  summarise(mean.age = mean(x = age),
            sd.age = sd(x=age),
            var.age = var(x=age),
            med.age = median(x = age),
            iqr.age = IQR(x=age),
            mode.age = names(x = sort(table(age),decreasing = TRUE))[1])

# create a subset of the data set to keep
# female respondents who are  
# age between 30 and 45
anes.small <- anes_2_cleaned %>%
  mutate(age = V201507x)%>%
  mutate(age = na_if(x=age,y = -9))%>%
  filter(sex == 'female') %>%
  filter(age > 29 & age < 46)
# if we look into the top right quadrent, R 
# shows 0 observations in anes.small
# we messed something up!

##note the case sensitivity of 'sex' from earlier
anes.small <- anes_2_cleaned %>%
  mutate(age = V201507x)%>%
  mutate(age = na_if(x=age,y = -9))%>%
  filter(sex == 'Female') %>%
  filter(age > 29 & age < 46)

# check the new data frame
summary(object = anes.small)
# better

##note a couple of things:  first, lots of the variables still need to be recoded
##second, the sex variable still has labels for Refused and Male even though 
# neither should be in our dataset since we only kept Females

# add labels to factor variables
anes.small <- anes_2_cleaned %>%
  mutate(age = V201507x)%>%
  mutate(age = na_if(x = age,y = -9))%>%
  filter(sex == 'Female') %>%
  filter(age > 29 & age < 46)%>%
  mutate(V201435 = na_if(x = V201435, y = -9))%>%
  mutate(V201435 = na_if(x = V201435,y = -8))%>%
  mutate(religion = as.factor(V201435))%>%
  mutate(religion = recode_factor(.x = religion,
                                  '1' = "Protestant",
                                  '2' = "Roman Catholic",
                                  '3' = "Orthodox Christian",
                                  '4' = "Latter-Day Saint",
                                  '5' = "Jewish",
                                  '6' = "Muslim",
                                  '7' = "Buddhist",
                                  '8' = "Hindu",
                                  '9' = "Athiest",
                                  '10' = "Agnostic",
                                  '11' = "Something else",
                                  '12' = "Nothing in particular")) %>%
  mutate(V201510 = na_if(x=V201510, y=-8)) %>%
  mutate(V201510 = na_if(x=V201510, y=-9)) %>%
  mutate(education = as.factor(V201510)) %>%
  mutate(education = recode_factor(.x=education,
                                   '1' = "Less than high school",
                                   '2' = "High school graduate",
                                   '3' = "Some college but no degree",
                                   '4' = "Associate degree",
                                   '5' = "Associate degree", #Note that this combines categories 4 & 5
                                   '6' = "Bachelor's degree",
                                   '7' = "Graduate degree",
                                   '8' = "Graduate degree",
                                   '95' = "Other"))%>%
  mutate(V201200 = na_if(x = V201200, y = -8))%>%
  mutate(V201200 = na_if(x = V201200, y = 99))%>%
  mutate(ideology = as.factor(V201200))%>%
  mutate(ideology = recode_factor(.x = ideology,
                                  '1' = "Extremely liberal",
                                  '2' = "Liberal",
                                  '3' = "Slightly liberal", 
                                  '4' = "Moderate",
                                  '5' = "Slightly conservative",
                                  '6' = "Conservative",
                                  '7' = "Extremely conservative"))

#check the work so far
summary(object = anes.small)
# let's look just at the 'sex' variable
summary(object = anes.small$sex)

anes.small <- anes_2_cleaned %>%
  mutate(age = V201507x)%>%
  mutate(age = na_if(x=age,y = -9))%>%
  filter(sex == 'Female') %>%
  filter(age > 29 & age < 46)%>%
  mutate(V201435 =na_if(x=V201435,y=-9))%>%
  mutate(V201435 =na_if(x=V201435,y=-8))%>%
  mutate(religion = as.factor(V201435))%>%
  mutate(religion = recode_factor(.x=religion,
                                  '1' = "Protestant",
                                  '2' = "Roman Catholic",
                                  '3' = "Orthodox Christian",
                                  '4' = "Latter-Day Saint",
                                  '5' = "Jewish",
                                  '6' = "Muslim",
                                  '7' = "Buddhist",
                                  '8' = "Hindu",
                                  '9' = "Athiest",
                                  '10' = "Agnostic",
                                  '11' = "Something else",
                                  '12' = "Nothing in particular"))%>%
  mutate(V201510 = na_if(x=V201510, y=-8))%>%
  mutate(V201510 = na_if(x=V201510, y=-9))%>%
  mutate(education = as.factor(V201510))%>%
  mutate(education = recode_factor(.x=education,
                                   '1' = "Less than high school",
                                   '2' = "High school graduate",
                                   '3' = "Some college but no degree",
                                   '4' = "Associate degree",
                                   '5' = "Associate degree", #Note that this combines the categories
                                   '6' = "Bachelor's degree",
                                   '7' = "Graduate degree",
                                   '8' = "Graduate degree",
                                   '95' = "Other"))%>%
  mutate(V201200 = na_if(x=V201200, y=-9))%>%
  mutate(V201200 = na_if(x=V201200, y=-8))%>%
  mutate(V201200 = na_if(x=V201200, y=99))%>%
  mutate(ideology = as.factor(V201200))%>%
  mutate(ideology = recode_factor(.x=ideology,
                                  '1' = "Extremely liberal",
                                  '2' = "Liberal",
                                  '3' = "Slightly liberal", 
                                  '4' = "Moderate",
                                  '5' = "Slightly conservative",
                                  '6' = "Conservative",
                                  '7' = "Extremely conservative"))%>%
  droplevels()


#check the work so far
summary(object = anes.small$sex)
# the 'droplevels() function above gets rid of
# unused categories

# get percents for ideology
prop.table(x = table(anes.small$ideology))

# open tableone package
install.packages("tableone")
library(package = "tableone")

# create a basic table
CreateTableOne(data = anes.small)

#note that it has all of the original variables

anes.small <- anes_2_cleaned %>%
  mutate(age = V201507x)%>%
  mutate(age = na_if(x=age,y = -9))%>%
  filter(sex == 'Female') %>%
  filter(age > 29 & age < 46)%>%
  mutate(V201435 =na_if(x=V201435,y=-9))%>%
  mutate(V201435 =na_if(x=V201435,y=-8))%>%
  mutate(religion = as.factor(V201435))%>%
  mutate(religion = recode_factor(.x=religion,
                                  '1' = "Protestant",
                                  '2' = "Roman Catholic",
                                  '3' = "Orthodox Christian",
                                  '4' = "Latter-Day Saint",
                                  '5' = "Jewish",
                                  '6' = "Muslim",
                                  '7' = "Buddhist",
                                  '8' = "Hindu",
                                  '9' = "Athiest",
                                  '10' = "Agnostic",
                                  '11' = "Something else",
                                  '12' = "Nothing in particular"))%>%
  mutate(V201510 = na_if(x=V201510, y=-8))%>%
  mutate(V201510 = na_if(x=V201510, y=-9))%>%
  mutate(education = as.factor(V201510))%>%
  mutate(education = recode_factor(.x=education,
                                   '1' = "Less than high school",
                                   '2' = "High school graduate",
                                   '3' = "Some college but no degree",
                                   '4' = "Associate degree",
                                   '5' = "Associate degree", #Note that this combines the categories
                                   '6' = "Bachelor's degree",
                                   '7' = "Graduate degree",
                                   '8' = "Graduate degree",
                                   '95' = "Other"))%>%
  mutate(V201200 = na_if(x=V201200, y=-9))%>%
  mutate(V201200 = na_if(x=V201200, y=-8))%>%
  mutate(V201200 = na_if(x=V201200, y=99))%>%
  mutate(ideology = as.factor(V201200))%>%
  mutate(ideology = recode_factor(.x=ideology,
                                  '1' = "Extremely liberal",
                                  '2' = "Liberal",
                                  '3' = "Slightly liberal", 
                                  '4' = "Moderate",
                                  '5' = "Slightly conservative",
                                  '6' = "Conservative",
                                  '7' = "Extremely conservative"))%>%
  droplevels()%>%
  select(sex,income, age, religion, education, ideology) # keep only our new, mutated variables

CreateTableOne(data = anes.small)

# check the labels for the data frame
str(object = anes.small)

# add variable labels to print in table
labelled::var_label(x = anes.small) <- c("Sex (n = 1161)",
                                         "Income",
                                         "Age in years",
                                         "Faith tradition",
                                         "Education category",
                                         "Ideology")
# check data frame for labels
str(object = anes.small)

# create a basic table as an object
anes.table <- CreateTableOne(data = anes.small)

# use print to show table with labels
print(x = anes.table, varLabels = TRUE)

# use print to show table with labels and percent
print(x = anes.table,
      varLabels = TRUE,
      format = "p",
      explain = FALSE)