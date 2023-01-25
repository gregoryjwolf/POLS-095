
###########################################
# Project: POLS 095
# Purpose: Introduction to R, Part 2
# Author: Greg Wolf
# Edit date: February 17, 2022
# Data:  2020 ANES
# Packages Used: tidyverse, haven, Hmisc                                                   
###########################################

### Step 0: clear all objects from R ###
# You don't necessarily need to do this, but if 
# your environment is cluttered and using a lot
# of memory, it can be useful
rm(list = ls())

#### Step 1: set the working directory ###

setwd("~/Library/CloudStorage/OneDrive-DrakeUniversity/Documents/R/095") 
##YOU NEED TO CHANGE THIS LINE (I use a Mac, so there is no "C:/...")
# see note below to set this via point and click

# for Windows:
# setwd("C:/User/Greg/Documents/R/095")

# note: it's easiest if you go to:
# Session -> Set Working Directory -> Choose Working Directory
# then copy the code that R spits out in the console 
# and save it to your R script

### Step 2: load libraries ###

# R uses "packages" to implement various functions
# You will need to install packages and then load
# them using the "library" function

# install the "haven" package. this package allows you 
# to easily read in data from other sources
install.packages(pkgs = "haven")

# install "tidyverse" package. tidyverse is a collection
# of packages used for "data science". useful for many actions
install.packages(pkgs = "tidyverse")

# install "Hmisc" package. this package is useful
# for summarizing manipulating data
install.packages(pkgs = "Hmisc")

# load the tidyverse, haven, and Hmisc packages
library(package = "tidyverse")
library(haven)
library(Hmisc)

### Step 3: read in the data ###
# (R will read in files stored in your Working Directory)
# the "read_dta" function will read .dta files;
# .dta files are Stata format (different file types
# will have different functions (e.g., .csv is read_csv))

# function: dataset.name <- read_dta("file.dta")
anes <- read_dta("anes.dta")

# Examine the contents of the anes file
# the "summary(object = ?)" function will show us that object's contents
summary(object = anes)
# note the terrible variable names. You will need to
# use the ANES codebook (on Blackboard) to make sense of things.

# let's find the variable for age. search the codebook.
# it's easiest to use command+f or ctrl+f and search "age"

### Step 4: Renaming variables: Make a new variable called
# 'age' that is V201507x, the variable for age in the anes dataset

# we can create a new variable in the anes dataset by:

# function: dataset$new.variable <- dataset$old.variable
anes$age <- anes$V201507x

# notice that this doesn't create a new object called "age"
# it creates a variable called "age" within "anes"

### Step 4: summarize the age variable ###

# now let's summarize the age variable, since we 
# might be interested what is in the age variable

#!# tidyverse piping #!#

# we'll use the "tidyverse" style, which implements the
# pipe operator %>%. The %>% (pipe) extends the code to the line below.
# Here, it means that everything after the line will be done
# to the anes data set. Piping allows us to write blocks of code
# instead of individual lines. It also keeps us from having to
# use dataset$variable over and over.

anes %>%
  summarize(length.age = length(x = age))
# the length function will tell us how many lines/respondents
# are in our dataset for the age variable

# Check for conflicts with functions in loaded R packages
# where we have the same function in two packages, which
# can make it so R doesn't know which one we want to use
conflicts()

# Use summarize from dplyr package to find how many
# lines of age there are
anes %>%
  dplyr::summarize(length.age = length(x = age))

# Check source package for summarize function
# this will tell us which package R is using 
# for the 'summarize()' function
environment(fun = summarize)
# well we don't want R to use Hmisc for summarize,
# so let's get rid of it

# Detach Hmisc package (we don't need it anymore)
detach("package:Hmisc", unload = TRUE)

# try summarize
anes %>%
  summarize(length.age = length(x = age))
head(x = anes) # show the first six rows of the 
# dataset. rows correspond with respondents

# what if we use summarize for age and not length?
anes %>%
  summarize(age) # shows us the age of the first ten rows/respondents
# still not the best. What if we want the average age?

anes %>%
  summarize(mean.age = mean(x = age))

# so the summarize function will work for a 
# variety of different types of summaries, but we
# have to specify what type of summary we want. the
# general function is:
# data %>%
#    summarize(type.variable = type(x = variable))
# where type is the statistic we're using to summarize

# now let's try to do this for turnout



# Make a new variable that if the respondent has voted (V202109x)

anes$turnout <- anes$V202109x

summary(anes$turnout)

# The variable is numeric. turnout isn't intrinsically 
# numeric, it needs to be a category. Thus it should
# be (for this exercise) a factor variable

# change turnout variable to a factor
anes$turnout <- as.factor(anes$turnout)
summary(anes$turnout) # shows the number of respondents in each category
# the categories -2, 0, and 1 correspond to the ANES codebook

# let's do the same for 'age'
summary(anes$age)
# The -9 value is the code for missing.
# R is using that value of -9 when
# computing the average age (and other stats)
# that's not good, we'll come back to this in a bit

# We can check the type of variable directly
class(anes$age)
class(anes$turnout)

# so what we have now are two variables that 
# aren't names or coded in very useful ways.

# We've done a couple of discrete steps to make them
# more to our liking. It would be easier (tidier)
# to do them all together. That is what tidyverse is for.

### Step 5: use tidyverse to manipulate data ###

# Start over by reading in the data (will overwrite prior changes)
anes <- read_dta("anes.dta")

# let's create a "clean" version of the ane
# dataset that has our changes and leave the
# original alone

# we also know that our anes data has missing values
# for some respondents on age and turnout. the codebook
# told us that -2 is the missing code for turnout
# and -9 for age. that's a problem for us, so we want to 
# change them from numeric values to 'real' missing values
# R will capture missingness with 'NA'. we can use the 
# "na_if" function to recode values to NA.

anes.cleaned <- anes %>%                            # creates new dataset named 'anes.cleaned'
  mutate(turnout = as.factor(V202109x)) %>%         # new variable 'turnout' that is a factor version of V202109x
  mutate(turnout = na_if(x = turnout, y = -2)) %>%  # changes the -2 missing code to NA in 'turnout'
  mutate(age = V201507x) %>%                        # new variable 'age' that is the same as V201507x   
  mutate(age = na_if(x = age, y = -9))              # changes -9 missing code to NA

summary(anes.cleaned$age)      # summarizes the age variable in the anes.cleaned dataset
summary(anes.cleaned$turnout)  # summarizes the turnout variable in the anes.cleaned dataset

# That turnout variable still thinks that the 
# missing values are levels of the factor.

# to make changes to our anes.cleaned dataset, 
# we want to build on the code we've already written.
# so we can copy and paste the above and add in 
# some additional lines to further manipulate the data

anes.cleaned <- anes %>%
  mutate(turnout = as.factor(V202109x)) %>%
  mutate(turnout = na_if(x= turnout, y = -2)) %>%
  mutate(turnout = recode_factor(.x = turnout,        # new line, recodes 0 to 'No' and 1 to 'Yes'
                                 '0'= "No",
                                 '1' = "Yes")) %>%
  mutate(age = V201507x)%>%
  mutate(age = na_if(x = age, y = -9)) %>%
  mutate(turnout = droplevels(x = turnout))          # 'droplevels' will delete values no longer in use (-2 in this case)
summary(anes.cleaned$turnout)


### Step 6: create a figure ###

# The goal is to visualize how turnout varies by age.

# Start with a plot of the turnout question

# let's create a new object that will be our figure 
# and call it 'turnout.bar'. we want to use the 
# anes.cleaned dataset. ggplot is our basic graphics
# utility. ggplot uses '+' instead of %>% to 
# continue to the next line

turnout.bar <- anes.cleaned %>%  # created new object turnout.bar using anes.cleaned dataset
  ggplot(aes(x = turnout)) +     # use ggplot, specify turnout as the variable to graph
  geom_bar()                     # geom_bar is the type of figure (a bar chart)
turnout.bar                      # calls the object 'turnout.bar' and shows the figure

# That NA category is worthless. let's get rid of it
turnout.bar <- anes.cleaned %>%
  drop_na(turnout) %>%           # make changes to the data in the figure before moving to ggplot, pipe down
  ggplot(aes(x = turnout)) +
  geom_bar()
turnout.bar

# let's add some color, it's 2022!
turnout.bar <- anes.cleaned %>%
  drop_na(turnout) %>%
  ggplot(aes(x = turnout, fill = turnout)) +     # we'll use 'fill = turnout' because we want to fill turnout with color
  geom_bar() +
  scale_fill_manual(values = c("#78A678", "#7463AC"),  # we have two categories, so we need to specify two colors with c("color.one", "color.two")
                    guide = FALSE)               # guide = FALSE means no legend
turnout.bar

# You can play around with the colors
turnout.bar <- anes.cleaned %>%
  drop_na(turnout) %>%
  ggplot(aes(x = turnout, fill = turnout)) +
  geom_bar() +
  scale_fill_manual(values = c("green", "blue"),
                    guide = FALSE)
turnout.bar

turnout.bar <- anes.cleaned %>%
  drop_na(turnout) %>%
  ggplot(aes(x = turnout, fill = turnout)) +
  geom_bar() +
  scale_fill_manual(values = c("#4363BD", "#F1BE48"),
                    guide = FALSE)
turnout.bar

# let's add some labels to our X and Y axes
turnout.bar <- anes.cleaned %>%
  drop_na(turnout) %>%
  ggplot(aes(x = turnout, fill= turnout)) +
  geom_bar() +
  scale_fill_manual(values = c("#4363BD", "#F1BE48"),
                    guide = FALSE)+
  theme_minimal() +                    # cleans up the figure (gets rid of gray background)
  labs(x = "Did Respondent vote?",
       y = "Number of responses")
turnout.bar

# Percent makes more sense than counts
turnout.bar <- anes.cleaned %>%
  drop_na(turnout) %>%
  ggplot(aes(x = turnout, 
             y = 100 * (..count..) / sum(..count..),     # changes the plot of each value of x to no/(yes+no) and yes/(yes+no), and scale of Y axis to percentage
             fill = turnout)) +
  geom_bar() +
  scale_fill_manual(values = c("#4363BD", "#F1BE48"),
                    guide = FALSE) +
  theme_minimal() +
  labs(x = "Did Respondent vote?",
       y = "Percent of responses")
turnout.bar

# Percent makes more sense, but let's recode categories for 'turnout' to 'voted' and 'did not vote' instead of 'yes' and 'no'
turnout.bar <- anes.cleaned %>%
  drop_na(turnout) %>%
  mutate(turnout = recode_factor(.x = turnout,
                                 `Yes` = "Voted",             # categories of 'turnout were 'Yes' and 'No'; change to 'Voted' and 'Did not vote'
                                 `No` = "Did not vote")) %>%
  ggplot(aes(x = turnout, 
             y = 100 * (..count..) / sum(..count..),
             fill = turnout)) +
  geom_bar() +
  scale_fill_manual(values = c("#4363BD", "#F1BE48"),
                    guide = FALSE) +
  theme_minimal() +
  labs(x = "Did Respondent vote?",
       y = "Percent of responses")
turnout.bar

# what about turnout by age? that's a relationship we're aware of
# Separate by age. Change the X in ggplot and the geometry of the bars, geom_bar()
# A little complicated, but we're going to fill 'age' with 'turnout', that
# way we can get a breakdown of turnout within our age categories (think 
# about it as 'turnout' in 'age')

turnout.bar <- anes.cleaned %>%
  drop_na(turnout) %>%
  mutate(turnout = recode_factor(.x = turnout,
                                 `Yes` = "Voted",
                                 `No` = "Did not vote")) %>%
  ggplot(aes(x = age, 
             y = 100 * (..count..) / sum(..count..),
             fill = turnout)) +                   # fills our ages with turnout percentages (yields turnout % by age)
  geom_bar(position = 'dodge') +                  # no more single bars for each category of turnout; need them the 'voted' and 'did not vote' categories to "dodge" each other by age 
  scale_fill_manual(values = c("#4363BD", "#F1BE48"),
                    guide = FALSE) +
  theme_minimal() +
  labs(x = "Did Respondent vote?",
       y = "Percent of responses")
turnout.bar
# oof. that doesn't look good!


# That age variable isn't very useful. Make it 
# a (simplified) category instead. We need to create
# a new variable, 'age.cat'.
# Let's create four categories for values of age: 
# (1) < 30, (2) 30-59, (3) 60-74, (4) 75+
turnout.bar <- anes.cleaned %>%
  drop_na(turnout) %>%
  mutate(turnout = recode_factor(.x = turnout,
                                 `Yes` = "Voted",
                                 `No` = "Did not vote")) %>%
  mutate(age.cat = cut(x = age,
                       breaks = c(-Inf, 29, 59, 74, Inf),   # creates cutpoints/breaks that go from -infinity to 29, 30 to 59, 60 to 74, and 75 to +infinity
                       labels = c("< 30", "30 - 59", "60 - 74", "75+" ))) %>% # labels our new categories
  ggplot(aes(x = age.cat, 
             y = 100 * (..count..) / sum(..count..),
             fill = turnout)) +
  geom_bar(position = 'dodge') +  
  scale_fill_manual(values = c("#4363BD", "#F1BE48"),
                    guide = FALSE) +
  theme_minimal() +
  labs(x = "Did Respondent vote?",
       y = "Percent of responses")
turnout.bar

# That produces the percent of all respondents in both 
# groups (age and vote).  We want the % in each age that
# voted and remove the NA from the age category
turnout.bar <- anes.cleaned %>%
  drop_na(turnout) %>%
  mutate(turnout = recode_factor(.x = turnout,
                                 `Yes` = "Voted",
                                 `No` = "Did not vote")) %>%
  mutate(age.cat = cut(x = age,
                       breaks = c(-Inf, 29, 59, 74, Inf),
                       labels = c("< 30", "30 - 59", "60 - 74", "75+" ))) %>%
  drop_na(age.cat) %>%
  group_by(turnout, age.cat) %>%
  count() %>%
  group_by(age.cat) %>%
  mutate(perc.turnout = 100*n/sum(n)) %>%    # creates a new variable to capture turnout percent that we can use in each age category
  ggplot(aes(x = age.cat, fill = turnout,    # fill each X axes value (age.cat) with 'turnout'
             y = perc.turnout)) +            # ensure that the Y axes comports with percent for each age category, not % overall
  geom_col(position = 'dodge') +
  scale_fill_manual(values = c("#4363BD", "#F1BE48"),
                    guide = FALSE) +
  theme_minimal() +
  labs(x = "Did Respondent vote?",
       y = "Percent of responses")
turnout.bar

# We did it! 

# Can you summarize, in words, the relationship that
# the final bar graph produces?

# What this script demonstrates is how to go from
# reading data into R, creating new variables,
# changing the coding of new variables, and
# producing figures that visualize an interesting 
# relationship in our data.

# you don't need to have every function memorized.
# we'll build and tweak our code, so that we're 
# rarely starting from zero.