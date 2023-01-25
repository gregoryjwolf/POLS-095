
###########################################
# Project: POLS 095
# Purpose: Ideology Example (HW #1)
# Author: Greg Wolf
# Edit date: February 10, 2022
# Data: ANES
# Packages Used: tidyverse, haven
###########################################

# This example is similar to Question 2 in 
# homework #1. 

library(package = "tidyverse")
library(haven)

# R will look for the data file in your working directory,
# so make sure it's set correctly

anes <- read_dta("anes.dta")

# Renaming variables: Make a new variable 
# called "ideology" that is V201200
anes$ideology <- anes$V201200

# Let's see what the variable "ideology"
# looks like
anes$ideology

# Let's see what type of variable "ideology" is
class(anes$ideology)

# hmm, that looks weird. ANES labeled the data, and so
# R is reading a "double" vector of values and labels.
# This means R is treating 'ideology' with an interval 
# level of measurement instead of ordinal, which R should
# treat as a factor.
# R will default to read the first column as the data type
# In this case, that is integer (-9, -8, 1, 2, etc.)

# Let's summarize the ideology variable
summary(anes$ideology)
# OK we get something that isn't an error 
# message but it's not particularly useful
# Why?


# Let's to this in a TIDIER way. We need to
# "transform" the variable. Ideology is an 
# ordinal (categorical) variable, so we should
# recreate it as a factor.

# We'll create a new dataset called "anes.cleaned" 
# from the ANES data set. We're also going to 
# simplify the variable into three ordinal categories
# (liberal, moderate, conservative) instead of seven

anes.cleaned <- anes %>%    # anes.cleaned "gets" anes data
  mutate(ideology = as.factor(V201200))%>%               # create variable "ideology" that is a factor version of V201200
  mutate(ideology = recode_factor(.x = ideology,         # note the . before x, which you need when recoding factors
                                  '1' = "Liberal",       # value of 1 = liberal
                                  '2' = "Liberal",       # value of 2 = liberal
                                  '3' = "Liberal",       # value of 3 = liberal
                                  '4' = "Moderate",      # value of 4 = moderate
                                  '5' = "Conservative",  # value of 5 = conservative
                                  '6' = "Conservative",  # value of 6 = conservative
                                  '7' = "Conservative",  # value of 7 = conservative
                                  '-8' = NA_character_,  # value of -8 = NA (missing)
                                  '-9' = NA_character_,  # value of -9 = NA (missing)
                                  '99' = NA_character_)) # value of 99 = NA (missing)

table(anes.cleaned$ideology) # create a table of frequencies

# Create a a bar chart
# this implements ideas from 1.intro.p2.R
# we'll use ggplot to create a bar chart
# and use the geom_bar() to do that

# version 1
ideology.bar <- anes.cleaned %>%
  ggplot(aes(x = ideology)) + 
  geom_bar() +
  labs(x = "Ideology", y = "Number of participants")
ideology.bar

# version 2: drop missing values
ideology.bar <- anes.cleaned %>%
  drop_na(ideology) %>%
  ggplot(aes(x = ideology)) + 
  geom_bar() +
  labs(x = "Ideology", y = "Number of participants")
ideology.bar

# version 3: add color to the bars
ideology.bar <- anes.cleaned %>%
  drop_na(ideology) %>%
  ggplot(aes(x = ideology)) + 
  geom_bar(aes(fill = ideology)) +
  labs(x = "Ideology", y = "Number of participants")
ideology.bar

# version 4: provide unique colors for the bars
ideology.bar <- anes.cleaned %>%
  drop_na(ideology) %>%
  ggplot(aes(x = ideology)) + 
  geom_bar(aes(fill = ideology)) +
  labs(x = "Ideology", y = "Number of participants") +
  scale_fill_manual(values = c("#7463AC", "pink", "yellow"))
ideology.bar

# version 5: get rid of the legend
ideology.bar <- anes.cleaned %>%
  drop_na(ideology) %>%
  ggplot(aes(x = ideology)) + 
  geom_bar(aes(fill = ideology)) +
  labs(x = "Ideology", y = "Number of participants") +
  scale_fill_manual(values = c("#7463AC", "pink", "yellow"), guide = FALSE)
ideology.bar


