
###########################################
# Project: POLS 095
# Purpose: Introduction to R, Part 1
# Author: Greg Wolf
# Edit date: February 11, 2022
# Data: None
# Packages Used: None
###########################################

#### PART 1: OBSERVATIONS AND VARIABLES ####

# Use R to do math
2+2
(4+6)/2
(72/9)+3
10^2
a <- 3 # creates an object called "a" that has a constant value of 3
a


## EXERCISE: create an object called "myname" that has a constant value of 48
# and add 7 to it
greg <- 48
greg
greg + 7

#### USING REPRODUCIBLE RESEARCH PRACTICES ####
# using naming conventions and code that is easily replicable

# create an object with the number of states with legal medical marijuana in 2017
states <- 29
# print the value of the states object
states
# determine how many states there would be if 2 more passed this policy
2 + states
states <- states + 2
states

## NAMING OBJECTS ##

# NAMING CONSTANTS
# use a k and then camel case to format the object name for constants
# make a new object with well-formatted name
kStates <- 29
# assign the existing states object a new name
kStates <- states
# remove the states object
rm(states)
rm(a, greg)

## NAMING VARIABLES ##

# use dot case or camel case (camel case is also called ''Pascal case")
# examples:
# dot case: filled.script.month, blood.pressure
# camel case: filledScriptMonth, bloodPressure

## NAMING FUNCTIONS ##

# use upper camel case
# example: multiplyByTwo

# Exercise
kIllegalNum <- 21 # create an object called kIllegalNum that has a value of 21 (which is a constant)
kIllegalNum - 2 # kIllegalNum minus 2

#### PART 2: UNDERSTANDING AND CHANGING DATA TYPES ####
class(x = variable)
## TYPES OF DATA ##

# NUMERIC DATA TYPE
# identify data type for states object
# the class() function tells us the data type
class(x = kStates)
class(kStates)  # without "x ="

# numeric class is best for continuous variables
# assign Rhode Island limit for medical marijuana in ounces per person
kOuncesRhode <- 2.5
# identify the data type for kOuncesRhode
class(x = kOuncesRhode)

## INTEGER DATA TYPE
# whole numbers only
# to change the type to integer use as.integer()
# as.integer() can also truncate numbers with decimal places (does NOT round)
# assign the value of 4 to a constant called kTestInteger
# make sure it is an integer
kTestInteger <- as.integer(x = 4)
# use class() to determine the data type of kTestInteger
class(x = kTestInteger)
kTestInteger <- as.numeric(kTestInteger)
class(x = kTestInteger)
# use as.integer() to truncate the constant kOuncesRhode
kOuncesRhodeInteger <- as.integer(x = kOuncesRhode)
# multiply the kTestInteger and kOuncesRhode constants
kTestInteger * kOuncesRhode
# multiple kTestIneger and interger kOuncesRhode constants
kTestInteger * as.integer(x = kOuncesRhode)
# type the object name to see what is currently saved in the object
kOuncesRhode

## LOGICAL DATA TYPE 
# contains values of TRUE and FALSE
# create the constant
kTestLogical <- TRUE
# print the value of constant
kTestLogical
# check the constant data type
class(x = kTestLogical)
# store the result of 6 > 8 in a constant called kSixEight
kSixEight <- 6 > 8
# print kSixEight
kSixEight
# determine the data type of kSixEight
class(x = kSixEight)

## CHARACTER DATA TYPE
# characters contain letters, words, or numbers that cannot be logically included
# in calculations (e.g., a zip code). Almost always wrapped in quotation marks.
# make constants
kFirstName <- "Corina"
kLastName <- "Hughes"
# check the data type
class(x = kFirstName)
# create a zip code constant
kZipCode <- "50311"
# check the data type
class(x = kZipCode)

## EXERCISE: check the datatype for kIllegalNum
class(x = kIllegalNum)

#### PART 3: ENTERING DATA INTO R ####

# data are stored in an object called a vector
# a vector is a set of data elements saved as the same type (numeric, logical, etc.)

# creates character vector char.vector
char.vector <- c('Oregon', 'Vermont', 'Maine')
# prints vector char.vector
char.vector
# creates numeric vector nums.1.to.4
nums.1.to.4 <- c(1,2,3,4)
# prints vector nums.1.to.4
nums.1.to.4 
# creates logical vector logic.vector
logic.vector <- c(TRUE, FALSE, FALSE, TRUE)
# prints vector logic.vector
logic.vector

# create and print vectors at the same time (for efficiency)
( char.vector <- c('Iowa', 'Minnesota', 'Illinois') )
( nums.1.to.4 <- c(1,2,3,4) )
( logic.vector <- c(TRUE, FALSE, FALSE, TRUE) )


## do some math to nums.1.to.4 vector
# reminder of what vector nums.1.to.4 is:
nums.1.to.4
# add 3 to each element in the nums.1.to.4 vector
nums.1.to.4 + 3
# add 1 to the first element of nums.1.to.4, 2 to the second element, etc.
nums.1.to.4 + c(1, 2, 3, 4)
# multiple each element of nums.1.to.4 by 5
nums.1.to.4 * 5
# subtract 1 from each element and then divide by 5
(nums.1.to.4 - 1)/5
# make a subset of the vector including numbers > 2
nums.1.to.4[nums.1.to.4 > 2]
# add 3 to number vector and save
( nums.1.to.4.plus.3 <- nums.1.to.4 + 3 )
# divide vector by 10 and save as new vector
( nums.1.to.4.div.10 <- nums.1.to.4 / 10 )
# add 3 and divide by 10 for each vector member
( nums.1.to.4.new <- (nums.1.to.4 + 3)/10 )

## Create a matrix to store data in rows and columns
# create and print a matrix
( policies <- matrix(data = c(1, 2, 3, 4, 5, 6), 
                     nrow = 2,
                     ncol = 3,
                     byrow = TRUE) )
# name the columns and rows of a matrix (row = year, col = type of policy)
# add names to the rows and columns of the matrix
dimnames(x = policies) <- list(
  c("2013", "2014"),
  c("medical", "recreational", "both")
)
# print the policies matrix
policies
# vector of policy types
policy.2013.and.2014 <- c('medical', 'medical', 'both', 'recreational', 
                          'medical', 'both')

# data type
class(x = policy.2013.and.2014)
# change the data type from character to factor (b/c it's a categorical variable)
policy.2013.and.2014 <- as.factor(x = policy.2013.and.2014)
# check the data type
class(x = policy.2013.and.2014)
# creating a data frame
# combine vectors to create a a data frame with variables
# state, year enacted, personal oz limit medical marijuana
# create vectors (variables)
state <- c('Alaska', 'Arizona', 'Arkansas', 'California', 'Colorado')
year.legal <- c('1998', '2010', '2016', '1996', '2000')
ounce.lim <- c(1, 2.5, 3, 8, 2)
# combine vectors into a data frame
# name the data frame pot.legal
pot.legal <- data.frame(state, year.legal, ounce.lim)
class(pot.legal$state)
# change state variable from pot.legal data frame to a factor variable
pot.legal$state <- as.factor(x = pot.legal$state)
# check the variable type
class(x = pot.legal$state)
# we prefer factor over character data type for categorical variables
# summarize the data frame
summary(object = pot.legal)
# check the class type of year.legal
class(x = pot.legal$year.legal)
pot.legal$year.legal <- as.factor(x = pot.legal$year.legal)
summary(object = pot.legal)
pot.legal$year.legal <- as.integer(x = pot.legal$year.legal)
class(pot.legal$year.legal)

# remove ounce.lim, state, year.legal individual objects from
# the R environment (they're redundant with the pot.legal data frame) 
rm(ounce.lim, state, year.legal)

# Clear all objects from the R environment:
rm(list = ls())