
######################################################################################
# Project: POLS 095
# Purpose: Recoding variables, conducting descriptive statistics and bivariate tests
# Author: Greg Wolf
# Edit date: April 19, 2022
# Data:  anes.dta
# Packages Used: tidyverse, haven, tableone, gmodels
######################################################################################

library(tidyverse)
library(haven)
library(gmodels)
library(tableone)


# set working directory 
setwd("~/Library/CloudStorage/OneDrive-DrakeUniversity/Documents/R/095") 

# read in anes data
anes <- read_dta("anes.dta")


#### Step 1: create a smaller data frame of only the variables we need ####
anes.small <- anes %>%
  select(V202109x, V201508, V201200, V202306, V201507x, V202143, V202435)
rm(anes) # removes the larger anes data frame from our computer's memory

#### Step 2: Clean/recode the variables ####

anes.small.cleaned <- anes.small %>% # clean our anes.small subset of data
  # turnout (nominal)
  mutate(turnout = as.factor(V202109x)) %>%        # create turnout variable as a factor
  mutate(turnout = na_if(x = turnout, y = -2)) %>% # change values of -2 to NA
  mutate(turnout = recode_factor(.x = turnout,     # new line, recode 0 to 'No' and 1 to 'Yes'
                                 '0'= "No",
                                 '1' = "Yes")) %>%
  # marital.status (nominal)
  mutate(marital.status = as.factor(V201508)) %>%
  mutate(marital.status = na_if(x = marital.status, y = -9)) %>% # value of -9 = NA
  mutate(marital.status = na_if(x = marital.status, y = -8)) %>% # value of -8 = NA
  mutate(marital.status = na_if(x = marital.status, y = 3)) %>%  # value of 3 = NA
  mutate(marital.status = na_if(x = marital.status, y = 4)) %>%  # value of 4 = NA
  mutate(marital.status = na_if(x = marital.status, y = 5)) %>%  # value of 5 = NA
  mutate(marital.status = recode_factor(.x = marital.status,
                                        '1' = "Married",     # value of 1 = Married
                                        '2' = "Married",     # value of 2 = Married
                                        '6' = "Single")) %>% # value of 6 = Single
  # ideology (ordinal)
  mutate(ideology = as.ordered(V201200)) %>%              # create variable "ideology" that is an ordered factor version of V201200
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
                                  '99' = NA_character_)) %>% # value of 99 = NA (missing) 
  #mutate(ideology = ordered(ideology)) %>%               # ensures factor is ordered
  # trust  
  mutate(trust.experts = ifelse(V202306< 0, NA, V202306)) %>%  # a different way to makes values less than zero NA
  mutate(trust.experts = as.ordered(trust.experts)) %>%
  mutate(trust.experts = recode_factor(.x = trust.experts,                 # notice the reordering on lines 65-67
                                       '1' = "Trust ordinary people more", # value of 1 = "Trust ordinary people more"
                                       '3' = "Trust both about the same",  # value of 3 = "Trust both about the same"
                                       '2' = "Trust experts more")) %>%    # value of 2 = "Trust experts more"
  #mutate(trust.experts = ordered(trust.experts)) %>%     # ensures factor is ordered
  # age
  mutate(age = as.numeric(V201507x)) %>%    # create age variable as a numeric
  mutate(age = na_if(x = age, y = -9)) %>%  # change -9 to NA
  # Biden feeling thermometer
  mutate(biden.ft = as.numeric(V202143)) %>%
  mutate(biden.ft = replace(biden.ft, biden.ft<0, NA)) %>%
  # Biden affect
  mutate(biden.affect = as.numeric(V202435)) %>%  # create biden.affect variable as a numeric
  mutate(biden.affect = ifelse(biden.affect < 0, NA, biden.affect)) %>% # make all values less than 0 = NA
  droplevels() %>%    # drop all variable labels that are no longer used
  select(turnout, marital.status, ideology, trust.experts, age, biden.ft, biden.affect) # keep only our new, mutated variables

# check class types for variables
class(anes.small.cleaned$turnout)
class(anes.small.cleaned$marital.status)
class(anes.small.cleaned$ideology)
class(anes.small.cleaned$trust.experts)
class(anes.small.cleaned$age)
class(anes.small.cleaned$biden.ft)
class(anes.small.cleaned$biden.affect)


#### Step 3: Create a table of descriptive statistics ####
library(tableone)
CreateTableOne(data = anes.small.cleaned) 

# let's add some variable labels in place of variable names
# add variable labels to print in table
labelled::var_label(x = anes.small.cleaned) <- c("Turnout",
                                         "Marital Status",
                                         "Ideology",
                                         "Trust in Experts",
                                         "Age in Years",
                                         "Biden Feeling Thermometer",
                                         "Biden Affect")

# check data frame for labels
str(object = anes.small.cleaned)
# create a basic table as an object
anes.table <- CreateTableOne(data = anes.small.cleaned)

# use print to show table with labels
print(x = anes.table, varLabels = TRUE)

# use print to show table with labels and percent instead of number of observations
print(x = anes.table,
      varLabels = TRUE,
      format = "p",
      explain = FALSE)

#### Step 4: Bivariate tests ####

### Chi-Square ###

# create cross table and chi-square for two categorical variables

#  turnout and marital.status
CrossTable(anes.small.cleaned$turnout, anes.small.cleaned$marital.status, chisq=T)

# marital status and ideology
CrossTable(anes.small.cleaned$marital.status, anes.small.cleaned$ideology, chisq=T)

# turnout and trust.experts
CrossTable(anes.small.cleaned$turnout, anes.small.cleaned$trust.experts, chisq=T)

### Difference of means tests ###

# Difference of means test (marital.status and biden.affect)

diff.test1 <- t.test(biden.affect ~ marital.status, data = anes.small.cleaned, var.eqaul = T)
diff.test1

# Difference of means box plot (marital.status and biden.affect)
diff.test1.fig1 <- anes.small.cleaned %>%
  drop_na(marital.status) %>%
  ggplot(aes(x = marital.status, y = biden.affect)) +
  geom_boxplot() +
  scale_x_discrete(labels = c("Married", "Single")) +
  theme_minimal() +
  xlab("Marital Status") +
  ylab("Biden Affect")
diff.test1.fig1

# Difference of means test (turnout and age)

diff.test2 <- t.test(age ~ turnout, data = anes.small.cleaned, var.eqaul = T)
diff.test2

# Difference of means box plot (turnout and age)
diff.test2.fig2 <- anes.small.cleaned %>%
  drop_na(turnout) %>%
  ggplot(aes(x = turnout, y = age)) +
  geom_boxplot() +
  scale_x_discrete(labels = c("No", "Yes")) +
  theme_minimal() +
  xlab("Turnout") +
  ylab("Age")
diff.test2.fig2

### Correlation ###

# correlation between age and biden affect
cor(x = anes.small.cleaned$age, y = anes.small.cleaned$biden.affect, use = "complete.obs")

# correlation between biden affect and biden feeling thermometer
cor(x = anes.small.cleaned$biden.affect, y = anes.small.cleaned$biden.ft, use = "complete.obs")

# scatter plot between two continuous variables
scatter.fig2 <- anes.small.cleaned %>%
  ggplot(aes(x = biden.affect, y = biden.ft)) +
  geom_point(position = "jitter") +
  geom_jitter(width = 5, height = 5) +
  theme_minimal() +
  xlab("Biden Affect") +
  ylab("Biden Feeling Thermometer")
scatter.fig2 # this is ugly!


### Regression ###

# dv: turnout, iv: age
# model <- lm(dv ~ iv + iv2, data = data.frame)
model.1 <- lm(as.integer(trust.experts) ~ biden.affect 
              + as.integer(ideology) + marital.status, 
              data = anes.small.cleaned)
summary(model.1)

hjhjkhjkh






















mod1 <- lm(biden.ft ~ marital.status, 
           data = anes.small.cleaned, na.action = na.exclude)
summary(mod1)

mod.a <- lm(biden.ft ~ biden.affect, data = anes.small.cleaned, na.action = "na.omit")
summary(mod.a)
library(broom)
tidy(mod.a)
glance(mod.a)

mod2 <- lm(biden.ft ~ marital.status + biden.affect, 
           data = anes.small.cleaned, na.action = na.exclude)
summary(mod2)

mod3 <- lm(biden.ft ~ marital.status + biden.affect + as.numeric(ideology) + age,
           data = anes.small.cleaned, na.action = na.exclude)
summary(mod3)

mod4 <- lm(as.numeric(trust.experts) ~ turnout,
           data = anes.small.cleaned, na.action = na.exclude)
summary(mod4)

mod5 <- lm(as.numeric(trust.experts) ~ turnout + age + as.numeric(ideology),
           data = anes.small.cleaned, na.action = na.exclude)
summary(mod5)

### END



model.a <- lm(ideology <- geography, data = anes.small.cleaned)
summary(model.a)
