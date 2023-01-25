#################################################################################
# Name:                                                           
# Date Created:                                                  
# Homework 5
# Packages Used: tidyverse, haven, gridExtra             
# Data Used: ANES                                                  
#################################################################################

library(package = "tidyverse")
library(haven)
library(package = "gridExtra")


anes <- read_dta("anes.dta")

#we're going to build a measure of racial resentment. This comes from four 
#questions: V202300, V202301, V202302, and V202303. Higher values on this 
#scale indicate higher levels of racial resentment

anes.hw5<-anes%>%
  mutate(V202300 = na_if(x = V202300, y = -9)) %>%
  mutate(V202300 = na_if(x = V202300, y = -8)) %>%
  mutate(V202300 = na_if(x = V202300, y = -7)) %>%
  mutate(V202300 = na_if(x = V202300, y = -6)) %>%
  mutate(V202300 = na_if(x = V202300, y = -5)) %>%
  mutate(V202300 = 6 - V202300) %>%
  mutate(V202301 = na_if(x = V202301, y = -9)) %>%
  mutate(V202301 = na_if(x = V202301, y = -8)) %>%
  mutate(V202301 = na_if(x = V202301, y = -7)) %>%
  mutate(V202301 = na_if(x = V202301, y = -6)) %>%
  mutate(V202301 = na_if(x = V202301, y = -5)) %>%
  mutate(V202302 = na_if(x = V202302, y = -9)) %>%
  mutate(V202302 = na_if(x = V202302, y = -8)) %>%
  mutate(V202302 = na_if(x = V202302, y = -7)) %>%
  mutate(V202302 = na_if(x = V202302, y = -6)) %>%
  mutate(V202302 = na_if(x = V202302, y = -5)) %>%
  mutate(V202303 = na_if(x = V202303, y = -9)) %>%
  mutate(V202303 = na_if(x = V202303, y = -8)) %>%
  mutate(V202303 = na_if(x = V202303, y = -7)) %>%
  mutate(V202303 = na_if(x = V202303, y = -6)) %>%
  mutate(V202303 = na_if(x = V202303, y = -5)) %>%
  mutate(V202303 = 6- V202303) %>%
  mutate(racial.resentment = V202300+V202301+V202302+V202303)

table(anes.hw5$racial.resentment)

##clean up the independent variables
anes.hw5<-anes.hw5%>%
  mutate(age = V201507x)%>%
  mutate(age = na_if(x=age, y=-9))%>%
  mutate(sex = as.factor(V201600))%>%
  mutate(sex = recode_factor(.x= sex,
                             '-9' = NA_character_,
                             '1' = "Male",
                             '2' = "Female"))%>%
  mutate(income = V201617x)%>%
  mutate(income = na_if(x=income, y=-5))%>%
  mutate(income = na_if(x=income, y=-9))%>%
  mutate(ideology = V201200)%>%
  mutate(ideology = na_if(x=ideology, y=-8))%>% 
  mutate(ideology = na_if(x=ideology, y=-9))%>% 
  mutate(ideology = na_if(x=ideology, y=-99))%>% 
  mutate(ideology = na_if(x=ideology, y=99))%>% 
  mutate(party = V201231x)%>%
  mutate(party = na_if(x=party, y=-8))%>% 
  mutate(party = na_if(x=party, y=-9))%>% 
  mutate(party = na_if(x=party, y=-99))%>% 
  mutate(education = recode_factor(.x=as.factor(V201510),
                                   '-8' = NA_character_,
                                   '-9' = NA_character_,
                                   '1' = "Not college",
                                   '2' = "Not college",
                                   '3' = "Not college",
                                   '4' = "Not college",
                                   '5' = "Not college", #Note that this combines the categories
                                   '6' = "College degree",
                                   '7' = "College degree",
                                   '8' = "College degree",
                                   '95' = NA_character_))%>%
  mutate(spending = V201246) %>%
  mutate(spending = na_if(x = spending, y = -9)) %>%
  mutate(spending = na_if(x = spending, y = 99)) %>%
  mutate(trust.gov = V201233) %>%
  mutate(trust.gov = na_if(x = trust.gov, y = -9)) %>%
  mutate(trust.gov = na_if(x = trust.gov, y = -8)) %>%
  mutate(race = recode_factor(.x= as.factor(V201549x),
                              '-8' = NA_character_,
                              '-9' = NA_character_,
                              '1' = "White, non-Hispanic",
                              '2' = "Black, non-Hispanic",
                              '3' = "Hispanic",
                              '4' = "Asian or Native Hawaiian/other Pacific Islander",
                              '5' = "Native American/Alaska Native or other race",
                              '6' = "Other race"
  ))%>%
  select(racial.resentment, party, ideology, race, sex, education, income, age, spending, trust.gov)


# 1: select one White, non-hispanic respondents
anes.hw5.white <- anes.hw5 %>%
  select(race = "White, non-Hispanic")

#2: Create a bivariate regression model with the racial resentment scale as the 
# dependent variable and trust in government as the independent variable.


#3: Interpret the bivaraite model results


#4: Create a multiple regression adding all the other variables to the model

#5: Interpre the multiple regression results

