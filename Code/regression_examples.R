# Regression Example
# packages: haven, tidyverse, broom

# Use cleaned data (anes.small.cleaned) from AnRExercise.R

#### EXAMPLE 1 ####

# VARIABLES:
# Dependent variable: Biden feeling thermometer (0-100) [interval data]
# Independent Variable: Biden affect (0-10) [interval data]

# CORRELATION A
cor(x = anes.small.cleaned$biden.affect, y = anes.small.cleaned$biden.ft,
    use = "complete.obs")

# SCATTERPLOT A
scatter.1 <- anes.small.cleaned %>%
  ggplot(aes(x = biden.affect, y = biden.ft)) +
  geom_point() +
  geom_point(position = position_jitter(width = .5, height = 10), 
             shape = 21, alpha = 0.6) +
  geom_smooth() +
  theme_minimal() +
  xlab("Biden Affect") +
  ylab("Biden Feeling Thermometer")
scatter.1

# REGRESSION

# regression equation
mod.1 <- lm(biden.ft ~ biden.affect, 
            data = anes.small.cleaned, na.action = "na.omit")
# display regression results
summary(mod.1)

# add a control for ideology
# regression equation
mod.2 <- lm(biden.ft ~ biden.affect + ideology, 
            data = anes.small.cleaned, na.action = "na.omit")
# display regression results
summary(mod.2)



# load a package to make a nicer looking table
library(broom)

# display a nicer looking table of regression coefs
tidy(mod.1)

# display model fit statistics
glance(mod.1)

#### EXAMPLE 2 ####

# VARIABLES
# Dependent variable: biden.affect
# Independent variable: marital.status

# Bivariate test: difference of means
diff.test2 <- t.test(biden.affect ~ marital.status, data = anes.small.cleaned, var.eqaul = T)
diff.test2

# Difference of means box plot (marital.status and biden.affect)
diff.test2.fig2 <- anes.small.cleaned %>%
  drop_na(marital.status) %>%
  ggplot(aes(x = marital.status, y = biden.affect)) +
  geom_boxplot() +
  scale_x_discrete(labels = c("Married", "Single")) +
  theme_minimal() +
  xlab("Marital Status") +
  ylab("Biden Affect")
diff.test2.fig2

# REGRESSION

# regression equation
mod.2 <- lm(biden.affect ~ marital.status, 
            data = anes.small.cleaned, na.action = "na.omit")
# display regression results
summary(mod.2)

# load a package to make a nicer looking table
library(broom)

# display a nicer looking table of regression coefs
tidy(mod.2)

# display model fit statistics
glance(mod.2)


#### EXAMPLE 3 ####

# REGRESSIONS

mod.3a <- lm(biden.affect ~ ideology, 
            data = anes.small.cleaned, na.action = "na.omit")
summary(mod.3a)
tidy(mod.3a)
glance(mod.3a)

mod.3b <- lm(biden.affect ~ marital.status + ideology, 
             data = anes.small.cleaned, na.action = "na.omit")
summary(mod.3b)
tidy(mod.3b)
glance(mod.3b)
