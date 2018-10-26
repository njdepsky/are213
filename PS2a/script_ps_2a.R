# PS 2a for ARE 213 by Peter Worley
# 10/26/2018

# enter: "ctrl+shift+c" to comment/uncomment blocks of code
# enter: "ctrl"shift"enter" to run entire script

install.packages("pacman")
library(pacman)
p_load(dplyr,haven,readr,ggplot2,stargazer)  #p_load allows you to install AND load numerous packages on one line

# The path to my ARE 213 folder (ARE213)
dir_ps <- "/Users/Peter/Documents/Berkeley/Courses/ARE 213/Problem Sets/2a/"

#load data from file into dataframe
all_data <- read_dta(paste0(dir_ps,"traffic_safety2.dta"))

# add log(fatalities per capita) into dataframe
Fpcl <- log(all_data$fatalities/all_data$population)
all_data <- cbind(all_data, Fpcl)

## (A-1)
# Create pooled bivariate on primary-law
bv_OLS_model <- lm(Fpcl ~ primary, data=all_data)

summary(bv_OLS_model)

# cf1 = coef(bv_OLS_model)
# xx = cf1[2]
# lx = exp(xx)
# lx


# Add a quadratic time trend
t_2 <- (all_data$year-1981)^2
all_data <- cbind(all_data, t_2)
pt_model <- lm(Fpcl ~ primary + t_2, data=all_data)
summary(pt_model)


## (A-3)
# Add all other covariates to model that we think are important

a3_model <- lm(Fpcl ~ primary + t_2 + secondary + totalvmt + precip + snow32 + rural_speed + urban_speed, data=all_data)
summary(a3_model)


## (B-2)
# Compute the robust standard errors
# library(foreign)
# library(sandwich)
p_load(foreign, lmtest, sandwich)
coeftest(a3_model, vcov = vcovHC(a3_model, type="HC1"))   #"HC1" is the the type for robust standard errors

summary(a3_model, robust=T)

## (B-3)
# Compute the clustered standard errors
summary(a3_model, cluster=c("state"))

## (C-1)
# Compute between estimator without covariates

