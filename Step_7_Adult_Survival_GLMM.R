# Remove everything in your environment (good practice at the start of an R session)
rm(list=ls(all=TRUE)) 

# install/load relevant packages
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("tidyverse")
library(tidyverse)
install.packages('janitor')
library(janitor)
install.packages('skimr')
library(skimr)
install.packages('lme4')
library(lme4)
install.packages('ggeffects')
library(ggeffects)
install.packages('performance')
library(performance)
install.packages('sjPlot')
library(sjPlot)
install.packages('paletteer')
library(paletteer)

# Read in the data
blue_tit_survival <- read.csv("C:/Users/EyoAl/Documents/Documents/Edinburgh Uni/Dissertation/Data_Processing_&_Modelling/data/blue_tit_survival.csv", header = TRUE, stringsAsFactors = F)

# make sure the model knows that sex and age_cat are categorical variables using the as.factor() function
blue_tit_survival$sex <- as.factor(blue_tit_survival$sex)
blue_tit_survival$age_cat <- as.factor(blue_tit_survival$age_cat)

### Make a new column called site_year and add it to the blue tit survival data-frame 
blue_tit_survival$site_year <- paste(blue_tit_survival$site_recode, blue_tit_survival$year,  sep='_')


# check the ranges of years to ensure year only goes up to 2023
range(blue_tit_survival$year)


########
###GLMMs
########
 
### Fit a GLMM that tests all potentially biologically important fixed and random effects 

#Fixed effects: sex, age category (age_cat), between site effects of occupancy (mean_occ) and fledgeling success (suc_mean), within site (between year) effects of occupancy (rel_occ) and fledgling success (rel_suc), mean daily minimum winter temperature (mean_temp), total winter precipitation (total_precip)
#Random effects: year, site, site_year
model <- glmer(survival ~ sex + age_cat + scale(mean_occ) + scale(suc_mean) + scale(rel_occ) + scale(rel_suc) + scale(mean_temp) + scale(total_precip) + (1|year) + (1|site) + (1|site_year), data = blue_tit_survival, family = binomial)
summary(model)

###Test for covariance


# Test for correlations/multicollinearity in the model
check_collinearity(model)
#Moderate correlation between mean_occ and suc_mean
?check_collinearity()
# Test for correlation between mean_occ and suc_mean in the data
cor.test(blue_tit_survival$suc_mean, blue_tit_survival$mean_occ)
#Very highly correlated (0.9)

### Fit a GLMM similar to the last model, but excluding suc_mean and rel_suc

#There was correlation between mean_occ and suc_mean so one must be excluded
#Since fledgling success has no significant/biologically important effect within or between sites then suc_mean and rel_suc will be excluded

model1 <- glmer(survival ~ sex + age_cat + scale(mean_occ) + scale(rel_occ) + scale(mean_temp) + scale(total_precip) + (1|year) + (1|site) + (1|site_year), data = blue_tit_survival, family = binomial)
summary(model1)
#AIC is 1.7 lower than in the full model
#Variances of random effects are similar to the full model
#Results associated with sex and age category are similar to the full model
#Effect of mean_occ is less positive than in the full model whilst the associated standard error and p values are lower
#Effect of rel_occ is slightly more negative than in the full model whilst the standard error is marginally lower and the p value is much lower
#The Effects, standard error and p values associated with mean_temp and total_precip are similar to the full model (p value associated with mean_temp is marginally higher)

# Test multicollinearity in model1
check_collinearity(model1)
#Overall low levels of correlation in the model

### Fit a GLMM similar to model1, but excluding site_year as a random effect
model2 <- glmer(survival ~ sex + age_cat + scale(mean_occ) + scale(rel_occ) + scale(mean_temp) + scale(total_precip) + (1|year) + (1|site), data = blue_tit_survival, family = binomial)
summary(model2)
#AIC is 2 lower than in model1
#Variances associated with site and year are similar to model1
#Results associated with sex and age category are similar to model1
#Results associated with mean_occ and rel_occ are similar to model1
#Results associated with mean_temp and total_precip are similar to model1


### Fit a GLMM similar to model1, but excluding year as a random effect
model3 <- glmer(survival ~ sex + age_cat + scale(mean_occ) + scale(rel_occ) + scale(mean_temp) + scale(total_precip) + (1|site) + (1|site_year), data = blue_tit_survival, family = binomial)
summary(model3)
#AIC is 2 higher than in model1
#Variance associated with site is similar to model1
#Variance associated with site_year is higher than in model1
#Results associated with sex and age category are similar to model1
#Results associated with mean_occ and rel_occ are similar to model1 (although p values are even lower)
#Negative effect of mean_temp is slightly higher in magnitude than in model1 and its effect is significant (p= 0.05)
#Negative effect of total_precip is higher in magnitude than in model1 and its p value is lower 

### Fit a GLMM similar to model1, but excluding site as a random effect
model4 <- glmer(survival ~ sex + age_cat + scale(mean_occ) + scale(rel_occ) + scale(mean_temp) + scale(total_precip) + (1|year) + (1|site_year), data = blue_tit_survival, family = binomial)
summary(model4)
#AIC is 4.6 higher than in model1
#Variance associated with year is similar to model1
#Variance associated with site_year is higher than in model1
#Results associated with sex and age category are similar to model1 (p value associated with age category is even lower)
#Results associated with mean_occ and rel_occ are similar to model1 (error and p values associated with mean_occ are lower)
#Results associated with mean_temp and total_precip are similar to model1

### Use anova() to compare models 2-4 with model 1

anova(model1, model2)
#model1 and model2 are not significantly different

anova(model1, model3)
#model1 is significantly better than model3

anova(model1, model4)
#model1 is significantly better than model4
