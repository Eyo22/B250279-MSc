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
install.packages('patchwork')
library(patchwork)
install.packages('MuMIn')
library(MuMIn)

# Read in the data
blue_tit_survival <- read.csv("C:/Users/EyoAl/Documents/Documents/Edinburgh Uni/Dissertation/Data_Processing_&_Modelling/data/blue_tit_survival.csv", header = TRUE, stringsAsFactors = F)

# make sure the model knows that sex and age_cat are categorical variables using the as.factor() function
blue_tit_survival$sex <- as.factor(blue_tit_survival$sex)
blue_tit_survival$age_cat <- as.factor(blue_tit_survival$age_cat)

### Make a new column called site_year and add it to the blue tit survival data-frame 
blue_tit_survival$site_year <- paste(blue_tit_survival$site_recode, blue_tit_survival$year,  sep='_')


# check the ranges of years to ensure year only goes up to 2023
range(blue_tit_survival$year)

# Run the model you want to plot and view the output
model <- glmer(survival ~ sex + age_cat + scale(mean_occ) + scale(rel_occ) + scale(mean_temp) + scale(total_precip) + (1|year) + (1|site) + (1|site_year), data = blue_tit_survival, family = binomial)
summary(model)
#NB: this model is model1 (the model which excludes the metrics related to fledgling success)

##########
###Tables
##########

### Generate a table of results name the object and save it to the relevant file

# Define a character vector with names of predicted variables
pred_names <- c("(Intercept)", "Sex[M]", "Age category[Young]", "Mean occupancy", "Relative occupancy", "Mean daily minimum winter temperature", "Total winter precipitation")

# Generate the table
tab_model(model, transform= NULL, show.se = TRUE, show.stat = TRUE, show.icc = FALSE, show.r2 = FALSE, digits.re = 3, pred.labels = pred_names, title = "GLMM Output", file = "model_results.html")
summary(model)
##########
###Plots
##########

#####
###Plotting the effects of Occupancy
#####

### Generate a plot of annual survival probability as a function of Relative occupancy (with both predicted survival and mean survival as calculated from the raw data)

# Check the range of the continuous predictor
range(blue_tit_survival$rel_occ)

# Visualise the frequency of different values of the continuous predictor in the data
ggplot(blue_tit_survival) + 
  geom_histogram(aes(rel_occ)) + 
  labs(x = "rel_occ")
#frequency of rel_occ values has a roughly normal distribution around 0.0

# Create bins for the continuous predictor in order to calculate mean survival values from the raw data
data <- blue_tit_survival %>%
  mutate(bin = cut(rel_occ, breaks = 10, include.lowest = TRUE, labels = FALSE))

# Define a function to calculate standard error
se <- function(x) {
  sd(x) / sqrt(length(x))
}

# Group by bin and calculate the mean and standard error of survival values
summary_data <- data %>%
  group_by(bin) %>%
  summarise(
    mean_survival = mean(survival), # Calculate means for respective groups of binned values in the survival column
    se_survival = se(survival),     # Calculate standard errors for respective groups of binned values in the survival column
    bin_center = mean(rel_occ)  # Calculate the center of each bin
  )

# Extract predicted model values (conditional on sex being "F" and age_cat being "O")
pred_rel_occ <- ggpredict(model, terms="rel_occ[all]", type = "fixed", condition = c(sex = "F", age_cat = "O"))

# Inspect the colours in your colour pallete
paletteer_d("nationalparkcolors::Acadia")

## Generate the plot
survival_vs_rel_occ <- ggplot() + 
  
  #set the theme
  theme_light() + 
  
  #set the colour palette
  scale_color_paletteer_d("nationalparkcolors::Acadia") +

  #plot predicted slope
  geom_line(data = pred_rel_occ, aes(x = x, y = predicted), size = 1, color = "#476F84FF") +
  
  #visualise the upper and lower 95% CIs around the predicted slope
  geom_ribbon(data = pred_rel_occ, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high), alpha = 0.4, fill = "#476F84FF", colour = NA) +
  
  #plot the mean values from the binned raw data
  geom_point(data = summary_data, aes(x = bin_center, y = mean_survival), color = "#453947FF") +
  
  #visualise the upper and lower 68% CIs (+/- 1*standard error) around the mean values
  geom_errorbar(data = summary_data, aes(x = bin_center, ymin = mean_survival - se_survival, ymax = mean_survival + se_survival), alpha = 0.4, width = 0.03, linetype= 1, color = "#453947FF") +
  
  #set the scale of the y axis
  scale_y_continuous(limits = c(0.00,1.00), breaks = seq(0.00, 1.00, by = 0.25)) +
  
  #set axis labels
  labs(x = "Relative Occupancy", y = "Annual Survival Probability")
  
# call the plot
survival_vs_rel_occ

### Generate a plot of annual survival probability as a function of Mean Proportional Occupancy (with both predicted survival and mean survival as calculated from the raw data)

# Check the range of the continuous predictor
range(blue_tit_survival$mean_occ)

# Visualise the frequency of different values of the continuous predictor in the data
ggplot(blue_tit_survival) + 
  geom_histogram(aes(mean_occ)) + 
  labs(x = "mean_occ")
#frequency of mean_occ values is not normally distributed
#there are more rows representing mean_occ values above 0.50

# Create bins for the continuous predictor in order to calculate mean survival values from the raw data
data <- blue_tit_survival %>%
  mutate(bin = cut(mean_occ, breaks = 10, include.lowest = TRUE, labels = FALSE))

# Define a function to calculate standard error
se <- function(x) {
  sd(x) / sqrt(length(x))
}

# Group by bin and calculate the mean and standard error of survival values
summary_data <- data %>%
  group_by(bin) %>%
  summarise(
    mean_survival = mean(survival), # Calculate means for respective groups of binned values in the survival column
    se_survival = se(survival),     # Calculate standard errors for respective groups of binned values in the survival column
    bin_center = mean(mean_occ)  # Calculate the center of each bin
  )

# Extract predicted model values (conditional on sex being "F" and age_cat being "O")
pred_mean_occ <- ggpredict(model, terms="mean_occ[all]", type = "fixed",  condition = c(sex = "F", age_cat = "O"))


## Generate the plot
survival_vs_mean_occ <- ggplot() + 
  
  #set the theme
  theme_light() + 
  
  #set the colour palette
  scale_color_paletteer_d("nationalparkcolors::Acadia") +
  
  #plot predicted slope
  geom_line(data = pred_mean_occ, aes(x = x, y = predicted), size = 1, color = "#476F84FF") +
  
  #visualise the upper and lower 95% CIs around the predicted slope
  geom_ribbon(data = pred_mean_occ, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high), alpha = 0.4, fill = "#476F84FF", colour = NA) +
  
  #plot the mean values from the binned raw data
  geom_point(data = summary_data, aes(x = bin_center, y = mean_survival), color = "#453947FF") +
  
  #visualise the upper and lower 68% CIs (+/- 1*standard error) around the mean values
  geom_errorbar(data = summary_data, aes(x = bin_center, ymin = mean_survival - se_survival, ymax = mean_survival + se_survival), alpha = 0.4, width = 0.03, linetype= 1, color = "#453947FF") +
  
  #set the scale of the y axis
  scale_y_continuous(limits = c(0.00,1.00), breaks = seq(0.00, 1.00, by = 0.25)) +
  
  #set axis labels
  labs(x = "Mean Occupancy", y = "Annual Survival Probability")

# call the plot
survival_vs_mean_occ


#####
###Plotting the effects of Sex
#####

### Generate a plot of annual survival probability as a function of sex (with predicted survival)

# Extract predicted model values (conditional on age_cat being "O")
pred_sex <- ggpredict(model, terms="sex[all]", type = "fixed",  condition = c(age_cat = "O"))

## Generate the plot
survival_vs_sex_predicted <- ggplot() + 
   
   #set the theme
   theme_light() + 
   
   #set the colour palette
   scale_color_paletteer_d("nationalparkcolors::Acadia") +

  #plot the predicted values along with their upper and lower 95% CIs
  geom_crossbar(data = pred_sex, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high), alpha = 0.4, color = "#476F84FF", fill = "#476F84FF") +

  #set the scale of the y axis
  scale_y_continuous(limits = c(0.00,1.00), breaks = seq(0.00, 1.00, by = 0.25)) +
  
  #set axis labels
  labs(x = "Sex", y = "Annual Survival Probability")

# call the plot
survival_vs_sex_predicted



### Generate a plot of annual survival probability as a function of sex (with mean survival as calculated from the raw data)

# Visualise the representation of males and females in the data
ggplot(blue_tit_survival) + 
  geom_bar(aes(sex)) + 
  labs(x = "sex")
#More rows representing females in the data than males

# Define a function to calculate standard error
se <- function(x) {
  sd(x) / sqrt(length(x))
}

# Group by sex and calculate the mean and standard error of survival values
summary_data <- blue_tit_survival %>%
  group_by(sex) %>%
  summarise(
    mean_survival = mean(survival), # Calculate the respective means for males and females in the survival column
    se_survival = se(survival),     # Calculate standard errors of these means
  )

## Generate the plot
survival_vs_sex_mean <- ggplot(data = summary_data) + 
  
  #set the theme
  theme_light() + 
  
  #set the colour palette
  scale_color_paletteer_d("nationalparkcolors::Acadia") +
  
  #plot the mean values from the raw data
  geom_point(mapping = aes(x = sex, y = mean_survival), color = "#453947FF") +
  
  #visualise the upper and lower 68% CIs (+/- 1*standard error) around the mean values
  geom_errorbar(data = summary_data, aes(x = sex, ymin = mean_survival - se_survival, ymax = mean_survival + se_survival), alpha = 0.4, width = 0.03, linetype= 1, color = "#453947FF") +
 
  #set the scale of the y axis
  scale_y_continuous(limits = c(0.00,1.00), breaks = seq(0.00, 1.00, by = 0.25)) +
  
  #set axis labels
  labs(x = "Sex", y = "Annual Survival Probability")

# call the plot
survival_vs_sex_mean

#####
###Plotting the effects of Age Category
#####

### Generate a plot of annual survival probability as a function of age category (with predicted survival)

# Extract predicted model values (conditional on sex being "F")
pred_age_cat <- ggpredict(model, terms= "age_cat[all]", type = "fixed",  condition = c(sex = "F"))

## Generate the plot
survival_vs_age_cat_predicted <- ggplot() + 
  
  #set the theme
  theme_light() + 
  
  #set the colour palette
  scale_color_paletteer_d("nationalparkcolors::Acadia") +
  
  #plot the predicted values along with their upper and lower 95% CIs
  geom_crossbar(data = pred_age_cat, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high), alpha = 0.4, color = "#476F84FF", fill = "#476F84FF") +
  
  #set the scale of the y axis and specify the name and order of x axis categories
  scale_y_continuous(limits = c(0.00,1.00), breaks = seq(0.00, 1.00, by = 0.25)) +
  scale_x_discrete(limits = c("Y","O"), labels = c("Age Category", "O" ="Old", "Y" = "Young")) +
  
  #set axis labels
  labs(x = "Age Category", y = "Annual Survival Probability")
  
# call the plot
survival_vs_age_cat_predicted



### Generate a plot of annual survival probability as a function of age category (with mean survival as calculated from the raw data)

# Visualise the representation of yearlings (Y) and older adults (O) in the data
ggplot(blue_tit_survival) + 
  geom_bar(aes(age_cat)) + 
  labs(x = "age_cat")
#There are considerably less yearlings represented in the data than older adults

# Define a function to calculate standard error
se <- function(x) {
  sd(x) / sqrt(length(x))
}

# Group by age_cat and calculate the mean and standard error of survival values
summary_data <- blue_tit_survival %>%
  group_by(age_cat) %>%
  summarise(
    mean_survival = mean(survival), # Calculate the respective means for O and Y adults in the survival column
    se_survival = se(survival),     # Calculate standard errors of these means
  )

## Generate the plot
survival_vs_age_cat_mean <- ggplot(data = summary_data) + 
  
  #set the theme
  theme_light() + 
  
  #set the colour palette
  scale_color_paletteer_d("nationalparkcolors::Acadia") +
  
  #plot the mean values from the raw data
  geom_point(mapping = aes(x = age_cat, y = mean_survival), color = "#453947FF") +
  
  #visualise the upper and lower 68% CIs (+/- 1*standard error) around the mean values
  geom_errorbar(data = summary_data, aes(x = age_cat, ymin = mean_survival - se_survival, ymax = mean_survival + se_survival), alpha = 0.4, width = 0.03, linetype= 1, color = "#453947FF") +
  
  #set the scale of the y axis and specify the name and order of x axis categories
  scale_y_continuous(limits = c(0.00,1.00), breaks = seq(0.00, 1.00, by = 0.25)) +
  scale_x_discrete(limits = c("Y","O"), labels = c("Age Category", "O" ="Old", "Y" = "Young")) +
  
  #set axis labels
  labs(x = "Age Category", y = "Annual Survival Probability")

# call the plot
survival_vs_age_cat_mean


#####
###Plotting the effects of Weather
#####

### Generate a plot of annual survival probability as a function of mean daily minimum winter temperature (with both predicted survival and mean survival as calculated from the raw data)

# Check the range of the continuous predictor
range(blue_tit_survival$mean_temp)

# Visualise the frequency of different values of the continuous predictor in the data
ggplot(blue_tit_survival) + 
  geom_histogram(aes(mean_temp)) + 
  labs(x = "mean_temp")
#frequency of mean_temp values seems to fluctuate markedly from low to high values, although the distribution is roughly normal in character and centered around 0.75

# Create bins for the continuous predictor in order to calculate mean survival values from the raw data
data <- blue_tit_survival %>%
  mutate(bin = cut(mean_temp, breaks = 10, include.lowest = TRUE, labels = FALSE))

# Define a function to calculate standard error
se <- function(x) {
  sd(x) / sqrt(length(x))
}

# Group by bin and calculate the mean and standard error of survival values
summary_data <- data %>%
  group_by(bin) %>%
  summarise(
    mean_survival = mean(survival), # Calculate means for respective groups of binned values in the survival column
    se_survival = se(survival),     # Calculate standard errors for respective groups of binned values in the survival column
    bin_center = mean(mean_temp)  # Calculate the center of each bin
  )

# Extract predicted model values (conditional on sex being "F" and age_cat being "O")
pred_mean_temp <- ggpredict(model, terms = "mean_temp[all]", type = "fixed",  condition = c(sex = "F", age_cat = "O"))

## Generate the plot
survival_vs_mean_temp <- ggplot() + 
  
  #set the theme
  theme_light() + 
  
  #set the colour palette
  scale_color_paletteer_d("nationalparkcolors::Acadia") +
  
  #plot predicted slope
  geom_line(data = pred_mean_temp, aes(x = x, y = predicted), size = 1, color = "#476F84FF") +
  
  #visualise the upper and lower 95% CIs around the predicted slope
  geom_ribbon(data = pred_mean_temp, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high), alpha = 0.4, fill = "#476F84FF", colour = NA) +
  
  #plot the mean values from the binned raw data
  geom_point(data = summary_data, aes(x = bin_center, y = mean_survival), color = "#453947FF") +
  
  #visualise the upper and lower 68% CIs (+/- 1*standard error) around the mean values
  geom_errorbar(data = summary_data, aes(x = bin_center, ymin = mean_survival - se_survival, ymax = mean_survival + se_survival), alpha = 0.4, width = 0.03, linetype= 1, color = "#453947FF") +
  
  #set the scale of the y axis
  scale_y_continuous(limits = c(0.00,1.00), breaks = seq(0.00, 1.00, by = 0.25)) +

  #set axis labels
  labs(x = "Mean Daily Minimum Winter Temperature(\u00B0C)", y = "Annual Survival Probability")

# call the plot
survival_vs_mean_temp

### Generate a plot of annual survival probability as a function of total winter precipitation (with both predicted survival and mean survival as calculated from the raw data)

# Check the range of the continuous predictor
range(blue_tit_survival$total_precip)

# Visualise the frequency of different values of the continuous predictor in the data
ggplot(blue_tit_survival) + 
  geom_histogram(aes(total_precip)) + 
  labs(x = "total_precip")
#frequency of total_precip values seems to be much higher at relatively lower values ~300

# Create bins for the continuous predictor in order to calculate mean survival values from the raw data
data <- blue_tit_survival %>%
  mutate(bin = cut(total_precip, breaks = 6, include.lowest = TRUE, labels = FALSE))
#Use less bins to account for the extremely low representation of high precipitation values

# Define a function to calculate standard error
se <- function(x) {
  sd(x) / sqrt(length(x))
}

# Group by bin and calculate the mean and standard error of survival values
summary_data <- data %>%
  group_by(bin) %>%
  summarise(
    mean_survival = mean(survival), # Calculate means for respective groups of binned values in the survival column
    se_survival = se(survival),     # Calculate standard errors for respective groups of binned values in the survival column
    bin_center = mean(total_precip)  # Calculate the center of each bin
  )

# Extract predicted model values (conditional on sex being "F" and age_cat being "O")
pred_total_precip <- ggpredict(model, terms = "total_precip[all]", type = "fixed",  condition = c(sex = "F", age_cat = "O"))

## Generate the plot
survival_vs_total_precip <- ggplot() + 
  
  #set the theme
  theme_light() + 
  
  #set the colour palette
  scale_color_paletteer_d("nationalparkcolors::Acadia") +
  
  #plot predicted slope
  geom_line(data = pred_total_precip, aes(x = x, y = predicted), size = 1, color = "#476F84FF") +
  
  #visualise the upper and lower 95% CIs around the predicted slope
  geom_ribbon(data = pred_total_precip, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high), alpha = 0.4, fill = "#476F84FF", colour = NA) +
  
  #plot the mean values from the binned raw data
  geom_point(data = summary_data, aes(x = bin_center, y = mean_survival), color = "#453947FF") +
  
  #visualise the upper and lower 68% CIs (+/- 1*standard error) around the mean values
  geom_errorbar(data = summary_data, aes(x = bin_center, ymin = mean_survival - se_survival, ymax = mean_survival + se_survival), alpha = 0.4, width = 0.03, linetype= 1, color = "#453947FF") +
  
  #set the scale of the y axis
  scale_y_continuous(limits = c(-0.00,1.00), breaks = seq(0.00, 1.00, by = 0.25)) +
  
  #set axis labels
  labs(x = "Total Winter Precipitation(mm)", y = "Annual Survival Probability")

# call the plot
survival_vs_total_precip
