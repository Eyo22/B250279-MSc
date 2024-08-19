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

# Read in the data
blue_tit_survival <- read.csv("C:/Users/EyoAl/Documents/Documents/Edinburgh Uni/Dissertation/Data_Processing_&_Modelling/data/blue_tit_survival.csv", header = TRUE, stringsAsFactors = F)
Weather <- read.csv("C:/Users/EyoAl/Documents/Documents/Edinburgh Uni/Dissertation/Data_Processing_&_Modelling/data/Weather.csv", header = TRUE, stringsAsFactors = F)

# Check the range of years included in blue_tit_survival and Weather dataframes respectively
range(blue_tit_survival$year)
range(Weather$year)
#Both should go up to 2023

##########
###Temperature
##########

###Assign each row in the blue_tit_survival data-frame with a mean_temp value corresponding to its site & year labels (using the Weather data-frame)

# Create a new vector "mean_temp" for the blue_tit_survival_weather data-frame
mean_temp <- numeric(2956)

# Create a for loop that assigns each index with the correct mean_temp value using site and year to find the corresponding value in the Weather data-frame
for(i in 1:2956){
  mean_temp[i] <- subset(Weather, site == blue_tit_survival[i,"site_recode"] & year == blue_tit_survival[i,"year"])[1,"mean_temp"]
}

# add the mean_temp vector to the blue_tit_survival data-frame as a new column
column_name <- "mean_temp"
blue_tit_survival[[column_name]] <- mean_temp

# ensure it represents numeric data
class(blue_tit_survival$mean_temp)



###Assign each row in the blue_tit_survival data-frame with a mean_mean_temp value corresponding to its site & year labels (using the Weather data-frame)

# Create a new vector "mean_mean_temp" for the blue_tit_survival data-frame
mean_mean_temp <- numeric(2956)

# Create a for loop that assigns each index with the correct mean_mean_temp value using site and year to find the corresponding value in the Weather data-frame
for(i in 1:2956){
  mean_mean_temp[i] <- subset(Weather, site == blue_tit_survival[i,"site_recode"] & year == blue_tit_survival[i,"year"])[1,"mean_mean_temp"]
}

# add the mean_mean_temp vector to the blue_tit_survival data-frame as a new column
column_name <- "mean_mean_temp"
blue_tit_survival[[column_name]] <- mean_mean_temp

# ensure it represents numeric data
class(blue_tit_survival$mean_mean_temp)



###Assign each row in the blue_tit_survival data-frame with a rel_mean_temp value corresponding to its site & year labels (using the Weather data-frame)

# Create a new vector "rel_mean_temp" for the blue_tit_survival data-frame
rel_mean_temp <- numeric(2956)

# Create a for loop that assigns each index with the correct rel_mean_temp value using site and year to find the corresponding value in the Weather data-frame
for(i in 1:2956){
  rel_mean_temp[i] <- subset(Weather, site == blue_tit_survival[i,"site_recode"] & year == blue_tit_survival[i,"year"])[1,"rel_mean_temp"]
}

# add the rel_mean_temp vector to the blue_tit_survival data-frame as a new column
column_name <- "rel_mean_temp"
blue_tit_survival[[column_name]] <- rel_mean_temp

# ensure it represents numeric data
class(blue_tit_survival$rel_mean_temp)

#NB: Ensure you compare rows in blue_tit_survival to rows with a corresponding site-year combination in Weather to check that the temperature metrics have been transferred correctly

##########
###Precipitation
##########

###Assign each row in the blue_tit_survival data-frame with a total_precip value corresponding to its site & year labels (using the Weather data-frame)

# Create a new vector "total_precip" for the blue_tit_survival data-frame
total_precip <- numeric(2956)

# Create a for loop that assigns each index with the correct total_precip value using site and year to find the corresponding value in the Weather data-frame
for(i in 1:2956){
  total_precip[i] <- subset(Weather, site == blue_tit_survival[i,"site_recode"] & year == blue_tit_survival[i,"year"])[1,"total_precip"]
}

# add the total_precip vector to the blue_tit_survival data-frame as a new column
column_name <- "total_precip"
blue_tit_survival[[column_name]] <- total_precip

# ensure it represents numeric data
class(blue_tit_survival$total_precip)

###Assign each row in the blue_tit_survival data-frame with a mean_total_precip value corresponding to its site & year labels (using the Weather data-frame)

# Create a new vector "mean_total_precip" for the blue_tit_survival data-frame
mean_total_precip <- numeric(2956)

# Create a for loop that assigns each index with the correct mean_total_precip value using site and year to find the corresponding value in the Weather data-frame
for(i in 1:2956){
  mean_total_precip[i] <- subset(Weather, site == blue_tit_survival[i,"site_recode"] & year == blue_tit_survival[i,"year"])[1,"mean_total_precip"]
}

# add the mean_total_precip vector to the blue_tit_survival data-frame as a new column
column_name <- "mean_total_precip"
blue_tit_survival[[column_name]] <- mean_total_precip

# ensure it represents numeric data
class(blue_tit_survival$mean_total_precip)



###Assign each row in the blue_tit_survival data-frame with a rel_total_precip value corresponding to its site & year labels (using the Weather data-frame)

# Create a new vector "rel_total_precip" for the blue_tit_survival data-frame
rel_total_precip <- numeric(2956)

# Create a for loop that assigns each index with the correct rel_total_precip value using site and year to find the corresponding value in the Weather data-frame
for(i in 1:2956){
  rel_total_precip[i] <- subset(Weather, site == blue_tit_survival[i,"site_recode"] & year == blue_tit_survival[i,"year"])[1,"rel_total_precip"]
}

# add the rel_total_precip vector to the blue_tit_survival data-frame as a new column
column_name <- "rel_total_precip"
blue_tit_survival[[column_name]] <- rel_total_precip

# ensure it represents numeric data
class(blue_tit_survival$rel_total_precip)

#NB: Ensure you compare rows in blue_tit_survival to rows with a corresponding site-year combination in Weather to check that the precipitation metrics have been transferred correctly



### Re-save the edited blue_tit_survival data-frame as a csv file
write.csv(blue_tit_survival, 'blue_tit_survival.csv', row.names=FALSE)
