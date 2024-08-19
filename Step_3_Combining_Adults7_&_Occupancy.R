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

# Read in the data (you will need to tell R where to find the relevant file)
adults7 <- read.csv("C:/Users/EyoAl/Documents/Documents/Edinburgh Uni/Dissertation/Data_Processing_&_Modelling/data/Adults7.csv", header = TRUE, stringsAsFactors = F)
Occupancy_2 <- read.csv("C:/Users/EyoAl/Documents/Documents/Edinburgh Uni/Dissertation/Data_Processing_&_Modelling/data/Occupancy_2.csv", header = TRUE, stringsAsFactors = F)


###Assign each row in the adults7 dataframe with a prop_occ value corresponding to its site & year labels (using the Occupancy_2 dataframe)
#Create a new vector "prop_occ" for the adults7 dataframe
prop_occ <- numeric(3243)

#create a for loop that assigns each index with the correct prop_occ value using site and year to find the corresponding value in the Occupancy_2 dataframe
for(i in 1:3243){
prop_occ[i] <- subset(Occupancy_2, site == adults7[i,"site_recode"] & year == adults7[i,"year"])[1,"prop_occ"]
  }

# add the prop_occ vector to the adults7 data frame as a new column
column_name <- "prop_occ"
adults7[[column_name]] <- prop_occ

# ensure it represents numeric data
class(adults7$prop_occ)

#NB: compare values in this new column with the corresponding Occupancy_2 values to ensure the code has worked as expected

###Assign each row in the adults7 dataframe with a mean_occ value corresponding to its site & year labels (using the Occupancy_2 dataframe)
#Create a new vector "mean_occ" for the adults7 dataframe
mean_occ <- numeric(3243)

#create a for loop that assigns each index with the correct mean_occ value using site and year to find the corresponding value in the Occupancy_2 dataframe
for(i in 1:3243){
  mean_occ[i] <- subset(Occupancy_2, site == adults7[i,"site_recode"] & year == adults7[i,"year"])[1,"mean_occ"]
}

# add the mean_occ vector to the adults7 data frame as a new column
column_name <- "mean_occ"
adults7[[column_name]] <- mean_occ

# ensure it represents numeric data
class(adults7$mean_occ)

#NB: compare values in this new column with the corresponding Occupancy_2 values to ensure the code has worked as expected


###Assign each row in the adults7 dataframe with a rel_occ value corresponding to its site & year labels (using the Occupancy_2 dataframe)
#Create a new vector "rel_occ" for the adults7 dataframe
rel_occ <- numeric(3243)

#create a for loop that assigns each index with the correct rel_occ value using site and year to find the corresponding value in the Occupancy_2 dataframe
for(i in 1:3243){
  rel_occ[i] <- subset(Occupancy_2, site == adults7[i,"site_recode"] & year == adults7[i,"year"])[1,"rel_occ"]
}

# add the rel_occ vector to the adults7 data frame as a new column
column_name <- "rel_occ"
adults7[[column_name]] <- rel_occ

# ensure it represents numeric data
class(adults7$rel_occ)

#NB: compare values in this new column with the corresponding Occupancy_2 values to ensure the code has worked as expected




###Assign each row in the adults7 dataframe with a suc_av value corresponding to its site & year labels (using the Occupancy_2 dataframe)
#Create a new vector "suc_av" for the adults7 dataframe
suc_av <- numeric(3243)

#create a for loop that assigns each index with the correct suc_av value using site and year to find the corresponding value in the Occupancy_2 dataframe
for(i in 1:3243){
  suc_av[i] <- subset(Occupancy_2, site == adults7[i,"site_recode"] & year == adults7[i,"year"])[1,"suc_av"]
}

# add the suc_av vector to the adults7 data frame as a new column
column_name <- "suc_av"
adults7[[column_name]] <- suc_av

# ensure it represents numeric data
class(adults7$suc_av)

#NB: compare values in this new column with the corresponding Occupancy_2 values to ensure the code has worked as expected

###Assign each row in the adults7 dataframe with a suc_mean value corresponding to its site & year labels (using the Occupancy_2 dataframe)
#Create a new vector "suc_mean" for the adults7 dataframe
suc_mean <- numeric(3243)

#create a for loop that assigns each index with the correct suc_mean value using site and year to find the corresponding value in the Occupancy_2 dataframe
for(i in 1:3243){
  suc_mean[i] <- subset(Occupancy_2, site == adults7[i,"site_recode"] & year == adults7[i,"year"])[1,"suc_mean"]
}

# add the suc_mean vector to the adults7 data frame as a new column
column_name <- "suc_mean"
adults7[[column_name]] <- suc_mean

# ensure it represents numeric data
class(adults7$suc_mean)

#NB: compare values in this new column with the corresponding Occupancy_2 values to ensure the code has worked as expected

###Assign each row in the adults7 dataframe with a rel_suc value corresponding to its site & year labels (using the Occupancy_2 dataframe)
#Create a new vector "rel_suc" for the adults7 dataframe
rel_suc <- numeric(3243)

#create a for loop that assigns each index with the correct rel_suc value using site and year to find the corresponding value in the Occupancy_2 dataframe
for(i in 1:3243){
  rel_suc[i] <- subset(Occupancy_2, site == adults7[i,"site_recode"] & year == adults7[i,"year"])[1,"rel_suc"]
}

# add the rel_suc vector to the adults7 data frame as a new column
column_name <- "rel_suc"
adults7[[column_name]] <- rel_suc

# ensure it represents numeric data
class(adults7$rel_suc)

#NB: compare values in this new column with the corresponding Occupancy_2 values to ensure the code has worked as expected

### Rename this modified dataframe as blue_tit_survival
blue_tit_survival <- adults7


### Subset blue_tit_survival to only include years up to 2023 (as survival between 2024 & 2025 is currently unknown and will be coded as zero)

blue_tit_survival <- subset(blue_tit_survival, year < 2024)
# Reset the row indices of the new data-frame (so they are in sequential order) - just to ensure it doesnt cause any issues
row.names(blue_tit_survival) <- seq_len(nrow(blue_tit_survival))

# check the ranges of years to ensure the code has worked properly
range(blue_tit_survival$year)

### Save blue_tit_survival as a CSV
write.csv(blue_tit_survival, 'blue_tit_survival.csv', row.names=FALSE)
