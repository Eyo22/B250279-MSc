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
### Read in the data
Temperature <- read.table("C:/Users/EyoAl/Documents/Documents/Edinburgh Uni/Dissertation/Data_Processing_&_Modelling/data/daily_mintemp_Hollis.txt", header=T, sep="\t")
head(Temperature)
#Rows represent site/year combinations
#Columns 3-368 represent days of the year


Precipitation <- read.table("C:/Users/EyoAl/Documents/Documents/Edinburgh Uni/Dissertation/Data_Processing_&_Modelling/data/daily_precipitation_Hollis.txt", header=T, sep="\t")
head(Precipitation)
#Rows represent site/year combinations
#Columns 3-368 represent days of the year

#NB: For leap years there are 366 columns (to account for the extra day)

#############
###Temperature
#############

###Which sites are represented
unique_temp_sites <- unique(Temperature$site)
unique_temp_sites
length(unique_temp_sites)
#all 44 sites are represented

###Which years are represented
unique_temp_years <- unique(Temperature$year)
unique_temp_years
length(unique_temp_years)
#there are 12 years of data from 2013 to 2024

###Which site-year combinations are represented
unique_temp_combinations <- unique(Temperature[c("year", "site")])
nrow(unique_temp_combinations)
#there are 528 possible combinations of site and year


###How many NAs are there for each site/year combination
temp_na_count_per_row <- apply(Temperature, 1, function(x) sum(is.na(x)))
temp_na_count_per_row
temp_na_count_per_row <- as.data.frame(temp_na_count_per_row)

# which unique values are there in the na_count_per_row dataframe
unique(temp_na_count_per_row$temp_na_count_per_row)
#There are either 1 0 or 184 NAs in each row
#0 for rows corresponding to the leap years 2016 and 2020
#184 for rows corresponding to the leap year 2024
#1 for all other years

###Which days are NAs for each row
temp_na_days_per_row <- apply(Temperature, 1, function(x) names(Temperature)[which(is.na(x))])
temp_na_days_per_row
#Rows in Temperature corresponding to leap year 2016: 133-176
#Rows in Temperature corresponding to leap year 2020: 309-352
#Rows in Temperature corresponding to leap year 2024: 485-528
#On non-leap years day 366 is has NA
#On leap years 2016 and 2020 there are no NAs
#On leap year 2024 there are NAs for 184 days spanning ordinal dates 183-366
# This is expected because in leap years an extra day is added at the end of February so the ordinal dates are shifted forward by 1 day after this point (therefore non-leap years will not have a 366th day)
# Data from July-December 2024 is missing which is to be expected as the dataset was created in 2024 and we only need data up to Februrary 2024

###Check the range of values in each row

# Create vectors for lower and upper extremes
lower_extreme <- numeric(528)
upper_extreme <- numeric(528)

# Run a for loop that calculates the range of values in each row (excluding NAs and values in the first two rows)
for(i in 1:528){
  row_values <- Temperature[i,3:ncol(Temperature)]
  row_values <- row_values[!is.na(row_values)]
  range <- range(row_values)
  lower_extreme[i] <- range[1]
  upper_extreme[i] <- range[2]
}
#put these ranges into a data-frame
Temp_ranges <- data.frame(Lower_Extreme = lower_extreme, Upper_Extreme = upper_extreme)

###Check the range of values in the lower and upper extremes columns
range(Temp_ranges$Lower_Extreme)
range(Temp_ranges$Upper_Extreme)


#############
###Precipitation
#############

###Which sites are represented
unique_precip_sites <- unique(Precipitation$site)
unique_precip_sites
length(unique_precip_sites)
#all 44 sites are represented

###Which years are represented
unique_precip_years <- unique(Precipitation$year)
unique_precip_years
length(unique_precip_years)
#there are 12 years of data from 2013 to 2024

###Which site-year combinations are represented
unique_precip_combinations <- unique(Precipitation[c("year", "site")])
nrow(unique_precip_combinations)
#there are 528 possible combinations of site and year

###How many NAs are there for each site/year combination
precip_na_count_per_row <- apply(Precipitation, 1, function(x) sum(is.na(x)))
precip_na_count_per_row
precip_na_count_per_row <- as.data.frame(precip_na_count_per_row)

# which unique values are there in the precip_na_count_per_row dataframe
unique(precip_na_count_per_row$precip_na_count_per_row)
#There are either 124 or 122 NAs in each row
#123 for rows corresponding to the years 2016 and 2020
#124 for all other years

###Which days are NAs for each row
precip_na_days_per_row <- apply(Precipitation, 1, function(x) names(Precipitation)[which(is.na(x))])
precip_na_days_per_row
#Rows in Precipitation corresponding to leap year 2016: 133-176
#Rows in Precipitation corresponding to leap year 2020: 309-352
#Rows in Precipitation corresponding to leap year 2024: 485-528
#On non-leap years day 366 is has NA
#On leap years 2016 and 2020 there are no NAs
#On leap year 2024 there are NAs for 184 days spanning ordinal dates 183-366
# This is expected because in leap years an extra day is added at the end of February so the ordinal dates are shifted forward by 1 day after this point (therefore non-leap years will not have a 366th day)
# Data from July-December 2024 is missing which is to be expected as the dataset was created in 2024 and we only need data up to Februrary 2024

###Check the range of values in each row

# Create vectors for lower and upper extremes
lower_extreme <- numeric(528)
upper_extreme <- numeric(528)

# Run a for loop that calculates the range of values in each row (excluding NAs and values in the first two rows)
for(i in 1:528){
  row_values <- Precipitation[i,3:ncol(Precipitation)]
  row_values <- row_values[!is.na(row_values)]
  range <- range(row_values)
  lower_extreme[i] <- range[1]
  upper_extreme[i] <- range[2]
}
#put these ranges into a data-frame
Precip_ranges <- data.frame(Lower_Extreme = lower_extreme, Upper_Extreme = upper_extreme)
#lower extreme for all rows is zero, this is expected as some days will have no rainfall

###Check the range of values in the upper extreme column
range(Precip_ranges$Upper_Extreme)

#############
###Checking to see if KCK and KCZ have the same weather data
#############

#This is because the sites are close to each other and will be treated as 1 site in the analysis (their metrics should be very similar if not identical for a given site in a given year)

###Comparing Temperature between the two sites

#subset the data so it only contains sites KCK and KCZ
temp_KCK <- subset(Temperature, site == "KCK")
temp_KCZ <- subset(Temperature, site == "KCZ")
#change all NAs in the subsetted dataframes to the number -999
temp_KCK[is.na(temp_KCK)] <- -999
temp_KCZ[is.na(temp_KCZ)] <- -999

# Compare the dataframes
comparison_result <- temp_KCK == temp_KCZ

# Check if all values are equal in all columns apart from column 1
all_equal <- all(comparison_result[,2:ncol(comparison_result)])
print(all_equal)  # Output: FALSE
# Temperatures are not equal for both sites on a given day in a given year


###Comparing Precipitation between the two sites

#subset the data so it only contains sites KCK and KCZ
precip_KCK <- subset(Precipitation, site == "KCK")
precip_KCZ <- subset(Precipitation, site == "KCZ")
#change all NAs in the subsetted dataframes to the number -999
precip_KCK[is.na(precip_KCK)] <- -999
precip_KCZ[is.na(precip_KCZ)] <- -999

# Compare the dataframes
comparison_result <- precip_KCK == precip_KCZ

# Check if all values are equal in all columns apart from column 1
all_equal <- all(comparison_result[,2:ncol(comparison_result)])
print(all_equal)  # Output: FALSE
# Precipitation is not equal for both sites on a given day in a given year
