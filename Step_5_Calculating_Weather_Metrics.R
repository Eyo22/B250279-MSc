
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

Occupancy_2 <- read.csv("C:/Users/EyoAl/Documents/Documents/Edinburgh Uni/Dissertation/Data_Processing_&_Modelling/data/Occupancy_2.csv", header = TRUE, stringsAsFactors = F)

###Ordinal dates for the months of interest

##December 
#Ordinal dates:
#335-365 (336-366 on leap years)

#Column indices:
#337-367 (338-368 on leap years)


##January, Februrary & March 
#Ordinal dates:
#1-90 (1-91 on leap years)

#Column indices:
#3-92 (3-93 on leap years)


###Making a Weather data-frame

# Subset Occupancy_2 to only include years up to 2023
Weather <- subset(Occupancy_2, year < 2024)

# Subset the new data-frame further to include only the columns site & year
Weather <- Weather[,1:2]

# Reset the row indices of the new data-frame (so they are in sequential order) 
row.names(Weather) <- seq_len(nrow(Weather))

##########
###Calculating mean daily minimum temperature during the winter months
##########

# Create a new vector "mean_temp" for the Weather dataframe
mean_temp <- numeric(390)

# Add mean_temp to the Weather data frame as a new column
column_name <- "mean_temp"
Weather[[column_name]] <- mean_temp

### Run a conditional for loop that calculates mean daily minimum temperature during the winter months for a given site in a given year (for all sites except for site KCK/KCZ)

for(i in 1:390){ if(Weather$year[i] != 2016 && Weather$year[i] != 2020 && Weather$year[i] != 2015 && Weather$year[i] != 2019 && Weather$year[i] != 2023 && Weather$site[i] != "KCK/KCZ"){#If year t isn't a leap year and year t+1 isn't a leap year
  # Subset temperature to extract daily minimum temperatures for December of year t
  t <- subset(Temperature, site == Weather[i,"site"] & year == Weather[i,"year"])[,337:367]
  # Subset temperature to extract daily minimum temperatures for January, February and March of year t+1
  t1 <- subset(Temperature, site == Weather[i,"site"] & year == (1 + Weather[i,"year"]))[,3:92]
  
  # Combine the data-frames column-wise to get daily minimumn temperatures for the winter season (Dec-Feb) between t and t+1
  winter_temps <- cbind(t, t1)
  
  # Convert the combined data-frame into a vector
  winter_temps_vector <- as.vector(t(as.matrix(winter_temps)))
  
  # Calculate the mean daily minimum temperature for the winter season between years t and t+1
  Weather$mean_temp[i] <- mean(winter_temps_vector)
  
} else if ((Weather$year[i] == 2016 | Weather$year[i] == 2020) && Weather$site[i] != "KCK/KCZ"){#If year t is a leap year
  # Subset temperature to extract daily minimum temperatures for December of year t
  t <- subset(Temperature, site == Weather[i,"site"] & year == Weather[i,"year"])[,338:368]
  # Subset temperature to extract daily minimum temperatures for January, February and March of year t+1
  t1 <- subset(Temperature, site == Weather[i,"site"] & year == (1 + Weather[i,"year"]))[,3:92]
  
  # Combine the data-frames column-wise to get daily minimumn temperatures for the winter season (Dec-Feb) between t and t+1
  winter_temps <- cbind(t, t1)
  
  # Convert the combined data-frame into a vector
  winter_temps_vector <- as.vector(t(as.matrix(winter_temps)))
  
  # Calculate the mean daily minimum temperature for the winter season between years t and t+1
  Weather$mean_temp[i] <- mean(winter_temps_vector)
  
} else if ((Weather$year[i] == 2015 | Weather$year[i] == 2019 | Weather$year[i] == 2023) && Weather$site[i] != "KCK/KCZ"){#If year t+1 is a leap year
  # Subset temperature to extract daily minimum temperatures for December of year t
  t <- subset(Temperature, site == Weather[i,"site"] & year == Weather[i,"year"])[,337:367]
  # Subset temperature to extract daily minimum temperatures for January, February and March of year t+1
  t1 <- subset(Temperature, site == Weather[i,"site"] & year == (1 + Weather[i,"year"]))[,3:93]
  
  # Combine the data-frames column-wise to get daily minimumn temperatures for the winter season (Dec-March) between t and t+1
  winter_temps <- cbind(t, t1)
  
  # Convert the combined data-frame into a vector
  winter_temps_vector <- as.vector(t(as.matrix(winter_temps)))
  
  # Calculate the mean daily minimum temperature for the winter season between years t and t+1
  Weather$mean_temp[i] <- mean(winter_temps_vector)
  
} else if (Weather$site[i] == "KCK/KCZ"){#Ensure sites that are KCK/KCZ are labelled with -999 (until subsequent for loops are run)
  Weather$mean_temp[i] <- -999
}
}



###Make a new data-frame (Weather0) 

Weather0 <- Weather
#This should be identical to the "Weather" data-frame
#Its purpose is to confirm that the following code only calculates values for KCK/KCZ sites and doesn't change any of the other values

# Compare the dataframes
comparison_result <- Weather == Weather0

# Check if all values are equal
all_equal <- all(comparison_result)
print(all_equal)  # Output: TRUE
# All values are equal for both dataframes thus far




### Run a conditional for loop that calculates mean daily minimum temperature during the winter months for site KCK/KCZ

for(i in 1:390){ if(Weather$year[i] != 2016 && Weather$year[i] != 2020 && Weather$year[i] != 2015 && Weather$year[i] != 2019 && Weather$year[i] != 2023 && Weather$site[i] == "KCK/KCZ"){#If year t isn't a leap year and year t+1 isn't a leap year
  # Subset Temperature to extract daily minimum temperatures for December of year t at KCK
  t_KCK <- subset(Temperature, site == "KCK" & year == Weather[i,"year"])[,337:367]
  # Subset Temperature to extract daily minimum temperatures for January, February and March of year t+1 at KCK
  t1_KCK <- subset(Temperature, site == "KCK" & year == (1 + Weather[i,"year"]))[,3:92]
  
  # Subset Temperature to extract daily minimum temperatures for December of year t at KCZ
  t_KCZ <- subset(Temperature, site == "KCZ" & year == Weather[i,"year"])[,337:367]
  # Subset Temperature to extract daily minimum temperatures for January, February and March of year t+1 at KCZ
  t1_KCZ <- subset(Temperature, site == "KCZ" & year == (1 + Weather[i,"year"]))[,3:92]
  
  # Combine the data-frames column-wise to get all daily minimumn temperatures for the winter season (Dec-March) between t and t+1 at both KCK and KCZ
  winter_temps <- cbind(t_KCK, t1_KCK, t_KCZ, t1_KCZ)
  
  # Convert the combined data-frames into a vector
  winter_temps_vector <- as.vector(t(as.matrix(winter_temps)))
  
  # Calculate the mean daily minimum temperature at KCK and KCZ for the winter season between years t and t+1
  Weather$mean_temp[i] <- mean(winter_temps_vector)
  
} else if ((Weather$year[i] == 2016 | Weather$year[i] == 2020) && Weather$site[i] == "KCK/KCZ"){#If year t is a leap year
  # Subset Temperature to extract daily minimum temperatures for December of year t at KCK
  t_KCK <- subset(Temperature, site == "KCK" & year == Weather[i,"year"])[,338:368]
  # Subset Temperature to extract daily minimum temperatures for January, February and March of year t+1 at KCK
  t1_KCK <- subset(Temperature, site == "KCK" & year == (1 + Weather[i,"year"]))[,3:92]
  
  # Subset Temperature to extract daily minimum temperatures for December of year t at KCZ
  t_KCZ <- subset(Temperature, site == "KCZ" & year == Weather[i,"year"])[,338:368]
  # Subset Temperature to extract daily minimum temperatures for January, February and March of year t+1 at KCZ
  t1_KCZ <- subset(Temperature, site == "KCZ" & year == (1 + Weather[i,"year"]))[,3:92]
  
  # Combine the data-frames column-wise to get all daily minimumn temperatures for the winter season (Dec-March) between t and t+1 at both KCK and KCZ
  winter_temps <- cbind(t_KCK, t1_KCK, t_KCZ, t1_KCZ)
  
  # Convert the combined data-frames into a vector
  winter_temps_vector <- as.vector(t(as.matrix(winter_temps)))
  
  # Calculate the mean daily minimum temperature at KCK and KCZ for the winter season between years t and t+1
  Weather$mean_temp[i] <- mean(winter_temps_vector)
  
} else if ((Weather$year[i] == 2015 | Weather$year[i] == 2019 | Weather$year[i] == 2023) && Weather$site[i] == "KCK/KCZ"){#If year t+1 is a leap year
  # Subset Temperature to extract daily minimum temperatures for December of year t at KCK
  t_KCK <- subset(Temperature, site == "KCK" & year == Weather[i,"year"])[,337:367]
  # Subset Temperature to extract daily minimum temperatures for January, February and March of year t+1 at KCK
  t1_KCK <- subset(Temperature, site == "KCK" & year == (1 + Weather[i,"year"]))[,3:93]
  
  # Subset Temperature to extract daily minimum temperatures for December of year t at KCZ
  t_KCZ <- subset(Temperature, site == "KCZ" & year == Weather[i,"year"])[,337:367]
  # Subset Temperature to extract daily minimum temperatures for January, February and March of year t+1 at KCZ
  t1_KCZ <- subset(Temperature, site == "KCZ" & year == (1 + Weather[i,"year"]))[,3:93]
  
  # Combine the data-frames column-wise to get all daily minimumn temperatures for the winter season (Dec-March) between t and t+1 at both KCK and KCZ
  winter_temps <- cbind(t_KCK, t1_KCK, t_KCZ, t1_KCZ)
  
  # Convert the combined data-frames into a vector
  winter_temps_vector <- as.vector(t(as.matrix(winter_temps)))
  
  # Calculate the mean daily minimum temperature at KCK and KCZ for the winter season between years t and t+1
  Weather$mean_temp[i] <- mean(winter_temps_vector)
}
}


# Compare the mean_temp columns of Weather and Weather0 again
comparison_result <- Weather == Weather0
#based on visual inspection of the data-frame "comparison_result" all values are equal apart from mean_temp values for rows corresponding to site KCK/KCZ (rows 258-267), which is to be expected


# ensure mean_temp represents numeric data
class(Weather$mean_temp)

# check the range of mean_temp values is realistic
range(Weather$mean_temp)#Output: -2.375455  2.998926




##########
###Calculating total precipitation during the winter months
##########

# Create a new vector "total_precip" for the Weather dataframe
total_precip <- numeric(390)

# Add mean_temp to the Weather data frame as a new column
column_name <- "total_precip"
Weather[[column_name]] <- total_precip

### Run a conditional for loop that calculates total precipitation during the winter months for a given site in a given year (for all sites except for site KCK/KCZ)

for(i in 1:390){ if(Weather$year[i] != 2016 && Weather$year[i] != 2020 && Weather$year[i] != 2015 && Weather$year[i] != 2019 && Weather$year[i] != 2023 && Weather$site[i] != "KCK/KCZ"){#If year t isn't a leap year and year t+1 isn't a leap year
  # Subset Precipitation to extract daily Precipitation for December of year t
  t <- subset(Precipitation, site == Weather[i,"site"] & year == Weather[i,"year"])[,337:367]
  # Subset Precipitation to extract daily Precipitation for January, February and March of year t+1
  t1 <- subset(Precipitation, site == Weather[i,"site"] & year == (1 + Weather[i,"year"]))[,3:92]
  
  # Combine the data-frames column-wise to get daily Precipitation for the winter season (Dec-March) between t and t+1
  winter_precip <- cbind(t, t1)
  
  # Convert the combined data-frame into a vector
  winter_precip_vector <- as.vector(t(as.matrix(winter_precip)))
  
  # Calculate the sum total precipitation for the winter season between years t and t+1
  Weather$total_precip[i] <- sum(winter_precip_vector)
  
} else if ((Weather$year[i] == 2016 | Weather$year[i] == 2020) && Weather$site[i] != "KCK/KCZ"){#If year t is a leap year
  # Subset Precipitation to extract daily Precipitation for December of year t
  t <- subset(Precipitation, site == Weather[i,"site"] & year == Weather[i,"year"])[,338:368]
  # Subset Precipitation to extract daily Precipitation for January, February and March of year t+1
  t1 <- subset(Precipitation, site == Weather[i,"site"] & year == (1 + Weather[i,"year"]))[,3:92]
  
  # Combine the data-frames column-wise to get daily Precipitation for the winter season (Dec-March) between t and t+1
  winter_precip <- cbind(t, t1)
  
  # Convert the combined data-frame into a vector
  winter_precip_vector <- as.vector(t(as.matrix(winter_precip)))
  
  # Calculate the sum total precipitation for the winter season between years t and t+1
  Weather$total_precip[i] <- sum(winter_precip_vector)
  
} else if ((Weather$year[i] == 2015 | Weather$year[i] == 2019 | Weather$year[i] == 2023) && Weather$site[i] != "KCK/KCZ"){#If year t+1 is a leap year
  # Subset Precipitation to extract daily Precipitation for December of year t
  t <- subset(Precipitation, site == Weather[i,"site"] & year == Weather[i,"year"])[,337:367]
  # Subset Precipitation to extract daily Precipitation for January, February and March of year t+1
  t1 <- subset(Precipitation, site == Weather[i,"site"] & year == (1 + Weather[i,"year"]))[,3:93]
  
  # Combine the data-frames column-wise to get daily Precipitation for the winter season (Dec-March) between t and t+1
  winter_precip <- cbind(t, t1)
  
  # Convert the combined data-frame into a vector
  winter_precip_vector <- as.vector(t(as.matrix(winter_precip)))
  
  # Calculate the sum total precipitation for the winter season between years t and t+1
  Weather$total_precip[i] <- sum(winter_precip_vector)
  
} else if (Weather$site[i] == "KCK/KCZ"){#Ensure sites that are KCK/KCZ are labelled with -999 (until subsequent for loops are run)
  Weather$total_precip[i] <- -999
}
}

###Redefine weather(0)
Weather0 <- Weather
#This should be identical to the "Weather" data-frame
#Its purpose is to confirm that the following code only calculates values for KCK/KCZ sites and doesn't change any of the other values

# Compare the dataframes
comparison_result <- Weather == Weather0

# Check if all values are equal
all_equal <- all(comparison_result)
print(all_equal)  # Output: TRUE
# All values are equal for both dataframes thus far

### Run a conditional for loop that calculates total precipitation during the winter months for site KCK/KCZ


for(i in 1:390){ if(Weather$year[i] != 2016 && Weather$year[i] != 2020 && Weather$year[i] != 2015 && Weather$year[i] != 2019 && Weather$year[i] != 2023 && Weather$site[i] == "KCK/KCZ"){#If year t isn't a leap year and year t+1 isn't a leap year
  # Subset Precipitation to extract daily Precipitation for December of year t at KCK
  t_KCK <- subset(Precipitation, site == "KCK" & year == Weather[i,"year"])[,337:367]
  # Subset Precipitation to extract daily Precipitation for January, February and March of year t+1 at KCK
  t1_KCK <- subset(Precipitation, site == "KCK" & year == (1 + Weather[i,"year"]))[,3:92]
  
  # Subset Precipitation to extract daily Precipitation for December of year t at KCZ
  t_KCZ <- subset(Precipitation, site == "KCZ" & year == Weather[i,"year"])[,337:367]
  # Subset Precipitation to extract daily Precipitation for January, February and March of year t+1 at KCZ
  t1_KCZ <- subset(Precipitation, site == "KCZ" & year == (1 + Weather[i,"year"]))[,3:92]
  
  # Combine the data-frames column-wise to get daily Precipitation for the winter season (Dec-March) between t and t+1 at both KCK & KCZ
  winter_precip <- cbind(t_KCK, t1_KCK, t_KCZ, t1_KCZ)
  
  # Convert the combined data-frames into a vector
  winter_precip_vector <- as.vector(t(as.matrix(winter_precip)))
  
  # Calculate the sum total precipitation for the winter season between years t and t+1 (divided by 2 to get a mean across KCK and KCZ)
  Weather$total_precip[i] <- (sum(winter_precip_vector))/2
  
} else if ((Weather$year[i] == 2016 | Weather$year[i] == 2020) && Weather$site[i] == "KCK/KCZ"){#If year t is a leap year
  # Subset Precipitation to extract daily Precipitation for December of year t at KCK
  t_KCK <- subset(Precipitation, site == "KCK" & year == Weather[i,"year"])[,338:368]
  # Subset Precipitation to extract daily Precipitation for January, February and March of year t+1 at KCK
  t1_KCK <- subset(Precipitation, site == "KCK" & year == (1 + Weather[i,"year"]))[,3:92]
  
  # Subset Precipitation to extract daily Precipitation for December of year t at KCZ
  t_KCZ <- subset(Precipitation, site == "KCZ" & year == Weather[i,"year"])[,338:368]
  # Subset Precipitation to extract daily Precipitation for January, February and March of year t+1 at KCZ
  t1_KCZ <- subset(Precipitation, site == "KCZ" & year == (1 + Weather[i,"year"]))[,3:92]
  
  # Combine the data-frames column-wise to get daily Precipitation for the winter season (Dec-March) between t and t+1 at both KCK & KCZ
  winter_precip <- cbind(t_KCK, t1_KCK, t_KCZ, t1_KCZ)
  
  # Convert the combined data-frames into a vector
  winter_precip_vector <- as.vector(t(as.matrix(winter_precip)))
  
  # Calculate the sum total precipitation for the winter season between years t and t+1 (divided by 2 to get a mean across KCK and KCZ)
  Weather$total_precip[i] <- (sum(winter_precip_vector))/2
  
} else if ((Weather$year[i] == 2015 | Weather$year[i] == 2019 | Weather$year[i] == 2023) && Weather$site[i] == "KCK/KCZ"){#If year t+1 is a leap year
  # Subset Precipitation to extract daily Precipitation for December of year t at KCK
  t_KCK <- subset(Precipitation, site == "KCK" & year == Weather[i,"year"])[,337:367]
  # Subset Precipitation to extract daily Precipitation for January, February and March of year t+1 at KCK
  t1_KCK <- subset(Precipitation, site == "KCK" & year == (1 + Weather[i,"year"]))[,3:93]
  
  # Subset Precipitation to extract daily Precipitation for December of year t at KCZ
  t_KCZ <- subset(Precipitation, site == "KCZ" & year == Weather[i,"year"])[,337:367]
  # Subset Precipitation to extract daily Precipitation for January, February and March of year t+1 at KCZ
  t1_KCZ <- subset(Precipitation, site == "KCZ" & year == (1 + Weather[i,"year"]))[,3:93]
  
  # Combine the data-frames column-wise to get daily Precipitation for the winter season (Dec-March) between t and t+1 at both KCK & KCZ
  winter_precip <- cbind(t_KCK, t1_KCK, t_KCZ, t1_KCZ)
  
  # Convert the combined data-frames into a vector
  winter_precip_vector <- as.vector(t(as.matrix(winter_precip)))
  
  # Calculate the sum total precipitation for the winter season between years t and t+1 (divided by 2 to get a mean across KCK and KCZ)
  Weather$total_precip[i] <- (sum(winter_precip_vector))/2
}
}


# Compare Weather and Weather0 again
comparison_result <- Weather == Weather0
#based on visual inspection of the data-frame "comparison_result" all values are equal apart from total_precip values for rows corresponding to site KCK/KCZ (rows 258-267), which is to be expected


# ensure total_precip represents numeric data
class(Weather$total_precip)

# check the range and mean of total_precip values is realistic
range(Weather$total_precip)#Output: 159.09 1061.41
mean(Weather$total_precip)#Output: 381.1308

##########
###Calculating mean (across years) and mean centered Weather metrics
##########

### Mean + mean centred temperature columns

## Make a new column (mean_mean_temp) that represents the mean of mean daily minimum winter temperatures (mean_temp) for a given site across years
mean_mean_temp <- numeric(390)
for(i in 1:390){
  mean_mean_temp[i] <-  mean(subset(Weather, site == Weather[i,"site"])$mean_temp)
}

mean_mean_temp

# assign the vector mean_mean_temp to the Weather dataframe as a new column
column_name <- "mean_mean_temp"
Weather[[column_name]] <- mean_mean_temp

## Make a new column (rel_mean_temp) that represents the mean daily minimum winter temperature relative to the mean of this metric across years for a given site (i.e. mean_temp relative to mean_mean_temp)
rel_mean_temp <- numeric(390)
for(i in 1:390){
  rel_mean_temp[i] <-  Weather$mean_temp[i] - Weather$mean_mean_temp[i]
}

rel_mean_temp

# assign the vector rel_mean_temp to the Weather dataframe as a new column
column_name <- "rel_mean_temp"
Weather[[column_name]] <- rel_mean_temp



### Mean + mean centred precipitation columns

## Make a new column (mean_total_precip) that represents the mean of total winter precipitation (total_precip) for a given site across years
mean_total_precip <- numeric(390)
for(i in 1:390){
  mean_total_precip[i] <-  mean(subset(Weather, site == Weather[i,"site"])$total_precip)
}

mean_total_precip

# assign the vector mean_total_precip to the Weather dataframe as a new column
column_name <- "mean_total_precip"
Weather[[column_name]] <- mean_total_precip

## Make a new column (rel_total_precip) that represents the total winter precipitation relative to the mean of this metric across years for a given site (i.e. total_precip relative to mean_total_precip)
rel_total_precip <- numeric(390)
for(i in 1:390){
  rel_total_precip[i] <-  Weather$total_precip[i] - Weather$mean_total_precip[i]
}

rel_total_precip

# assign the vector rel_total_precip to the Weather dataframe as a new column
column_name <- "rel_total_precip"
Weather[[column_name]] <- rel_total_precip


# Incase you already saved the Weather data and want to read it in again
Weather <- read.csv("C:/Users/EyoAl/Documents/Documents/Edinburgh Uni/Dissertation/Data_Processing_&_Modelling/data/Weather.csv", header = TRUE, stringsAsFactors = F)

# Save Weather as a CSV 
write.csv(Weather, 'Weather.csv', row.names=FALSE)
