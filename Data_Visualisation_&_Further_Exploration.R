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
Occupancy <- read.csv("C:/Users/EyoAl/Documents/Documents/Edinburgh Uni/Dissertation/Data_Processing_&_Modelling/data/Occupancy.csv", header = TRUE, stringsAsFactors = F)
Occupancy_2 <- read.csv("C:/Users/EyoAl/Documents/Documents/Edinburgh Uni/Dissertation/Data_Processing_&_Modelling/data/Occupancy_2.csv", header = TRUE, stringsAsFactors = F)
nestlings <- read.csv("C:/Users/EyoAl/Documents/Documents/Edinburgh Uni/Dissertation/Data_Processing_&_Modelling/data/Nestlings.csv", header = TRUE, stringsAsFactors = F)

#########
###Exploring adults7
#########
###Birds

# How many individuals are represented in the dataset adults7
unique_rings <- unique(adults7$ring)
unique_rings
length(unique_rings)
#There are 2035 unique individuals

# How many are males
males <- subset(adults7, sex == "M")
unique_males <- unique(males$ring)
length(unique_males)
# There are 922 males so there should be 1113 females

# Confirm the number of females
females <- subset(adults7, sex == "F")
unique_females <- unique(females$ring)
length(unique_females)
#1113 (54.7%) of the individuals are indeed female
#so either females have a higher capture probability or there are more females than males (the latter is less likely since most birds form heterosexual monogomous pairs)


# could look at the relationship between sex/year/site and estimated age or age category
# could investigate whether different sites have different sex ratios
# could investigate whether different years have different sex ratios

###Investigating birds with accurate age estimates

# Subset the data to include only birds with a known age in the "age_est_4eq5" column
adults_known_age <- subset(adults7, age_est_4eq5 != -999)
#NB: this column assumes that birds that are classed as 4s as well as 5s upon first capture are 1 year old and calculates their ages for all subsequent recaptures

# Subset the data to include only birds with a known age in the "age_est" column
adults_known_age_5s_only <- subset(adults7, age_est != -999)
#NB: this column assumes that birds that are 5s upon first capture are 1 year old and calculates their ages for all subsequent recaptures


# what percentage of individuals have accurate age estimates based on the "age_est" column
unique_aged <- unique(adults_known_age_5s_only$ring)
length(unique_aged) #=1146
(1146/1853)*100 #= %61.8 of the individuals could be aged accurately using this method 
# what percentage of individuals have accurate age estimates based on the "age_est_4eq5" column
unique_aged <- unique(adults_known_age$ring)
length(unique_aged) #=1156
(1156/1853)*100 #= %62.4 of the individuals could be aged accurately using this method

###Relationship between sex and age

# make a "lifespan" vector consisting of the last recorded age of each individual with a known age estimate
lifespan <- numeric(1156)
for(i in 1:1156){
lifespan[i] <- subset(adults7, ring == unique_aged[i])[nrow(subset(adults7, ring == unique_aged[i])),"age_est_4eq5"]
}
range(lifespan)

# make a "sex" vector consisting of the sex of each individual with a known age estimate
sex <- numeric(1156)
for(i in 1:1156){
  sex[i] <- subset(adults7, ring == unique_aged[i])[1,"sex"]
}
unique(sex)

# make "unique_aged" a dataframe and add the "lifespan" and "sex" vectors to it   
unique_aged <- as.data.frame(unique_aged)

# assign the vector sex to the unique_aged dataframe as a new column
column_name <- "sex"
unique_aged[[column_name]] <- sex

# assign the vector lifespan to the unique_aged dataframe as a new column
column_name <- "lifespan"
unique_aged[[column_name]] <- lifespan

# make a scatterplot representing the relationship between sex and lifespan
#PICK UP WHERE YOU LEFT OFF: ^^


### Investigating the number of birds ringed as nestlings that were recovered as adults

# Subset nestlings to exclude 2024 data
nestlings <- subset(nestlings, year != 2024)

unique(nestlings$year)

# Find matching codes
matching_codes <- intersect(adults7$ring, nestlings$ring)

# Display matching codes
print(matching_codes)
#105 birds recovered

# How many individuals are represented in the dataset nestlings (up to 2023)
unique_rings <- unique(nestlings$ring)
length(unique_rings)
#so only 105 of the 12070 birds ringed as nestlings up to 2023 were recovered as adults (up to 2024) 


#########
###Exploring blue_tit_survival
#########

blue_tit_survival <- read.csv("C:/Users/EyoAl/Documents/Documents/Edinburgh Uni/Dissertation/Data_Processing_&_Modelling/data/blue_tit_survival.csv", header = TRUE, stringsAsFactors = F)

### Make a new column called site_year and add it to the blue tit survival data-frame 
blue_tit_survival$site_year <- paste(blue_tit_survival$site_recode, blue_tit_survival$year,  sep='_')

# How many individuals are represented in the dataset blue_tit_survival
unique_rings <- unique(blue_tit_survival$ring)
unique_rings
length(unique_rings)
#There are 1853 unique individuals

# How many are males
males <- subset(blue_tit_survival, sex == "M")
unique_males <- unique(males$ring)
length(unique_males)
# There are 842 males so there should be 1011 females

# Confirm the number of females
females <- subset(blue_tit_survival, sex == "F")
unique_females <- unique(females$ring)
length(unique_females)
#1011 (54.6%) of the individuals are indeed female
#so either females have a higher capture probability or there are more females than males (the latter is less likely since most birds form heterosexual monogomous pairs)

# Visualise representation of sites
ggplot(blue_tit_survival) + 
  geom_bar(aes(site_recode)) + 
  labs(x = "site")

# Visualise representation of years
ggplot(blue_tit_survival) + 
  geom_bar(aes(year)) + 
  labs(x = "year")

# Visualise representation of site_year combinations
ggplot(blue_tit_survival) + 
  geom_bar(aes(site_year)) + 
  labs(x = "site_year")

#########
###Exploring 2021/2022 Weather data (including wind)
#########

### Read in the data
Temperature <- read.table("C:/Users/EyoAl/Documents/Documents/Edinburgh Uni/Dissertation/Data_Processing_&_Modelling/data/daily_mintemp.txt", header=T, sep="\t")
head(Temperature)
#Rows represent site/year combinations
#Columns 3-368 represent days of the year

Wind <- read.table("C:/Users/EyoAl/Documents/Documents/Edinburgh Uni/Dissertation/Data_Processing_&_Modelling/data/monthly_wind.txt", header=T, sep="\t")
head(Wind)
#Rows represent site/year combinations
#Columns 3-14 represent months of the year


Precipitation <- read.table("C:/Users/EyoAl/Documents/Documents/Edinburgh Uni/Dissertation/Data_Processing_&_Modelling/data/daily_precipitation.txt", header=T, sep="\t")
head(Precipitation)
#Rows represent site/year combinations
#Columns 3-368 represent days of the year
#For leap years there are 366 columns (to account for the extra day)

Weather <- read.csv("C:/Users/EyoAl/Documents/Documents/Edinburgh Uni/Dissertation/Data_Processing_&_Modelling/data/Weather_2021.csv", header = TRUE, stringsAsFactors = F)

site_details <- read.csv("C:/Users/EyoAl/Documents/Documents/Edinburgh Uni/Dissertation/Data_Processing_&_Modelling/data/site_details.csv", header = TRUE, stringsAsFactors = F)

###Plotting daily minimum temperature as a function of ordinal date

# Subset the data to include only columns up to 183 (to get data from January to approximately the end of June)
Temp_Jan_June <- Temperature[,3:183]


# Create a dataframe for mean Jan-June temperatures
Day <- c(1:181)
temp <- numeric(181)

mean_Jan_June <- data.frame(Day = Day, temp = temp)
# Calculate the mean (across all sites and years) minimum temperatures for each day from January through to the end of June and add it to the data-frame
for(i in 1:181){
mean_Jan_June$temp[i] <- mean(Temp_Jan_June[,i])
  }

# Plot mean minimum daily temperature as a function of ordinal date (from January through to the end of June)
plot(mean_Jan_June$Day,mean_Jan_June$temp)

#plot indicates that temperatures increase from January to June (as expected)
#this indicates that the raw data is behaving as expected

###Investigating mean daily minimum winter temperature as a function of latitude and elevation

# Order sites from South to North in site_details data-frame (should already be ordered this way in the data-frame)
site_details <- site_details %>%
  arrange(Mean.Lat)

# Rename KCK and KCZ in site_details so they are coded the same as in the Weather & blue_tit_survival_weather data-frames
site_details$site[site_details$site == "KCK"] <- "KCK/KCZ"
site_details$site[site_details$site == "KCZ"] <- "KCK/KCZ"


 
# Remove rows representing sites OSP and SPD
site_details <- subset(site_details, site != "OSP")
site_details <- subset(site_details, site != "SPD")

# Reset the row indices of the data-frame (to ensure they are in sequential order)
row.names(site_details) <- seq_len(nrow(site_details))



# Create a new vector "mean_mean_temp" for the site_details data-frame
mean_mean_temp <- numeric(42)

# Create a for loop that calculates the mean of mean daily minimum temperature during the winter months across years for a given site (i.e. mean_mean_temp = mean of "Weather$mean_temp" values for a given site across all years the site was active)
for(i in 1:42){
  mean_mean_temp[i] <- mean(subset(Weather, site == site_details[i,"site"])[,"mean_temp"])
}

# add the mean_mean_temp vector to the site_details data-frame as a new column
column_name <- "mean_mean_temp"
site_details[[column_name]] <- mean_mean_temp

# Plot mean_mean_temp as a function of site latitude
plot(site_details$Mean.Lat,site_details$mean_mean_temp)
#This plot shows that temperature decreases as latitude increases until the central sites (latitude= ~57 degrees) beyond which it increases again
#This likely reflects the fact that elevation has a greater effect on temperature than latitude (the central sites around the Cairngorms have highest elevation)

# Plot mean_mean_temp as a function of site elevation
plot(site_details$Mean.Elev,site_details$mean_mean_temp)
#This plot is consistent with the assumption that high elevation sites will experience lower temperatures on average
#These plots indicate that the Weather and blue_tit_survival_weather data-frames were coded correctly and that the mean daily minimum winter temperatures in their respective mean_temp columns are reflective of reality


###Plotting monthly wind-speed as a function of month

# Subset the data to include only columns representing wind-speeds
Wind_speeds <- Wind[,3:14]


# Create a data-frame for mean monthly wind-speeds
month <- c(1:12)
wind <- numeric(12)

mean_monthly_wind <- data.frame(month = month, wind = wind)

# Calculate the mean (across all sites and years) wind-speed for each month from January (month 1) to December (month 12) and add it to the data-frame
for(i in 1:12){
  mean_monthly_wind$wind[i] <- mean(Wind_speeds[,i])
}

# Plot mean monthly wind-speed as a function of month of the year, from January (month 1) to December (month 12)
plot(mean_monthly_wind$month,mean_monthly_wind$wind)

#plot indicates that wind-speeds are are highest during the winter months (especially February) and lowest in late summer (this is not surprising)
#this indicates that the raw data is behaving as expected

###Investigating mean monthly winter wind-speed as a function of latitude and elevation


# Create a new vector "winter_wind" for the site_details data-frame
winter_wind <- numeric(42)

# Create a for loop that calculates the mean monthly wind-speed during the winter months across years for a given site (i.e. winter_wind = mean of "Weather$monthly_wind" values for a given site across all years the site was active)
for(i in 1:42){
  winter_wind[i] <- mean(subset(Weather, site == site_details[i,"site"])[,"monthly_wind"])
}

# add the winter_wind vector to the site_details data-frame as a new column
column_name <- "winter_wind"
site_details[[column_name]] <- winter_wind

# Plot winter_wind as a function of site latitude
plot(site_details$Mean.Lat,site_details$winter_wind)
#This plot indicates that winter_wind has no consistent relationship with latitude (although some of the highest winter wind-speeds come from more southerly sites)


# Plot winter_wind as a function of site elevation
plot(site_details$Mean.Elev,site_details$winter_wind)
#This plot indicates that winter_wind has no consistent relationship with elevation

#These results are not unexpected, as many topographic factors can affect wind-speeds detected at a site (especially given the fact that this data is extrapolated from weather stations to represent a 1kmx1km grid square in which the site is located)


###Plotting daily precipitation as a function of ordinal date

# Subset the data to include only columns up to 183 (to get data from January to approximately the end of June)
Precip_Jan_June <- Precipitation[,3:183]


# Create a dataframe for mean Jan-June precipitation
Day <- c(1:181)
precip <- numeric(181)

mean_Jan_June <- data.frame(Day = Day, precip = precip)
# Calculate the mean (across all sites and years) precipitation for each day from January through to the end of June and add it to the data-frame
for(i in 1:181){
  mean_Jan_June$precip[i] <- mean(Precip_Jan_June[,i])
}

# Plot mean daily precipitation as a function of ordinal date (from January through to the end of June)
plot(mean_Jan_June$Day,mean_Jan_June$precip)

#plot doesn't indicate any obvious change in precipitation throughout the year although it could be interpreted as being slightly higher in January and February
#this is not unexpected as it can rain a lot throughout the year in Scotland


###Investigating mean total winter precipitation as a function of latitude and elevation


# Create a new vector "mean_total_precip" for the site_details data-frame
mean_total_precip <- numeric(42)

# Create a for loop that calculates the mean total winter precipitation across years for a given site (i.e. mean_total_precip = mean of "Weather$total_precip" values for a given site across all years the site was active)
for(i in 1:42){
  mean_total_precip[i] <- mean(subset(Weather, site == site_details[i,"site"])[,"total_precip"])
}

# add the mean_total_precip vector to the site_details data-frame as a new column
column_name <- "mean_total_precip"
site_details[[column_name]] <- mean_total_precip

# Plot mean_total_precip as a function of site latitude
plot(site_details$Mean.Lat,site_details$mean_total_precip)
#There is no consistent relationship between site latitude and winter precipitation
#Although some mid-latitude sites have very high precipitation values

# Find the mean of mean_total_precip across sites
mean(site_details$mean_total_precip)#Output: 307.2883

# Identify the 3 mid-latitude sites with especially high winter precipitation
subset(site_details, mean_total_precip > 400)
#These are DNC, DNS and DLW which are located in valleys and are known for their high rainfall

# Plot mean_total_precip as a function of site elevation
plot(site_details$Mean.Elev,site_details$mean_total_precip)
#Interestingly the plot indicates that higher elevation sites tend to have higher winter precipitation
#This could reflect the fact that higher elevation sites are situated in valleys in highland reagions 
#Thus these regions are prone to high levels of orographic precipitation 

#These plots indicate that the Weather and blue_tit_survival_weather data-frames were coded correctly and that total winter precipitation values in their respective total_precip columns are likely reflective of reality

#########
###Exploring site interpolated 2023/2024 Weather data
#########

### Read in the data
Temperature <- read.table("C:/Users/EyoAl/Documents/Documents/Edinburgh Uni/Dissertation/Data_Processing_&_Modelling/data/daily_mintemp_Hollis.txt", header=T, sep="\t")
head(Temperature)
#Rows represent site/year combinations
#Columns 3-368 represent days of the year


Precipitation <- read.table("C:/Users/EyoAl/Documents/Documents/Edinburgh Uni/Dissertation/Data_Processing_&_Modelling/data/daily_precipitation_Hollis.txt", header=T, sep="\t")
head(Precipitation)
#Rows represent site/year combinations
#Columns 3-368 represent days of the year

Weather <- read.csv("C:/Users/EyoAl/Documents/Documents/Edinburgh Uni/Dissertation/Data_Processing_&_Modelling/data/Weather.csv", header = TRUE, stringsAsFactors = F)

site_details <- read.csv("C:/Users/EyoAl/Documents/Documents/Edinburgh Uni/Dissertation/Data_Processing_&_Modelling/data/site_details.csv", header = TRUE, stringsAsFactors = F)

###Plotting daily minimum temperature as a function of ordinal date

# Subset the data to include only columns up to 182 (to get data from January to approximately the end of June)
Temp_Jan_June <- Temperature[,3:182]


# Create a dataframe for mean Jan-June temperatures
Day <- c(1:180)
temp <- numeric(180)

mean_Jan_June <- data.frame(Day = Day, temp = temp)
# Calculate the mean (across all sites and years) minimum temperatures for each day from January through to the end of June and add it to the data-frame
for(i in 1:180){
  mean_Jan_June$temp[i] <- mean(Temp_Jan_June[,i])
}

# Plot mean minimum daily temperature as a function of ordinal date (from January through to the end of June)
plot(mean_Jan_June$Day,mean_Jan_June$temp)

#plot indicates that temperatures increase from January to June (as expected)
#this indicates that the raw data is behaving as expected

###Investigating mean daily minimum winter temperature as a function of latitude and elevation

# Order sites from South to North in site_details data-frame (should already be ordered this way in the data-frame)
site_details <- site_details %>%
  arrange(Mean.Lat)

# Rename KCK and KCZ in site_details so they are coded the same as in the Weather & blue_tit_survival_weather data-frames
site_details$site[site_details$site == "KCK"] <- "KCK/KCZ"
site_details$site[site_details$site == "KCZ"] <- "KCK/KCZ"



# Remove rows representing sites OSP and SPD
site_details <- subset(site_details, site != "OSP")
site_details <- subset(site_details, site != "SPD")

# Reset the row indices of the data-frame (to ensure they are in sequential order)
row.names(site_details) <- seq_len(nrow(site_details))



# Create a new vector "mean_mean_temp" for the site_details data-frame
mean_mean_temp <- numeric(42)

# Create a for loop that calculates the mean of mean daily minimum temperature during the winter months across years for a given site (i.e. mean_mean_temp = mean of "Weather$mean_temp" values for a given site across all years the site was active)
for(i in 1:42){
  mean_mean_temp[i] <- mean(subset(Weather, site == site_details[i,"site"])[,"mean_temp"])
}

# add the mean_mean_temp vector to the site_details data-frame as a new column
column_name <- "mean_mean_temp"
site_details[[column_name]] <- mean_mean_temp

# Plot mean_mean_temp as a function of site latitude
plot(site_details$Mean.Lat,site_details$mean_mean_temp)
#This plot shows that temperature decreases as latitude increases until the central sites (latitude= ~57 degrees) beyond which it increases again
#This likely reflects the fact that elevation has a greater effect on temperature than latitude (the central sites around the Cairngorms have highest elevation)

# Plot mean_mean_temp as a function of site elevation
plot(site_details$Mean.Elev,site_details$mean_mean_temp,ylab = "Mean Daily Minimum Winter Temperature(\u00B0C)", xlab = "Mean Site Elevation above sea level(m)")
#This plot is consistent with the assumption that high elevation sites will experience lower temperatures on average
#These plots indicate that the Weather and blue_tit_survival_weather data-frames were coded correctly and that the mean daily minimum winter temperatures in their respective mean_temp columns are reflective of reality

###Plotting mean daily minimum winter temperature as a function of year

# Make a boxplot of mean daily minimum winter temperature as a function of year
boxplot(Weather$mean_temp~as.factor(Weather$year))
#can fluctuate considerably among years although there is alot of overlap in ranges (2016, 2019, 2021 & 2023 were the warmest years whilst 2014, 2017 & 2020 were some of the coldest)
#The fact that the winter of 2017 (Dec 2017 to March 2018) was particularly cold isn't surprising considering the impact of the beast from the east



###Plotting daily precipitation as a function of ordinal date

# Subset the data to include only columns up to 182 (to get data from January to approximately the end of June)
Precip_Jan_June <- Precipitation[,3:182]


# Create a dataframe for mean Jan-June precipitation
Day <- c(1:180)
precip <- numeric(180)

mean_Jan_June <- data.frame(Day = Day, precip = precip)
# Calculate the mean (across all sites and years) precipitation for each day from January through to the end of June and add it to the data-frame
for(i in 1:180){
  mean_Jan_June$precip[i] <- mean(Precip_Jan_June[,i])
}

# Plot mean daily precipitation as a function of ordinal date (from January through to the end of June)
plot(mean_Jan_June$Day,mean_Jan_June$precip)
#plot doesn't indicate any obvious change in precipitation throughout the year although it could be interpreted as being slightly higher in January and February
#this is not unexpected as it can rain a lot throughout the year in Scotland


###Investigating mean total winter precipitation as a function of latitude and elevation


# Create a new vector "mean_total_precip" for the site_details data-frame
mean_site_precip <- numeric(42)

# Create a for loop that calculates the mean total winter precipitation across years for a given site (i.e. mean_site_precip = mean of "Weather$total_precip" values for a given site across all years the site was active)
for(i in 1:42){
  mean_site_precip[i] <- mean(subset(Weather, site == site_details[i,"site"])[,"total_precip"])
}

# add the mean_site_precip vector to the site_details data-frame as a new column
column_name <- "mean_site_precip"
site_details[[column_name]] <- mean_site_precip

# Plot mean_site_precip as a function of site latitude
plot(site_details$Mean.Lat,site_details$mean_site_precip)
#There is no consistent relationship between site latitude and winter precipitation
#Although some mid-latitude sites have very high precipitation values

# Find the mean of mean_site_precip across sites
mean(site_details$mean_site_precip)#Output: 380.0113

# Identify the 3 mid-latitude sites with especially high winter precipitation
subset(site_details, mean_site_precip > 500)
#These are DNC, DNS and DLW which are located in valleys and are known for their high rainfall

# Plot mean_site_precip as a function of site elevation
plot(site_details$Mean.Elev,site_details$mean_site_precip, ylab = "Mean Total Winter Precipitation (mm)", xlab = "Mean Site Elevation above sea level(m)")
#Interestingly the plot indicates that higher elevation sites tend to have higher winter precipitation
#This could reflect the fact that higher elevation sites are situated in valleys in highland reagions 
#Thus these regions are prone to high levels of orographic precipitation 

#These plots indicate that the Weather and blue_tit_survival_weather data-frames were coded correctly and that total winter precipitation values in their respective total_precip columns are likely reflective of reality

###Plotting total winter precipitation as a function of year

# Make a boxplot of total winter precipitation as a function of year
boxplot(Weather$total_precip~as.factor(Weather$year))
#can fluctuate widely among years although there is alot of overlap in ranges (2015 & 2019 seem to have particularly high precipitation with some sites having extremely high precipitation values in these years)


