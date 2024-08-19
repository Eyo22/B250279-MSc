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
########
###Reading in and exploring the adults data
########

# Read in the data
adults <- read.csv("C:/Users/EyoAl/Documents/Documents/Edinburgh Uni/Dissertation/Data_Processing_&_Modelling/data/Adults.csv", header = TRUE, stringsAsFactors = F)
# Have a look at the head and structure of the dataframe  
head(adults)
str(adults)

### Some corrections 

# Check the unique adult ring numbers
unique(adults$ring)

# Exclude rows where the ring number is blank or incorrect (just incase)
adults <- subset(adults, !ring%in%c('','DURING POO CHECK"'))


# Replace any blank spaces with NA
adults[adults=='' & !is.na(adults)] <- NA
# Correct mass measurements saved in time format 
adults$mass[adults$mass%in%c('10:48','11:20','11:00')] <- c('10.48','11.20','11.00')
# Change mass column from character to numeric
adults$mass <- as.numeric(adults$mass)
#Warning message:
#NAs introduced by coercion
#warning indicates there may be a character in the column that cannot be converted to a numeric value, it will be changed to NA in any case

# How many rows in the dataframe? 
nrow(adults) # 3673
# How many unique individuals (ring numbers) in the dataframe? 
length(unique(adults$ring)) # 2263


# How many observations in each season
table(adults$season)
# spring winter 
#   3263    410 
# Most captures in spring, some in winter - we want to exclude these as we are only considering spring sampling events 
# Any rows where info on season is missing? 
sum(is.na(adults$season)) # 0

# How are those captures distributed across the year? Historgram
hist(adults$date, xlim = c(1,182))
# Clear separation of spring vs winter captures


# New dataframe including only spring captures 
adults2 <- subset(adults, season=='spring')

# Reset the row indices of the new data-frame (so they are in sequential order) - just to ensure it doesn't cause any issues
row.names(adults2) <- seq_len(nrow(adults2))

nrow(adults2) #Output: 3263


# Save new data-frame excluding ring errors and winter captures as adults2 
write.csv(adults2, 'Adults2.csv', row.names=FALSE)




# If you need to read in adults 2 again
adults2 <- read.csv("C:/Users/EyoAl/Documents/Documents/Edinburgh Uni/Dissertation/Data_Processing_&_Modelling/data/Adults2.csv", header = TRUE, stringsAsFactors = F)

####################
###Manipulating adults2 to provide survival data in a suitable format for GLMM and CJS models
####################

###Deleting unecessary columns

# ensure the data is of the "data.frame" class
class(adults2)

# get the structure of the data to see which columns need deleting
str(adults2)

# subset the data by removing unecessary columns
adults3 <- subset(adults2, select = -c(box, age, wing, tarsus, mass, date, time, ringer, season, notes, catching, X))

###Ordering the data

# Order the data in such a way that all the observations of a ring ID are ordered from first to last year observed (The ring IDs themselves are in alphabetical order)
class(adults3$year)
adults4 <- adults3 %>%
  arrange(ring, year) %>%        # Sorts the dataframe first by ring, then by year
  distinct(ring, year, .keep_all = TRUE)  #remove duplicate observations of a ring in the same year (keeping data for the first observation in a given year)

# Reset the row indices of the new data-frame (so they are in sequential order) - just to ensure it doesn't cause any issues
row.names(adults4) <- seq_len(nrow(adults4))

### Check the code above is working correctly incase some observations of a given ring ID are not grouped together but scattered throughout the dataset (or throughout the part of the dataset with ring IDs with the same letter code)


# check the number of unique ring IDs in adults2, adults3 and adults4
length(unique(adults2$ring))#Output: 2112
length(unique(adults3$ring))#Output: 2112
length(unique(adults4$ring))#Output: 2112
#All the same as expected

# unique values in the history column
unique(adults4$history)# 0s and 1s only

history1 <- subset(adults4, history == 1)
nrow(history1)#1312 observations are considered recaptures
length(unique(history1$ring))#806 individuals are represented in these recaptures 

history0 <- subset(adults4, history == 0)
nrow(history0)#1911 observations are considered first captures
length(unique(history0$ring))#1910 individuals are represented in these first captures 
#An individual can only be captured for the first time once
#so the fact that there are 1 less unique ring IDs than first time captures indicates that one individual was mistakely given a history of 0 when it was recaptured

#There are 2112 unique individuals represented in adults4 but only 1910 individuals recorded as having a history of zero
#This indicates that there are 202 individuals with a history of 1 that only appear once in the dataset
#This may be something to do with within-year duplicates being removed, but it should be keeping the first row of these duplicates, for which the history value would presumably be 0 if its the first time it has ever been captured
#It could also be due to the fact that some individuals may have been caught for the first time during winter
#and there may be mistakes in the data where a bird is mistakenly assigned a 1 or a 0 in the history column 
#This is not a huge problem as the history column will not be utilised further in data manipulation

####Check the number of unique ring/year rows (in adults3) to ensure that the code above has removed the correct amount of data

# Create a new data frame with unique combinations of the two columns
unique_combinations <- unique(adults3[c("ring", "year")])

# Count the number of rows, which equals the number of unique combinations (compare them to the number of rows in the "adults3" and "adults4" dataframes)
# It should match the number of observations in adults4

nrow(adults3)
#3263
nrow(adults4)
#3223
nrow(unique_combinations)
#3223
# Indeed it does






###Making a survival column

# Make a survival column where for each row an individual is marked as surviving to the following year (1) or presumed as dying in the following year (0)


# check how many unique site codes are in adults4
length(unique(adults4$site))
#44

# check how many unique ring IDs there are in the data
length(unique(adults4$ring))
#2112

# check how many unique combinations of ring ID and site code there are in the data (this should be equal to the number of unique ring IDs if all individuals are associated with a single site [i.e. are always recaptured at the same site where they were first captured])
unique_combinations <- unique(adults4[c("ring", "site")])
nrow(unique_combinations)
#2117
# slightly higher than number of ring IDs so there are a few cases where an individual is recaptured at a different site in a different year


# create a vector that labels rows representing recaptures of an individual at a different site
Difsite <- numeric(3223)
for(i in 2:3223){ if (adults4$ring[i] == adults4$ring[i-1] && adults4$site[i] == adults4$site[i-1] | adults4$ring[i] != adults4$ring[i-1]) {
  Difsite[i] <- 0 #if the ring ID & site is the same as in the previous row OR the ring ID is not the same as in the previous row then it is not classed as a "same-individual-different-site" recapture
} else if (adults4$ring[i] == adults4$ring[i-1] && adults4$site[i] != adults4$site[i-1]) {
  Difsite[i] <- 1 #if the ring ID is the same as in the previous row but the site is different then it is classed as a "same-individual-different-site" recapture
}
}

# assign the vector Difsite to the adults4 dataframe as a new column
column_name <- "Difsite"
adults4[[column_name]] <- Difsite

# view the rows representing recaptures at different sites
result <- subset(adults4, Difsite == 1)

# Print the result
print(result)

# view rows representing all captures of individuals that were recaptured at different sites
dispersers <-c("ARD7082", "ARD7678", "AXH8209", "AXH8659", "Z632993")
# Create a logical vector using the %in% operator
condition <- adults4$ring %in% dispersers

# Subset the dataframe using the condition
result <- adults4[condition, ]

# Print the result
print(result)



# make a survival vector and assign survival status using a conditional for loop
survival <- numeric(3222)
for(i in 1:3222){ if (adults4$ring[i] == adults4$ring[i+1]) {
  survival[i] <- 1 #if the ring ID in row i is the same as in row i+1 then apparent survival is marked as 1
} else if (adults4$ring[i] != adults4$ring[i+1]) {
  survival[i] <- 0 #if the ring ID of row i is not the same as row i+1, then apparent survival is marked as 0
}
}

# assign the final row a survival value of zero (including this row in the conditional for loop would produce an NA so the for loop cannot run)
survival <- c(survival, 0)

# remove the history column
# history column is technically no longer useful as it includes history of recaptures within years (might provide insight into potential issues with the edited data but should eventually be discarded)
adults4 <- adults4[,-5]

# add the survival vector to the adults4 data frame as a new column
column_name <- "survival"
adults4[[column_name]] <- survival


###Create a new site column where KCK & KCZ sites are combined/classified as a single site
# check how many rows are classified as KCK or KCZ in the site column respectively
#KCK: 63
#KCZ: 67
tabyl(adults4[,3])

adults5 <- adults4 %>%
  mutate(site_recode = fct_recode(site,
                                  "KCK/KCZ"    =   "KCK",
                                  "KCK/KCZ"    =   "KCZ"
  ))
# Check the number of rows classified as KCK/KCZ in the site_recode column this should be 130
tabyl(adults5[,7])



###Add extra rows representing intervening years between captures of the same bird that happened more than a year apart

# create a vector that labels rows representing late recaptures (recaptures that occurred more than a year since the last capture)
Lrecapture <- numeric(3223)
for(i in 2:3223){ if (adults5$ring[i] == adults5$ring[i-1] && adults5$year[i] == (adults5$year[i-1] + 1) |adults5$ring[i] != adults5$ring[i-1]) {
  Lrecapture[i] <- 0 #if the ring ID is the same as in the previous row and the year is equal to the year in the previous row + 1 OR the ring ID is not the same as in the previous row (i.e. its the first capture of that ring ID) then it is not classed as a late recapture
} else if (adults5$ring[i] == adults5$ring[i-1] && adults5$year[i] > (adults5$year[i-1] + 1)) {
  Lrecapture[i] <- 1 #if the ring ID is the same as in the previous row but the year is greater than the year in the previous row + 1 then it is classed as a late recapture
}
}

#assign the vector Lrecapture to the adults5 dataframe as a new column
column_name <- "Lrecapture"
adults5[[column_name]] <- Lrecapture


# Create a new column (time_SLC) indicates how many years since the last capture
time_SLC <- numeric(3223)
for(i in 2:3223){ if (adults5$Lrecapture[i] == 1) {
  time_SLC[i] <- adults5$year[i] - adults5$year[i-1] #if the row represents a late recapture then the time since last capture (in years) is equal to the the year it was recaptured minus the year it was last captured
} else if (adults5$Lrecapture[i] == 0 && adults5$ring[i] == adults5$ring[i-1]) {
  time_SLC[i] <- 1 #if the row represents a recapture but doesn't represent a late recapture then the time since the last capture is 1 year by default
} else if (adults5$Lrecapture[i] == 0 && adults5$ring[i] != adults5$ring[i-1]) {
  time_SLC[i] <- 0 #if the row represents a first capture then the time since the last capture is 0 years
}
}

#assign the vector timeSLC to the adults5 dataframe as a new column
column_name <- "time_SLC"
adults5[[column_name]] <- time_SLC

##Investigating late recaptures in adults5
# view the rows representing late recaptures
result <- subset(adults5, Lrecapture == 1)

# Print the result
print(result)



# view rows representing all captures of individuals that were eventually recaptured late
Lrecapture_IDs <- unique(result[,1])
# Create a logical vector using the %in% operator
condition <- adults5$ring %in% Lrecapture_IDs

# Subset the dataframe using the condition
adults5_Lrecapture_subset <- adults5[condition, ]

# Print the result
print(adults5_Lrecapture_subset)

# see the maximum time_SLC for a late recapture
max(adults5$time_SLC)
# its 4 years

# Save adults5 as a CSV 
write.csv(adults5, 'Adults5.csv', row.names=FALSE)


#NB: For the added rows, ring: remains the same, year: changes, site: either presumed to be the same as the site of the late recapture or labelled as unknown/NA in the years not recaptured, sex: remains the same, Difsite: presumed as 0 or labelled as unknonwn or NA for all added rows, survival: always 1 for added rows, site_recode: same reasoning as for site, Lrecapture: always 0 or NA for added rows, time_SLC: calculated as usual

# Define the new row template
new_row_template <- data.frame(
  ring = NA,
  year = NA,
  site = NA,
  sex = NA,
  Difsite = NA,
  survival = NA,
  site_recode = NA,
  Lrecapture = NA,
  time_SLC = NA
)

# Initialize the modified dataframe
adults6 <- adults5

# Loop through the initial dataframe
for (i in 1:nrow(adults5)) {
  if (adults5$time_SLC[i] == 2) {
    # Define the new row with conditional data
    new_row <- new_row_template
    new_row$ring <- paste(adults5$ring[i])
    new_row$year <- adults5$year[i] - 1
    new_row$site <- NA
    new_row$sex <- adults5$sex[i]
    new_row$Difsite <- NA
    new_row$survival <- 1
    new_row$site_recode <- NA
    new_row$Lrecapture <- NA
    new_row$time_SLC <- adults5$time_SLC[i] - 1
    # Add the new row to the modified dataframe
    adults6 <- rbind(adults6, new_row)
  }
 else if (adults5$time_SLC[i] == 3) {
    # Define the new row with conditional data
    new_row <- new_row_template
    new_row$ring <- paste(adults5$ring[i])
    new_row$year <- adults5$year[i] - 1
    new_row$site <- NA
    new_row$sex <- adults5$sex[i]
    new_row$Difsite <- NA
    new_row$survival <- 1
    new_row$site_recode <- NA
    new_row$Lrecapture <- NA
    new_row$time_SLC <- adults5$time_SLC[i] - 1
    new_row2 <- new_row_template
    new_row2$ring <- paste(adults5$ring[i])
    new_row2$year <- adults5$year[i] - 2
    new_row2$site <- NA
    new_row2$sex <- adults5$sex[i]
    new_row2$Difsite <- NA
    new_row2$survival <- 1
    new_row2$site_recode <- NA
    new_row2$Lrecapture <- NA
    new_row2$time_SLC <- adults5$time_SLC[i] - 2
    # Add the new row to the modified dataframe
    adults6 <- rbind(adults6, new_row, new_row2)
  }
 else if (adults5$time_SLC[i] == 4) {
    # Define the new row with conditional data
    new_row <- new_row_template
    new_row$ring <- paste(adults5$ring[i])
    new_row$year <- adults5$year[i] - 1
    new_row$site <- NA
    new_row$sex <- adults5$sex[i]
    new_row$Difsite <- NA
    new_row$survival <- 1
    new_row$site_recode <- NA
    new_row$Lrecapture <- NA
    new_row$time_SLC <- adults5$time_SLC[i] - 1
    new_row2 <- new_row_template
    new_row2$ring <- paste(adults5$ring[i])
    new_row2$year <- adults5$year[i] - 2
    new_row2$site <- NA
    new_row2$sex <- adults5$sex[i]
    new_row2$Difsite <- NA
    new_row2$survival <- 1
    new_row2$site_recode <- NA
    new_row2$Lrecapture <- NA
    new_row2$time_SLC <- adults5$time_SLC[i] - 2
    new_row3 <- new_row_template
    new_row3$ring <- paste(adults5$ring[i])
    new_row3$year <- adults5$year[i] - 3
    new_row3$site <- NA
    new_row3$sex <- adults5$sex[i]
    new_row3$Difsite <- NA
    new_row3$survival <- 1
    new_row3$site_recode <- NA
    new_row3$Lrecapture <- NA
    new_row3$time_SLC <- adults5$time_SLC[i] - 3
    # Add the new row to the modified dataframe
    adults6 <- rbind(adults6, new_row, new_row2, new_row3)
  }
}



# reorder the dataframe using the same method you used to order the rows before
adults7 <- adults6 %>%
  arrange(ring, year) %>%        # Sorts the dataframe first by ring, then by year
  distinct(ring, year, .keep_all = TRUE)  #remove duplicate observations of a ring in the same year (keeping data for the first observation in a given year)

# Reset the row indices of the new data-frame (so they are in sequential order)
row.names(adults7) <- seq_len(nrow(adults7))
#Pretty sure they were in sequential order anyway but reset them just incase

###Confirm that the rows have been added to the adults data correctly

#there were 121 rows added between adults 5 and adults7 (3344-3223 = 121) this should be equal to the total number of years between captures for all late recaptures

# subset adults5 to include late recaptures only (i.e. rows with a time_SLC greater than 1) 
adults5_Lrecapture_subset <- subset(adults5, time_SLC > 1)

# Reset the row indices of the new data-frame (so they are in sequential order) - just to ensure it doesn't cause any issues
row.names(adults5_Lrecapture_subset) <- seq_len(nrow(adults5_Lrecapture_subset))

# make a vector (extra_rows) representing the number of rows that would need to be added to represent years when these individuals were not captured
extra_rows <- numeric(102)
for(i in 1:102){
extra_rows[i] <- adults5_Lrecapture_subset$time_SLC[i] - 1
  }

# sum this vector to get the total number of rows that should have been added 
sum(extra_rows)
#it is indeed 121 matching the number of rows that were added to make adults7

###Create a capture column (i.e. was the bird caught this year)
capture <- numeric(3344)
for(i in 1:3344){ if (is.na(adults7$Lrecapture[i])) {
  capture[i] <- 0 #if Lrecapture is NA then the row does not represent a capture
} else {
  capture[i] <- 1 #if the Lrecapture isn't NA then the row represents a capture
}
}

# assign the vector capture to the adults7 dataframe as a new column
column_name <- "capture"
adults7[[column_name]] <- capture

###Adding an age_class column to the adults7 data

# modify adults2 by sorting it by ring and year whilst removing duplicate observations of a ring in the same year
adults2_mod <- adults2 %>%
  arrange(ring, year) %>%        # Sorts the dataframe first by ring, then by year
  distinct(ring, year, .keep_all = TRUE)  #remove duplicate observations of a ring in the same year (keeping data for the first observation in a given year)

# Reset the row indices of the new data-frame (so they are in sequential order) - just to ensure it doesn't cause any issues
row.names(adults2_mod) <- seq_len(nrow(adults2_mod))
#They were probably in sequential order before but reset it just as a precaution

# make an age_class column for the adults7 dataframe
age_class <- numeric(3344)
for(i in 1:3344){ if(adults7$capture[i] == 1){
  age_class[i] <- subset(adults2_mod, ring == adults7[i,"ring"] & year == adults7[i,"year"])$age
}
else{
  age_class[i] <- NA  
}
  }
age_class

# assign the vector age_class to the adults7 dataframe as a new column
column_name <- "age_class"
adults7[[column_name]] <- age_class
#NB age_class represents the age_class assigned to the bird upon capture in the field based on visual observation and following the BTO age codes

###Adding an age estimate (age_est) column to the adult7 data


#if a bird is age = 4 on first capture then it was hatched before that calender year of ringing (1 year +) but its exact year of hatching is unknown
#if a bird is age = 5 on first capture then it hatched during the previous calendar year of ringing (1 year) so its age can be calculated for all subsequent years it was alive
#if a bird is age = 6 on first capture then it hatched before the previous calendar year (2 years +) but its exact year of hatching is unknown


# Subset the data to look at all instances of age_class 4
age_class_4 <- subset(adults7, age_class == 4)
# View rows for individuals that were assgined age class 4 upon recapture
subset(adults7, ring== "S921093")
subset(adults7, ring== "S922228")
subset(adults7, ring== "S922795")
#1 instances were the bird was a 5 the year before 1 instance where the bird was a late recapture and classed as a 5 on first capture and 1 instance were a bird was a 6 the year before

# Subset the data to look at all instances of age_class 5
age_class_5 <- subset(adults7, age_class == 5)

# Find instances where a bird has been caught a year or more previously but still classed as a 5
subset(age_class_5, time_SLC == 1)
#there is one case of this occurring

# View the capture history of this bird
subset(adults7, ring== "_APR7313")
#the bird is classed as a 5 in 2024 but was caught the previous 3 years, being classed as a 5 on first capture and classed as a 6 in both 2022 & 2023

#thus age estimates should be based on the age class assigned to the bird in the first year it was caught

# Check the unique values in the age class column
unique(adults7$age_class)#Output: 4, 5, 6 & NA
#Age_class is NA for rows where a bird is not captured

# View rows where age_class is NA for a first time capture (time_SLC = 0)
subset(adults7, is.na(age_class) & time_SLC == 0)
#There appears to be a first time capture in 2024 (ARR9688) where age class is registered as NA

# Reassign NAs as -999 were age_class is NA (to avoid problems caused by NA age classes in first time captures)
adults7$age_class[is.na(adults7$age_class)] <- -999

# Make an age estimate column (age_est) that estimates the age of a bird (but only for birds that are classed as age 5 upon first capture) 

age_est <- numeric(3344)
for(i in 1:3344){ if(subset(adults7, ring == adults7[i,"ring"])[1,"time_SLC"] == 0 && subset(adults7, ring == adults7[i,"ring"])[1,"age_class"] == 5){
  age_est[i] <- 1 + (adults7$year[i] - subset(adults7, ring == adults7[i,"ring"])[1,"year"]) #If the bird is assigned age class 5 upon first capture then the age_est assigned to that row = 1 + (the year represented in that row - the year that bird was first captured)
}
  else{
    age_est[i] <- "U" #if the bird was not age_class 5 upon first capture (i.e. 4, 6 or NA/-999) then its age is classed as unknown at all capture events
  }
}
age_est


# assign the vector age_est to the adults7 dataframe as a new column
column_name <- "age_est"
adults7[[column_name]] <- age_est

#Make an age estimate column (age_est_4eq5) that estimates the age of a bird (but only for birds that are classed as age 4 or 5 upon first capture assuming that 4s are also 1 year old) 
age_est_4eq5 <- numeric(3344)
for(i in 1:3344){ if(subset(adults7, ring == adults7[i,"ring"])[1,"time_SLC"] == 0 && subset(adults7, ring == adults7[i,"ring"])[1,"age_class"] == 5 | subset(adults7, ring == adults7[i,"ring"])[1,"time_SLC"] == 0 && subset(adults7, ring == adults7[i,"ring"])[1,"age_class"] == 4){
  age_est_4eq5[i] <- 1 + (adults7$year[i] - subset(adults7, ring == adults7[i,"ring"])[1,"year"]) #If the bird is assigned age class 4 or 5 upon first capture then the age_est assigned to that row = 1 + (the year represented in that row - the year that bird was first captured)
}
  else{
    age_est_4eq5[i] <- "U" #if the bird was not age_class 4 or 5 upon first capture (i.e. 6 or NA/-999) then its age is classed as unknown at all capture events
  }
}
age_est_4eq5


# assign the vector age_est_4eq5 to the adults7 dataframe as a new column
column_name <- "age_est_4eq5"
adults7[[column_name]] <- age_est_4eq5

#Reassign "U" as "-999" in the age_est and age_est_4eq_5 vectors and make them numeric 
adults7$age_est[adults7$age_est == "U"] <- "-999"
adults7$age_est_4eq5[adults7$age_est_4eq5 == "U"] <- "-999"

adults7$age_est <- as.numeric(adults7$age_est)
adults7$age_est_4eq5 <- as.numeric(adults7$age_est_4eq5)
class(adults7$age_est)
class(adults7$age_est_4eq5)

#Make a column that assigns each bird an age category (age_cat) using the age_est_4eq5 column

age_cat <- numeric(3344)
for(i in 1:3344){ if(adults7$age_est_4eq5[i] >= 2 | adults7$age_est_4eq5[i] == -999){
  age_cat[i] <- "O" #for older adults that are 2+ years old
}
  else{
    age_cat[i] <- "Y" #for young adults under 2 years
  }
}

# assign the vector age_cat to the adults7 dataframe as a new column
column_name <- "age_cat"
adults7[[column_name]] <- age_cat

###Make a column that represents years since first capture (Time_SFC) just for further investigation

time_SFC <- numeric(3344)

for(i in 1:3344){ 
  time_SFC[i] <- (adults7$year[i] - subset(adults7, ring == adults7[i,"ring"])[1,"year"])

}

# assign the vector time_SFC to the adults7 dataframe as a new column
column_name <- "time_SFC"
adults7[[column_name]] <- time_SFC

###Assign an appropriate site code to rows where capture = 0

# Assign the site code at which the bird was last caught to rows representing years in which that bird was not captured

for(i in 1:3344){ if(is.na(adults7$site[i])) {
  adults7$site[i] <-  subset(adults7, ring == adults7[i,"ring"])[nrow(subset(adults7, ring == adults7[i,"ring"])),"site"]
}
  
}

# Assign the site code at which the bird was last caught to rows representing years in which that bird was not captured (for the "site_recode" column)

for(i in 1:3344){ if(is.na(adults7$site_recode[i])) {
  adults7$site_recode[i] <-  subset(adults7, ring == adults7[i,"ring"])[nrow(subset(adults7, ring == adults7[i,"ring"])),"site_recode"]
}
  
}

###Put columns in a more intuitive order

adults7 <- adults7 %>%
  select(ring, sex, year, site, site_recode, Difsite, survival, capture, Lrecapture, time_SLC, time_SFC, age_class, age_est, age_est_4eq5, age_cat)

# Save adults7 as a CSV 
write.csv(adults7, 'Adults7.csv', row.names=FALSE)

### Remove rows representing the sites SPD and OSP

#check the number of rows representing OSP
OSP_subset <- subset(adults7, site == "OSP")
# 54 rows

#check the number of rows representing SPD
SPD_subset <- subset(adults7, site == "SPD")
# 47 rows

# Remove rows representing these sites
adults7 <- subset(adults7, site != "OSP")
adults7 <- subset(adults7, site != "SPD")
#Once these rows have been removed the adults7 dataframe should have 3243 rows (3344-101)

# Reset the row indices of the new data-frame (so they are in sequential order) - just to ensure it doesn't cause any issues
row.names(adults7) <- seq_len(nrow(adults7))


### Final change to adults7: Edit the site recode column to ensure dispersers are only represented by the last site they were caught at (as it is assumed this is where they spent the most time)

# read in the data-frame if you need to
adults7 <- read.csv("C:/Users/EyoAl/Documents/Documents/Edinburgh Uni/Dissertation/Data_Processing_&_Modelling/data/Adults7.csv", header = TRUE, stringsAsFactors = F)

# If the ring ID in row i is represented in multiple rows & belongs to a bird that was caught at more than one site:
# Assign the site code at which the bird was last caught to rows representing all captures of that bird (for the "site_recode" column)
for(i in 1:3243){ if(nrow(subset(adults7, ring == adults7[i,"ring"])) > 1 && length(unique(subset(adults7, ring == adults7[i,"ring"])$site_recode)) > 1) {
  adults7$site_recode[i] <-  subset(adults7, ring == adults7[i,"ring"])[nrow(subset(adults7, ring == adults7[i,"ring"])),"site_recode"]
}
  
}

# check this has worked correctly by viewing rows representing all captures of individuals that were recaptured at different sites
dispersers <-c("ARD7082", "ARD7678", "AXH8209", "AXH8659", "Z632993")
# Create a logical vector using the %in% operator
condition <- adults7$ring %in% dispersers

# Subset the dataframe using the condition
result <- adults7[condition, ]

# Print the result
print(result)

#remember to resave adults7 after this
write.csv(adults7, 'Adults7.csv', row.names=FALSE)
