######
#This is effectively the same as the previous version except it treats KCK and KCZ as the same site
######

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
Bird_Phenology <- read.csv("C:/Users/EyoAl/Documents/Documents/Edinburgh Uni/Dissertation/Data_Processing_&_Modelling/data/Bird_Phenology.csv", header = TRUE, stringsAsFactors = F)
head(Bird_Phenology)
unique(Bird_Phenology$year)

###Combine KCK & KCZ into a single site (don't need to run this code if you want to test treat them as seperate sites)

# check how many rows are classified as KCK or KCZ in the site column respectively
tabyl(Bird_Phenology[,2])
#KCK: 82
#KCZ: 76


# Renumber the boxes for KCZ so that box numbers are not repeated when the two sites are combined
for(i in 1:3275){ if(Bird_Phenology$site[i] == "KCZ") {
  Bird_Phenology$box[i] <-  Bird_Phenology$box[i] + 8
}
  
}

# Rename site in rows where site = KCK or KCZ to "KCK/KCZ" 
Bird_Phenology$site[Bird_Phenology$site == "KCK"] <- "KCK/KCZ"
Bird_Phenology$site[Bird_Phenology$site == "KCZ"] <- "KCK/KCZ"



# Check the number of rows classified as KCK/KCZ in the site_recode column this should be 158
tabyl(Bird_Phenology[,2])



### Exploring and Cleaning Bird_Phenology

# Remove unecessary columns
#columns to keep: year, site, box, species, parents, cs, suc
Bird_Phenology1 <- subset(Bird_Phenology, select = -c(male, female, moss, n1, nl, latestfed, latestcc, fed, cc, lastnotinc, fki, clutch.swap.treatment, extra.eggs, hd_1.45, hatching_first_recorded, hatching, number.hatched, weight, av.weight, time.of.day.for.hatch.weight, uhe, v1date, v1alive, v1poo, v1time, v1duration, v2date, v2alive, v2poo, v2time, v2duration, dfl, X2brood, X2suc, worms.notes, btworms, distworms, UHE.on.removal, notes, relay, ncs, eggbroke, eggcheck, helper, visit.frequency, pine.marten..or.other.predation.))

# how many relays/second clutches are represented in the data (i.e. how many instances where a year-site-box combination is duplicated)
unique_box_year_site <- unique(Bird_Phenology1[c("year", "site", "box")])
nrow(unique_box_year_site)
# there are 3269 unique combinations of year, site & box 
# this means that there are 6 instances of repeated rows where year, site & box are the same (3275-3269 = 6)
# these represent relays

### Adding a relay column for further investigation
# create a vector that labels rows representing relays
relay <- numeric(3275)
for(i in 2:3275){ if (Bird_Phenology$year[i] != Bird_Phenology$year[i-1] | Bird_Phenology$site[i] != Bird_Phenology$site[i-1] | Bird_Phenology$box[i] != Bird_Phenology$box[i-1]) {
  relay[i] <- 0 #if the year or the site or the box is different between the row of interest and the previous row then the row does not represent a relay
} else if (Bird_Phenology$year[i] == Bird_Phenology$year[i-1] && Bird_Phenology$site[i] == Bird_Phenology$site[i-1] && Bird_Phenology$box[i] == Bird_Phenology$box[i-1]) {
  relay[i] <- 1 #if the year, site & box are the same in the row of interest and the previous row then that row represents a relay
}
}

# assign the vector relay to the Bird_phenology1 dataframe as a new column
column_name <- "relay"
Bird_Phenology1[[column_name]] <- relay

# check the number of relays indicated by the relay column matches the number of relays estimated previously
sum(Bird_Phenology1$relay)
#it sums to 6 meaning it indicates 6 relays occurred in the data which matches the estimation made previously

# view the rows representing relays
result <- subset(Bird_Phenology1, relay == 1)

# Print the result
print(result)

# view rows that represent sites/years in which relays occurred

relays_EDI_2019 <- subset(Bird_Phenology, year == 2019 & site == "EDI")

relays_SER_2019 <- subset(Bird_Phenology, year == 2019 & site == "SER")

relays_AVN_2019 <- subset(Bird_Phenology, year == 2019 & site == "AVN")

relays_MUN_2019 <- subset(Bird_Phenology, year == 2019 & site == "MUN")

relays_DOW_2021 <- subset(Bird_Phenology, year == 2021 & site == "DOW")

relays_AVN_2021 <- subset(Bird_Phenology, year == 2021 & site == "AVN")

# view rows representing relays using the relay column already present in the Bird_Phenology data
result <- subset(Bird_Phenology, relay == 1)
# Print the result
print(result)
#it also gives 6 instances of relays but the sixth relay is at LVN 5 in 2022 not at AVN 8 in 2021
# in notes for the row of LVN 5 2022 it says it is assumed to be a relay even though there is only one row representing that box for that year
# the row for the first instance of AVN 8 2021 has a fed but no cs value indicating that 1 or 2 eggs were laid intiially but a proper clutch was relayed over them 
# why isn't it classed as a relay? - likely a mistake in data input



# How many instances of second broods were there
sum(!is.na(Bird_Phenology$X2brood))
# apparently there is one instance of a second brood in the data

# subset the data to show the row with the second brood
Bird_Phenology_2nd_broods <- subset(Bird_Phenology, !is.na(X2brood))

### Making a new data frame for occupancy data 

# Create a new data frame with unique combinations of site and year to get the number of rows needed in the new data-frame
unique_combinations <- unique(Bird_Phenology1[c("year", "site")])
nrow(unique_combinations)
#446 rows 

# rename this data frame and reorder it by site then year
Occupancy_2 <- unique_combinations %>%
  arrange(site, year) # Sorts the dataframe first by site, then by year
#Ensure you double check the row indices remain in sequential order

### Making a boxes column for the Occupancy dataframe

# make a for loop that subsets the Bird_phenology data by each site/year combination and counts the number of unique "box" numbers
# this should give the number of boxes for a given site/year (and avoid the issue of counting extra rows representing relays as extra boxes)
boxes <- numeric(446)
for(i in 1:446){
  boxes[i] <-  length(unique(subset(Bird_Phenology1, year == Occupancy_2[i,"year"] & site == Occupancy_2[i,"site"])$box))
}

# assign the vector boxes to the Occupancy_ dataframe as a new column
column_name <- "boxes"
Occupancy_2[[column_name]] <- boxes
# check the unique values in the new column (shouldn't be less than 4 or more than 16)
unique(Occupancy_2$boxes)
# smallest value is 4 and the largest is 16

### Making a nests column for the Occupancy_ data-frame

## Check the utility of v1date as an indicator of nest box occupancy
# Initially a cs not being an NA was used as the criteria for a nest but had to resort to using v1date not being NA due to cs not being recorded in northern sites in covid years

clutches <- subset(Bird_Phenology, species == "bluti" & cs != is.na(Bird_Phenology$cs))
# There were a total of 1963 confirmed blue tit clutches

failed_clutches <- subset(Bird_Phenology, species == "bluti" & cs != is.na(Bird_Phenology$cs) & is.na(v1date))
# 91 of these clutches completely failed to produce hatchlings that survived to v1date

suc_clutches <- subset(Bird_Phenology, species == "bluti" & cs != is.na(Bird_Phenology$cs) & v1date != is.na(Bird_Phenology$v1date))
# 1872 of these clutches did produce hatchlings that survived to v1date

# check the class of the Bird_Phenology$suc column
class(Bird_Phenology$suc)
#it is a character vector (needs to be numeric)

# check the variety of characters given in the Bird_Phenology$suc column
unique(Bird_Phenology$suc)
# contains blank rows question marks and "-999" which represents NAs (these need to be classed as 0)

# create a modified Bird_Phenology dataframe where the values "-999", "" and "?" in the suc column are classed as 0s
Bird_Phenology_mod <- Bird_Phenology 

Bird_Phenology_mod$suc[Bird_Phenology$suc == ""] <- "0"
Bird_Phenology_mod$suc[Bird_Phenology$suc == "-999"] <- "0"
Bird_Phenology_mod$suc[Bird_Phenology$suc == "?"] <- "0"

# check that all the values in the column are now numbers
unique(Bird_Phenology_mod$suc)

# class the data in the suc column as numeric and check that it has worked
Bird_Phenology_mod$suc <- as.numeric(Bird_Phenology_mod$suc)
class(Bird_Phenology_mod$suc)

# are there any rows where successful fledging is recorded but no v1 is recorded?
suc_no_v1 <- subset(Bird_Phenology_mod, species == "bluti" & is.na(v1date) & suc != 0)
# there are 5 instances of successful fledging being recorded but without any v1 data

#Conclusion:
#v1 isn't a bad proxy for nest box occupancy to replace cs (only 4.6% of clutches fail completely) and although it may underestimate true population density at a site it should still capture fluctuations in population density

# However there are 5 instances of successful fledging were v1date is NA
# To correct for this, assign v1date with the value -999 where there is successful fledging but v1date is NA
for(i in 1:3275){ if(Bird_Phenology_mod$suc[i] > 0  && is.na(Bird_Phenology_mod$v1date[i])) {
  Bird_Phenology_mod$v1date[i] <-  -999
}
  
}
#check that the rows this has been applied to match the rows in the suc_no_v1 subset
subset(Bird_Phenology_mod, v1date == -999)
#indeed they do

# make a for loop that counts the number of boxes where blue tits are confirmed as nesting (i.e. v1 is not "NA") for a given site in a given year
nests <- numeric(446)
for(i in 1:446){
  nests[i] <-  length(unique(subset(Bird_Phenology_mod, year == Occupancy_2[i,"year"] & site == Occupancy_2[i,"site"] & species == "bluti" & v1date != is.na(Bird_Phenology_mod$v1date))$box))
  
}

# assign the vector nests to the Occupancy_2 data-frame as a new column
column_name <- "nests"
Occupancy_2[[column_name]] <- nests

# check the unique values in the new column (shouldn't be more than 16)
unique(Occupancy_2$nests)
# ranges from 0 to 12

### Making a proportional occupancy column for the Occupancy dataframe (represents the proportion of nests occupied)

# make a for loop that calculates the proportional occupancy (prop_occ) for each site in a given year
prop_occ <- numeric(446)
for(i in 1:446){
  prop_occ[i] <-  Occupancy_2$nests[i]/Occupancy_2$boxes[i]
}

# assign the vector prop_occ to the Occupancy dataframe as a new column
column_name <- "prop_occ"
Occupancy_2[[column_name]] <- prop_occ

### Creating mean and relative occupancy columns for the Occupancy dataframe

#This is a type of within group mean centering the will allow us to test for between and within site effects (which can also be seen as within and between year effects respectively) 

# make a for loop that calculates the mean proporitional occupancy value across years at a given site
mean_occ <- numeric(446)
for(i in 1:446){
  mean_occ[i] <-  mean(subset(Occupancy_2, site == Occupancy_2[i,"site"])$prop_occ)
}
mean_occ

# assign the vector mean_occ to the Occupancy dataframe as a new column
column_name <- "mean_occ"
Occupancy_2[[column_name]] <- mean_occ

# make a for loop that calculates the relative occupancy (i.e. occupancy relative to the mean) at a given site in a given year
rel_occ <- numeric(446)
for(i in 1:446){
  rel_occ[i] <-  Occupancy_2$prop_occ[i] - Occupancy_2$mean_occ[i] 
}
rel_occ

# assign the vector rel_occ to the Occupancy dataframe as a new column
column_name <- "rel_occ"
Occupancy_2[[column_name]] <- rel_occ


### Creating fledgling success columns for the Occupancy data


## Make a new column for the occupancy dataframe (suc_total) that represents the total fledgling success at a site in a given year
suc_total <- numeric(446)
for(i in 1:446){
  suc_total[i] <-  sum(subset(Bird_Phenology_mod, year == Occupancy_2[i,"year"] & site == Occupancy_2[i,"site"])$suc)
}
suc_total

# assign the vector suc_total to the Occupancy dataframe as a new column
column_name <- "suc_total"
Occupancy_2[[column_name]] <- suc_total


#Find out which row of of Occupancy data has the year/site where the second brood was raised and add the X2suc value to the suc_total value to account for the fledgling success of the 2nd brood 

Occupancy_2$suc_total[149] <- Occupancy_2$suc_total[149] + 6

## Make a new column for the occupancy dataframe (suc_av) that represents the average fledgling success per box at a given site in a given year
suc_av <- numeric(446)
for(i in 1:446){
  suc_av[i] <-  Occupancy_2$suc_total[i]/Occupancy_2$boxes[i]
}
# check the range of values in suc_av
range(suc_av)

# assign the vector suc_av to the Occupancy dataframe as a new column
column_name <- "suc_av"
Occupancy_2[[column_name]] <- suc_av

## Make a new column for the occupancy dataframe (suc_mean) that represents the mean fledgling success (suc_av) for a given site in across years
suc_mean <- numeric(446)
for(i in 1:446){
  suc_mean[i] <-  mean(subset(Occupancy_2, site == Occupancy_2[i,"site"])$suc_av)
}

suc_mean

# assign the vector suc_mean to the Occupancy dataframe as a new column
column_name <- "suc_mean"
Occupancy_2[[column_name]] <- suc_mean

## Make a new column for the occupancy dataframe (rel_suc) that represents the fledgling success relative to the mean (i.e. suc_av relative to suc_mean)
rel_suc <- numeric(446)
for(i in 1:446){
  rel_suc[i] <-  Occupancy_2$suc_av[i] - Occupancy_2$suc_mean[i]
}

rel_suc

# assign the vector rel_suc to the Occupancy dataframe as a new column
column_name <- "rel_suc"
Occupancy_2[[column_name]] <- rel_suc


### Remove rows representing the sites SPD and OSP from the occupancy dataframe

#check the number of rows representing OSP
subset(Occupancy_2, site == "OSP")
#7 rows

#check the number of rows representing SPD
subset(Occupancy_2, site == "SPD")
#9 rows

# Remove rows representing these sites
Occupancy_2 <- subset(Occupancy_2, site != "OSP")
Occupancy_2 <- subset(Occupancy_2, site != "SPD")
#Once these rows have been removed the Occupnacy dataframe should have 430 rows (446-16)

# Reset the row indices of the new data-frame (so they are in sequential order) - just to ensure it doesn't cause any issues
row.names(Occupancy_2) <- seq_len(nrow(Occupancy_2))

### Save Occupancy_2 as a CSV 
write.csv(Occupancy_2, 'Occupancy_2.csv', row.names=FALSE)





