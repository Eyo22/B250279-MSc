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
install.packages('marked')
library(marked)
install.packages('R2ucare')
library(R2ucare)
install.packages('performance')
library(performance)
install.packages('sjPlot')
library(sjPlot)
install.packages('paletteer')
library(paletteer)

# Read in the data
blue_tit_survival <- read.csv("C:/Users/EyoAl/Documents/Documents/Edinburgh Uni/Dissertation/Data_Processing_&_Modelling/data/Adults7.csv", header = TRUE, stringsAsFactors = F)
#To use capture data up to 2024 read in Adults7 (not blue_tit_survival as the data in this file only goes up to 2023)

# Check the number of unique ring IDs (individuals) in the data-frame
length(unique(blue_tit_survival$ring)) #Output:2035

# Generate a table of number of rows representing each year
table(blue_tit_survival$year)

# Generate a table of number of rows representing each site
table(blue_tit_survival$site)

# How many sites are represented
length(unique(blue_tit_survival$site))

########
###Converting blue_tit_survival to a data-frame with the correct format to run a CJS model
########

###Generating the new data-frame

# Subset blue_tit survival to include columns ring, year & capture and create a temporary data-frame
temp <- blue_tit_survival[,c("ring","year","capture")]

temp <- temp %>%
# Spread out the data so that each row represents the capture history of an individual across all years (fill = 0 fills in zeros for years where an individual is not represented in the data to ensure that there are no NAs)
  spread(year, capture, fill = 0) %>% 
  # ensure the data-frame groups by ring ID before performing the following operations
  group_by(ring) %>%
  # paste together 0's and 1's in the capture history using the unite() function
  # i.e. for each row it pastes together the objects from the second column (first year)
  # to the last year ("tail(names(.),1)")
  # sep="" ensures there are no characters separating 0's and 1's
  unite("ch", 2:tail(names(.),1), sep = "")

# Rename the modified temp data-frame as a new data-frame "blue_tit_CJS"
blue_tit_CJS <- as.data.frame(temp)
head(blue_tit_CJS)

### Adding additional variables to the blue_tit_CJS data-frame

# Add a sex column (assigning sex based on the sex of the individual in the blue_tit_survival data-frame)
blue_tit_CJS$sex <- blue_tit_survival$sex[match(blue_tit_CJS$ring, blue_tit_survival$ring)]

#Since there are some cases where birds disperse between sites there are some cases where a bird is represented by more than one site
# To apply the match() function to create a site column we first need to subset blue_tit_survival to include only the last capture of an individual (i.e. rows of this data frame where the value in the survival column is 0)
last_captures <- subset(blue_tit_survival, survival == 0)
#This ensures that the last site at which a bird is caught is the representative site for that individual (this assumes that its previous site had no effect on its survival probability)

# Add a site column (assigning site based on the site at which the individual was last caught using information on a given individual in the last_captures data-frame)
blue_tit_CJS$site <- last_captures$site_recode[match(blue_tit_CJS$ring, last_captures$ring)]
#NB: This code allows you to use either the site_recode or site column of blue_tit_survival to assign site in blue_tit_CJS 
#Since each individual is only represented by one site in the site_recode column of blue_tit_survival, the use of the last_captures subset is not technically necessary
#However if you want to use the site column (which doesn't combine KCK and KCZ into one site) then dispersers are represented by more than one site and the subsetting is necessary

### Compare the site allocation of the dispersers between blue_tit_CJS and blue_tit_survival

# Create a vector of ring IDs for all known dispersers
dispersers <-c("ARD7082", "ARD7678", "AXH8209", "AXH8659", "Z632993")

# Create a logical vector using blue_tit_survival
condition <- blue_tit_survival$ring %in% dispersers

# Subset the blue_tit_survival using the condition
blue_tit_survival_dispersers <- blue_tit_survival[condition, ]

# Create a logical vector using blue_tit_CJS
condition <- blue_tit_CJS$ring %in% dispersers

# Subset the blue_tit_CJS using the condition
blue_tit_CJS_dispersers <- blue_tit_CJS[condition, ]
#It appears to have worked correctly

# Remove the ring column so capture histories appear in the first column
blue_tit_CJS <- droplevels(subset(blue_tit_CJS, select = -ring))

# Ensure sex and site are classified as a factors
blue_tit_CJS$sex <- as.factor(blue_tit_CJS$sex)
blue_tit_CJS$site <- as.factor(blue_tit_CJS$site)

#######
###CJS models
#######

###Data processing/making a design matrix

# use the "process.data()" function to process blue_tit_CJS
blue_tit_CJS.proc <- process.data(blue_tit_CJS)

# check the output
str(blue_tit_CJS.proc)

head(blue_tit_CJS.proc[[1]])

head(blue_tit_CJS.proc$data)

# Use the function "make.design.data()" to build a design matrix
blue_tit_CJS.ddl <- make.design.data(blue_tit_CJS.proc)

# Check the output
str(blue_tit_CJS.ddl)

head(blue_tit_CJS.ddl[[1]])

head(blue_tit_CJS.ddl$Phi)


###Comparing models that test site and/or year (as static and time varying covariates respectively)


# Create a function that tests a variety of models structures and compares them to each other

fit.models <- function() {
  Phi.dot <- list(formula=~1) # constant survival
  Phi.time <- list(formula=~time) # survival differs between years
  Phi.site <- list(formula=~site) # survival differs between sites
  Phi.time.site <- list(formula=~time+site) # survival differs between years and sites
  p.dot <- list(formula=~1) # constant detection
  p.time <- list(formula=~time) # detection probability differs between years
  p.site <- list(formula=~site) # detection probability differs between sites
  p.time.site <- list(formula=~time+site) # detection probability differs between years and sites
  cml <- create.model.list(c("Phi","p"))
  results <- crm.wrapper(cml, data=blue_tit_CJS.proc, ddl=blue_tit_CJS.ddl,
                         external=FALSE, accumulate=FALSE, hessian=TRUE)
  return(results)
}

blue_tit_CJS.time_site_models <- fit.models() # run function 
#NB: this may take ~24 minutes or more to run

#This produced a number of warning messages:
#Warning messages:
#1: In optimx.check(par, optcfg$ufn, optcfg$ugr, optcfg$uhess, lower,  :
#Parameters or bounds appear to have different scalings.
#This can cause poor performance in optimization. 
#It is important for derivative free methods like BOBYQA, UOBYQA, NEWUOA.
#2: In optimx.check(par, optcfg$ufn, optcfg$ugr, optcfg$uhess, lower,  :
#Parameters or bounds appear to have different scalings.
#This can cause poor performance in optimization. 
#It is important for derivative free methods like BOBYQA, UOBYQA, NEWUOA.
#3: In optimx.check(par, optcfg$ufn, optcfg$ugr, optcfg$uhess, lower,  :
#Parameters or bounds appear to have different scalings.
#This can cause poor performance in optimization. 
#It is important for derivative free methods like BOBYQA, UOBYQA, NEWUOA.
#4: In optimx.check(par, optcfg$ufn, optcfg$ugr, optcfg$uhess, lower,  :
#Parameters or bounds appear to have different scalings.
#This can cause poor performance in optimization. 
#It is important for derivative free methods like BOBYQA, UOBYQA, NEWUOA.


# Display a table comparing these models
blue_tit_CJS.time_site_models
#The model in which survival probability varies between years and detection probability varies between sites was the best fitting model

#Although its AIC value was not siginficantly different (difference < 0.2) to a model in which detection probability varies between sites and years whilst survival probability only varies between years was the best fitting model
#Their respective AIC values are significantly lower than the third best fitting model (difference > 4)

# Sve the model comparison table as a pdf
write.csv(blue_tit_CJS.time_site_models[["model.table"]], 'CJS_model_comparison.csv', row.names=FALSE)



### Test the models individually to see where the warnings appear (in the order in which they are presented in the model comparison table)

# define the possible formulas for survival and detection probability
Phi.dot <- list(formula=~1) # constant survival
Phi.time <- list(formula=~time) # survival differs between years
Phi.site <- list(formula=~site) # survival differs between sites
Phi.time.site <- list(formula=~time+site) # survival differs between years and sites
p.dot <- list(formula=~1) # constant detection
p.time <- list(formula=~time) # detection probability differs between years
p.site <- list(formula=~site) # detection probability differs between sites
p.time.site <- list(formula=~time+site) # detection probability differs between years and sites

# Run model 10 (the best fitting model)
CJS10 <- crm(blue_tit_CJS.proc, 
             blue_tit_CJS.ddl, 
            model.parameters = list(Phi = Phi.time, 
                                    p = p.site), 
            accumulate=FALSE, hessian = TRUE)
# Check the AIC
(CJS10$results$AIC)
#It should match that given in the results comparison table

# Run model 12 (the 2nd best fitting model)
CJS12 <- crm(blue_tit_CJS.proc, 
             blue_tit_CJS.ddl, 
             model.parameters = list(Phi = Phi.time, 
                                     p = p.time.site), 
             accumulate=FALSE, hessian = TRUE)
# Check the AIC
(CJS12$results$AIC)
#It should match that given in the results comparison table

# Run model 4 (the 3rd best fitting model)
CJS4 <- crm(blue_tit_CJS.proc, 
             blue_tit_CJS.ddl, 
             model.parameters = list(Phi = Phi.dot, 
                                     p = p.time.site), 
             accumulate=FALSE, hessian = TRUE)
# Check the AIC
(CJS4$results$AIC)
#It should match that given in the results comparison table

# Run model 14
CJS14 <- crm(blue_tit_CJS.proc, 
            blue_tit_CJS.ddl, 
            model.parameters = list(Phi = Phi.time.site, 
                                    p = p.site), 
            accumulate=FALSE, hessian = TRUE)
# Check the AIC
(CJS14$results$AIC)
#It should match that given in the results comparison table


# Run model 16
CJS16 <- crm(blue_tit_CJS.proc, 
             blue_tit_CJS.ddl, 
             model.parameters = list(Phi = Phi.time.site, 
                                     p = p.time.site), 
             accumulate=FALSE, hessian = TRUE)
# Check the AIC
(CJS16$results$AIC)
#It should match that given in the results comparison table


# Run model 2
CJS2 <- crm(blue_tit_CJS.proc, 
             blue_tit_CJS.ddl, 
             model.parameters = list(Phi = Phi.dot, 
                                     p = p.site), 
             accumulate=FALSE, hessian = TRUE)
# Check the AIC
(CJS2$results$AIC)
#It should match that given in the results comparison table

# Run model 8
CJS8 <- crm(blue_tit_CJS.proc, 
            blue_tit_CJS.ddl, 
            model.parameters = list(Phi = Phi.site, 
                                    p = p.time.site), 
            accumulate=FALSE, hessian = TRUE)
# Check the AIC
(CJS8$results$AIC)
#It should match that given in the results comparison table

# Run model 7
CJS7 <- crm(blue_tit_CJS.proc, 
            blue_tit_CJS.ddl, 
            model.parameters = list(Phi = Phi.site, 
                                    p = p.time), 
            accumulate=FALSE, hessian = TRUE)

# This model produced a warning message:
#In optimx.check(par, optcfg$ufn, optcfg$ugr, optcfg$uhess, lower,  :
#Parameters or bounds appear to have different scalings.
#This can cause poor performance in optimization. 
#It is important for derivative free methods like BOBYQA, UOBYQA, NEWUOA.

# Check the AIC
(CJS7$results$AIC)

# Run model 13
CJS13 <- crm(blue_tit_CJS.proc, 
            blue_tit_CJS.ddl, 
            model.parameters = list(Phi = Phi.time.site, 
                                    p = p.dot), 
            accumulate=FALSE, hessian = TRUE)
# Check the AIC
(CJS13$results$AIC)
#It should match that given in the results comparison table

# Run model 6
CJS6 <- crm(blue_tit_CJS.proc, 
             blue_tit_CJS.ddl, 
             model.parameters = list(Phi = Phi.site, 
                                     p = p.site), 
             accumulate=FALSE, hessian = TRUE)
# Check the AIC
(CJS6$results$AIC)
#It should match that given in the results comparison table

# Run model 15
CJS15 <- crm(blue_tit_CJS.proc, 
            blue_tit_CJS.ddl, 
            model.parameters = list(Phi = Phi.time.site, 
                                    p = p.time), 
            accumulate=FALSE, hessian = TRUE)
# This model produced a warning message:
#In optimx.check(par, optcfg$ufn, optcfg$ugr, optcfg$uhess, lower,  :
#Parameters or bounds appear to have different scalings.
#This can cause poor performance in optimization. 
#It is important for derivative free methods like BOBYQA, UOBYQA, NEWUOA.

# Check the AIC
(CJS15$results$AIC)
#It should match that given in the results comparison table

# Run model 3
CJS3 <- crm(blue_tit_CJS.proc, 
            blue_tit_CJS.ddl, 
            model.parameters = list(Phi = Phi.dot, 
                                    p = p.time), 
            accumulate=FALSE, hessian = TRUE)
# This model produced a warning message:
#In optimx.check(par, optcfg$ufn, optcfg$ugr, optcfg$uhess, lower,  :
#Parameters or bounds appear to have different scalings.
#This can cause poor performance in optimization. 
#It is important for derivative free methods like BOBYQA, UOBYQA, NEWUOA.


# Check the AIC
(CJS3$results$AIC)
#It should match that given in the results comparison table

# Run model 9
CJS9 <- crm(blue_tit_CJS.proc, 
            blue_tit_CJS.ddl, 
            model.parameters = list(Phi = Phi.time, 
                                    p = p.dot), 
            accumulate=FALSE, hessian = TRUE)

# Check the AIC
(CJS9$results$AIC)
#It should match that given in the results comparison table

# Run model 11
CJS11 <- crm(blue_tit_CJS.proc, 
            blue_tit_CJS.ddl, 
            model.parameters = list(Phi = Phi.time, 
                                    p = p.time), 
            accumulate=FALSE, hessian = TRUE)
# This model produced a warning message:
#In optimx.check(par, optcfg$ufn, optcfg$ugr, optcfg$uhess, lower,  :
#Parameters or bounds appear to have different scalings.
#This can cause poor performance in optimization. 
#It is important for derivative free methods like BOBYQA, UOBYQA, NEWUOA.

# Check the AIC
(CJS11$results$AIC)
#It should match that given in the results comparison table

# Run model 5
CJS5 <- crm(blue_tit_CJS.proc, 
            blue_tit_CJS.ddl, 
            model.parameters = list(Phi = Phi.site, 
                                    p = p.dot), 
            accumulate=FALSE, hessian = TRUE)

# Check the AIC
(CJS5$results$AIC)
#It should match that given in the results comparison table

# Run model 1
CJS1 <- crm(blue_tit_CJS.proc, 
            blue_tit_CJS.ddl, 
            model.parameters = list(Phi = Phi.dot, 
                                    p = p.dot), 
            accumulate=FALSE, hessian = TRUE)

# Check the AIC
(CJS1$results$AIC)
#It should match that given in the results comparison table

###Obtain the estimates of CJS1 to check average survival and detection probability

# obtain the estimates on the data scale
CJS1$results$reals


###Plot the model results for CJS12

# Plot detection probability as a function of year and site
ggplot(CJS12$results$reals$p, aes(time, estimate, ymin=lcl, ymax=ucl, col=site)) + 
  geom_errorbar(width=0) + geom_point() + ylim(0,1)

# Plot detection probability as a function of site only
ggplot(CJS12$results$reals$p, aes(site, estimate, ymin=lcl, ymax=ucl)) + 
  geom_errorbar(width=0) + geom_point() + ylim(0,1)

# Plot survival probability as a function of year
ggplot(CJS12$results$reals$Phi, aes(time, estimate, ymin=lcl, ymax=ucl)) + 
  geom_errorbar(width=0.2) + geom_point() + ylim(0,1)

###Plot the model results for CJS10

# Plot survival probability as a function of year
ggplot(CJS10$results$reals$Phi, aes(time, estimate, ymin=lcl, ymax=ucl)) + 
  
  #set the theme
  theme_light() + 
  
  #set the colour palette
  scale_color_paletteer_d("nationalparkcolors::Acadia") +
  
  #plot the predicted values with errorbars
   geom_errorbar(width=0.2, color = "#476F84FF") + 
  geom_point(color = "#476F84FF") + 
 
  #set y axis limits  
  ylim(0,1) +
  #set axis labels and specify the time intervals on the x axis  
  labs(x = "Time Intervals (between sampling events)", y = "Annual Survival Probability (Phi)") +
  scale_x_discrete(labels = c("1" ="2014-2015", "2" = "2015-2016","3" = "2016-2017","4" = "2017-2018","5" = "2018-2019", "6" = "2019-2020","7" = "2020-2021","8" = "2021-2022","9" = "2022-2023","10" = "2023-2024"))


# Plot detection probability as a function of site
ggplot(CJS10$results$reals$p, aes(site, estimate, ymin=lcl, ymax=ucl)) +

  #set the theme
  theme_light() + 
  
  #set the colour palette
  scale_color_paletteer_d("nationalparkcolors::Acadia") +
  
  #plot the predicted values with errorbars
  geom_errorbar(width=0.2, color = "#476F84FF") + 
  geom_point(color = "#476F84FF") + 
  
  #set y axis limits  
  ylim(0,1) +

  #set axis labels  
  labs(x = "Site", y = "Detection Probability (p)")

########
###Goodness of fit tests on the data
########

# Reformat the data into a matrix
blue_tit_CJS.gof <- blue_tit_CJS$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(blue_tit_CJS))

###Test 1

#The overall test. Overall, is there evidence that animals have equal detection probabilities and equal survival?

# Perform Test 1 and view output
overall_CJS(blue_tit_CJS.gof, rep(1,nrow(blue_tit_CJS)))
#p value= 0.19
#We cannot reject the null hypothesis (detection probability and survival probability does not vary among individuals)
#No strong evidence for overall lack of fit

###Test 2
#Tests the equal detection assumption

#Test 2 CT: Is there a difference in p at t+1 between those captured and not captured at t (when animals are known to be alive because are captured later in the study)?

# Perform Test 2 CT and view output
test2ct <- test2ct(blue_tit_CJS.gof, rep(1,nrow(blue_tit_CJS)))
test2ct
#p value= 0.362
#no significant difference

#Test 2 CL: Is there a difference in the expected time of next recapture between individuals captured and not captured at t when animals are known to be alive?

# Perform Test 2 CL and view output
test2cl <- test2cl(blue_tit_CJS.gof, rep(1,nrow(blue_tit_CJS)))
test2cl
#p value= 0.910
#no siginficant difference

#equal detection assumption is met

###Test 3
#Tests the equal survival assumption

#Test 3 SR: Do individuals with previous marks have different survival rates than first-time captures?

# Perform test 3 SR and view the output
test3sr <- test3sr(blue_tit_CJS.gof, rep(1,nrow(blue_tit_CJS)))
test3sr
#p value= 0.015
#individuals with previous marks appear to have different survival rates to first time captures

#Test 3 SM: For animals seen again, does when they are recaptured depend on whether they were marked on or before t?

# Perform test 3 SM and view the output
test3sm <- test3sm(blue_tit_CJS.gof, rep(1,nrow(blue_tit_CJS)))
test3sm
#p value= 0.741
#no significant difference

#Equal survival assumption is partially violated
#Test 3 SR found a signficant difference in survival probability between individuals with previous marks and first time captures
#This may be because most birds that are caught for the first time are not recaptured in the following year whilst many that are recaptured go on to be recaptured many times