##R code to to analyze freezing resistance##
##In line with Lim et al 1998##


library(dplyr)
library(dtplyr)
library(data.table)
library(ggplot2)
library(ggfortify)

setwd("~/Library/CloudStorage/GoogleDrive-jendris@my.apsu.edu/.shortcut-targets-by-id/1p5eHgH8eX9-QjkyyA3uRz5Lk7ontMZtO/Rehm lab - General/Trees/1- Freezing/Data/")

raw_data <- read.csv("scrap.csv")

#create an unique ID for each tree species/number/temp/date
freeze_data <- mutate(raw_data, Unique_ID = paste(Date.Collected, Spec,Num, Temp, sep = "."))

#calculate leakage (initial leakage/final leakage) x 100
leakage_percent <- mutate(freeze_data, leakage = ((adjusted_elec1/adjusted_elec2)*100))

#calculate injury (leakage- leakage(control))/(100-leakage(control)) x 100
#i need this to reference the appropriate control for each replicate
injury_percent <- mutate (leakage_percent, injury = ((leakage-leakage_percent[1,13])/(100-leakage_percent[1,13])*100))

#force any negative numbers in injury to be positive
injury_percent$injury -> ifelse (injury_percent$injury<0,0,injury_percent$injury)

#calculate adjusted injury (injury/injury(-40)) x 100
adjusted_injury_percent <- mutate (injury_percent, adjusted_injury = (injury/injury_percent[6,14])*100)

#force any negative numbers in adjusted_injury to be positive
adjusted_injury_percent$injury -> ifelse (adjusted_injury_percent$injury<0,0,adjusted_injury_percent$injury)

#Current Issues
#I need to figure out how to loop back to appropriate controls for each replicate, not just call to a specific cell
#code for absolute numbers isnt working correctly


