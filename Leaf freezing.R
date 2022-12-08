##R code to to analyze freezing resistance##
##In line with Lim et al 1998##
##by Joe Endris##

library(dplyr)
library(dtplyr)
library(data.table)
library(ggplot2)
library(ggfortify)

setwd("~/Library/CloudStorage/GoogleDrive-jendris@my.apsu.edu/.shortcut-targets-by-id/1p5eHgH8eX9-QjkyyA3uRz5Lk7ontMZtO/Rehm lab - General/Trees/1- Freezing/Data/")

raw_data <- read.csv("scrap.csv")

#create an unique ID for each tree species/number/temp/date
freeze_data <- mutate(raw_data, Unique_ID = paste(Date.Collected, Spec,Num, sep = ""))

#calculate leakage (initial leakage/final leakage) x 100
freeze_data<-mutate(freeze_data, leakage = ((adjusted_elec1/adjusted_elec2)*100))

#looping over each unique ID for injury
comb<-data.frame()
for(j in 1:length(unique(freeze_data$Unique_ID))){
  id<-unique(freeze_data$Unique_ID)[j]
  test<-freeze_data[which(freeze_data$Unique_ID==id),]
  for(i in 1:6){
test$injury[i]<-((test$leakage[i]-test$leakage[1])/(100-test$leakage[1]))*100
}
comb<-rbind(comb,test)
}
comb$injury<-as.numeric(comb$injury)
#changing all negatives to zero before calculating the adjusted injury
comb$injury<-ifelse(comb$injur<0,0,comb$injury)
comb$adjInj<-0

#change all -40 control results to 100 when the value in the injury column is 0
#first need to inspect these individual data to see if entered wrong
x<-which(comb$injury==0&comb$Temp==-40)
comb$Unique_ID[x]

#still need to do the adjusted injury
comb2<-data.frame()
for(j in 1:length(unique(comb$Unique_ID))){
  id<-unique(comb$Unique_ID)[j]
  test<-comb[which(comb$Unique_ID==id),]
  for(i in 1:6){
    test$adjInj[i]<-((test$injury[i]/test$injury[6]))*100
  }
  comb2<-rbind(comb2,test)
}

#force any negative numbers in adjusted_injury to be positive
comb2$adjInj<-round(comb2$adjInj,2)

#Current Issues
#I need to figure out how to loop back to appropriate controls for each replicate, not just call to a specific cell
#is this where I need to run a loop?

#code for absolute numbers isn't working correctly; 
#returns error Error in ifelse(injury_percent$injury < 0, 0, NA) <- injury_percent$injury : 
#could not find function "ifelse<-"



