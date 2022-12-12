##R code to to analyze freezing resistance##
##In line with Lim et al 1998##
##by Joe Endris##

##change raw .csv file to actual data when time to analyze 

library(dplyr)
library(dtplyr)
library(data.table)
library(ggplot2)
library(ggfortify)

setwd("~/Library/CloudStorage/GoogleDrive-jendris@my.apsu.edu/.shortcut-targets-by-id/1p5eHgH8eX9-QjkyyA3uRz5Lk7ontMZtO/Rehm lab - General/Trees/1- Freezing/Data/")
#for Evan's data
#raw_data<-read.csv("/Volumes/GoogleDrive/My Drive/Rehm lab - General/Trees/1- Freezing/Data/scrap.csv")

raw_data <- read.csv("scrap.csv")

#create an unique ID for each tree species/number/temp/date
freeze_data <- mutate(raw_data, Unique_ID = paste(Date.Collected, Spec,Num, sep = ""))

#calculate leakage (initial leakage/final leakage) x 100
freeze_data<-mutate(freeze_data, leakage = ((adjusted_elec1/adjusted_elec2)*100))

#looping over each unique ID for injury
injury_percent<-data.frame()
for(j in 1:length(unique(freeze_data$Unique_ID))){
  id<-unique(freeze_data$Unique_ID)[j]
  test<-freeze_data[which(freeze_data$Unique_ID==id),]
  for(i in 1:6){
test$injury[i]<-((test$leakage[i]-test$leakage[1])/(100-test$leakage[1]))*100
}
injury_percent<-rbind(injury_percent,test)
}
injury_percent$injury<-as.numeric(injury_percent$injury)

#changing all negatives to zero before calculating the adjusted injury
injury_percent$injury<-ifelse(injury_percent$injur<0,0,injury_percent$injury)
injury_percent$adjInj<-0

#change all -40 control results to 100 when the value in the injury column is 0
#first need to inspect these individual data to see if entered wrong
x<-which(injury_percent$injury==0&injury_percent$Temp==-40)
injury_percent$Unique_ID[x]

#force any negative numbers in injury to be positive
injury_percent$injury <- ifelse(injury_percent$injury<0,0,injury_percent$injury)

#force any negative numbers in injury to be positive
injury_percent$injury <- ifelse(injury_percent$injury<0,0,injury_percent$injury)

#calculate adjusted injury (Injury[temperature]-/injury[-40])x 100
adj_injury_percent<-data.frame()
for(j in 1:length(unique(injury_percent$Unique_ID))){
  id<-unique(injury_percent$Unique_ID)[j]
  test<-injury_percent[which(injury_percent$Unique_ID==id),]
  for(i in 1:6){
    test$adjInj[i]<-((test$injury[i]/test$injury[6]))*100
  }
  adj_injury_percent<-rbind(adj_injury_percent,test)
}

#round the numbers in the adjusted injury column to two digits past decimal point
adj_injury_percent$adjInj<-round(adj_injury_percent$adjInj,2)

#force any negative numbers in adjusted_injury to be positive
adj_injury_percent$injury <- if_else (adj_injury_percent$injury<0,0,adj_injury_percent$injury)


#to do worklist
#1 determine use of wonky individuals
#2 



