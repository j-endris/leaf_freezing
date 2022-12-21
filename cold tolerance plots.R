##code to start developing figures for cold tolerance##
##written by Joe Endris##

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggfortify)
library(multcomp)
library(multcompView)
library(lubridate)
library(readxl)
library(gridExtra)

setwd("~/Library/CloudStorage/GoogleDrive-jendris@my.apsu.edu/.shortcut-targets-by-id/1p5eHgH8eX9-QjkyyA3uRz5Lk7ontMZtO/Rehm lab - General/Trees/1- Freezing/Data/")

outputs<-read_excel("~/Library/CloudStorage/GoogleDrive-jendris@my.apsu.edu/.shortcut-targets-by-id/1p5eHgH8eX9-QjkyyA3uRz5Lk7ontMZtO/Rehm lab - General/Trees/1- Freezing/Data/LT50 master.xlsx")


ggplot(outputs, aes(x = Location, y = LT50, shape = Species, color = Species)) +
  geom_point(size = 2) +
  xlab("Location") +
  ylab("Temperature (C)") +
  theme_bw()

#edit the below code when its time to save actual plots
##ggsave(filename, plot = last_plot(),device = png(),path = NULL, scale = 1, +
#width = NA, height = NA, units = c("in"), dpi = 300, limitsize = TRUE, bg = NULL)


#plot with LT values grouped by species
thresholds <- outputs%>%
  group_by(Species)%>%
  dplyr::summarise(across(LT15:LT95,list(mean=~mean(.),sd=~sd(.),se=~sd(./sqrt(6)))))
 

ggplot(thresholds, aes(x = Species, color = Species)) +
  geom_point(aes(y=LT15_mean))+
  geom_point(aes(y=LT50_mean))+
  geom_point(aes(y=LT95_mean))+
  xlab("Species") +
  ylab("Temperature (C)") +
  theme_bw()

#edit the below code when its time to save actual plots
##ggsave(filename, plot = last_plot(),device = png(),path = NULL, scale = 1, +
#width = NA, height = NA, units = c("in"), dpi = 300, limitsize = TRUE, bg = NULL)

##plot with LT values grouped by species and state
options(dplyr.summarise.inform = FALSE)

locations <- outputs%>%
  group_by(Species, Location)%>%
  dplyr::summarise(across(LT15:LT95,list(mean=~mean(.),sd=~sd(.),se=~sd(./sqrt(12)))))

locations2 <- locations%>%
  pivot_longer(locations, cols=starts_with("LT"),
               names_to = "dmg_threshold",
               values_to = "values")

g1<-ggplot(locations, aes(x = Location, y=LT50_mean, group=Species,color=Species)) +
  geom_point(position=position_dodge(0.5))+
  geom_errorbar(aes(ymax=LT50_mean+LT50_se,ymin=LT50_mean-LT50_se),position=position_dodge(0.5))+
  ylim(-35,-5) +
  theme(legend.position="none")

g2<-ggplot(locations, aes(x = Location, y=LT15_mean, group=Species,color=Species)) +
  geom_point(position=position_dodge(0.5))+
  geom_errorbar(aes(ymax=LT15_mean+LT15_se,ymin=LT15_mean-LT15_se),position=position_dodge(0.5))+
  ylim(-35,-5) +
  theme(legend.position="none")

g3<- ggplot(locations, aes(x = Location, y=LT95_mean, group=Species,color=Species)) +
  geom_point(position=position_dodge(0.5))+
  geom_errorbar(aes(ymax=LT95_mean+LT95_se,ymin=LT95_mean-LT95_se),position=position_dodge(0.5))+
  ylim(-35,-5)
  
grid.arrange(g1,g2,g3,nrow=1)

# geom_point(aes(y=LT15_mean))+
  # geom_point(aes(y=LT50_mean))+
  # geom_point(aes(y=LT95_mean))+
  # xlab("Location") +
  # ylab("Temperature (C)") +
  # theme_bw()

#scatterplot with time as the X axis
outputs$Date <- ymd(outputs$Date)
class(outputs$Date)

outputs_date <- outputs%>%
  group_by(Date, Species) %>%
  summarise(LT15.m=mean(LT15), LT50.m=mean(LT50), LT95.m=mean(LT95))

ggplot(outputs_date, aes(x= Date,y=LT50.m, color= Species)) +
  geom_point()+
  xlab ("Date")+
  ylab ("Temperature (C)")+
  theme_bw()

###########################################
##Before and After Mean last freeze plots##
###########################################


#omit any blank spots in the last_freeze column
outputs_LF <- outputs[complete.cases(outputs[,8]),]

outputs_LF <- outputs_LF%>%
  group_by(last_freeze, Location, Species)%>%
  dplyr::summarise(across(LT15:LT95,list(mean=~mean(.),sd=~sd(.),se=~sd(./sqrt(6)))))

##Grouped by location##
BA_loc_plot <- ggplot(outputs_LF, aes(x=Location, y=LT50_mean, color=Species, last_freeze)) +
  geom_point(position=position_dodge(0.5))+
  geom_errorbar(aes(ymax=LT50_mean+LT50_se,ymin=LT50_mean-LT50_se), position=position_dodge(0.5))+
  xlab ("Location") +
  ylab ("Temperature (C)")+
  theme_grey()

BA_species_plot <- ggplot(outputs_LF, aes(x=Species, y=LT50_mean, color=last_freeze))+
  geom_point(position=position_dodge(0.5))+
  geom_errorbar(aes(ymax=LT50_mean+LT50_se,ymin=LT50_mean-LT50_se), position=position_dodge(0.5))+
  xlab ("Species") +
  ylab ("Temperature (C)")+
  facet_wrap(~Location)+
  theme_grey()

BA_loc_plot
BA_species_plot
grid.arrange(BA_loc_plot,BA_species_plot,nrow=1)



