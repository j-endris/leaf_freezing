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

temp<-read_excel("~/Library/CloudStorage/GoogleDrive-jendris@my.apsu.edu/.shortcut-targets-by-id/1p5eHgH8eX9-QjkyyA3uRz5Lk7ontMZtO/Rehm lab - General/Trees/1- Freezing/Data/LT50 master.xlsx")
outputs <- read.csv("LT50 practice.csv")

ggplot(outputs, aes(x = Location, y = LT50, shape = Species, color = Species)) +
  geom_boxplot(size = 2) +
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
  ylim(-20,0) +
  theme(legend.position="none")

g2<-ggplot(locations, aes(x = Location, y=LT15_mean, group=Species,color=Species)) +
  geom_point(position=position_dodge(0.5))+
  geom_errorbar(aes(ymax=LT15_mean+LT15_se,ymin=LT15_mean-LT15_se),position=position_dodge(0.5))+
  ylim(-20,0)
g3<- 
  
g1
g2
grid.arrange(g1,g2,nrow=1)
# geom_point(aes(y=LT15_mean))+
  # geom_point(aes(y=LT50_mean))+
  # geom_point(aes(y=LT95_mean))+
  # xlab("Location") +
  # ylab("Temperature (C)") +
  # theme_bw()

#scatterplot with time as the X axis
outputs$Date <- mdy(outputs$Date)

outputs_date <- group_by(outputs, Date) +
  summarise(LT15= mean(LT15), LT50=mean(LT50), LT95=mean(LT95))

ggplot(outputs, aes(x= Date, color= Species)) +
  geom_point(aes(y=LT15))+
  geom_point(aes(y=LT50))+
  geom_point(aes(y=LT95))+
  xlab ("Date")+
  ylab ("Temperature (C)")+
  theme_bw()

###work to do
#1 add std dev for plots
#2 review Lim et al 1998 & Perez and Feely 2020 for other possible types of plots to create

