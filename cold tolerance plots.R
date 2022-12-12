##code to start developing figures for cold tolerance##
##written by Joe Endris##

library(dplyr)
library(ggplot2)
library(ggfortify)
library(multcomp)
library(multcompView)

setwd("~/Library/CloudStorage/GoogleDrive-jendris@my.apsu.edu/.shortcut-targets-by-id/1p5eHgH8eX9-QjkyyA3uRz5Lk7ontMZtO/Rehm lab - General/Trees/1- Freezing/Data/")

outputs <- read.csv("LT50 practice.csv")

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
  summarise(LT15= mean(LT15), LT50=mean(LT50), LT95=mean(LT95))

ggplot(thresholds, aes(x = Species, color = Species)) +
  geom_point(aes(y=LT15))+
  geom_point(aes(y=LT50))+
  geom_point(aes(y=LT95))+
  xlab("Species") +
  ylab("Temperature (C)") +
  theme_bw()

#edit the below code when its time to save actual plots
##ggsave(filename, plot = last_plot(),device = png(),path = NULL, scale = 1, +
#width = NA, height = NA, units = c("in"), dpi = 300, limitsize = TRUE, bg = NULL)

##plot with LT values grouped by species and state
locations <- outputs%>%
  group_by(Species, Location)%>%
  summarise(LT15= mean(LT15), LT50=mean(LT50), LT95=mean(LT95))

ggplot(locations, aes(x = Location, color = Species)) +
  geom_point(aes(y=LT15))+
  geom_point(aes(y=LT50))+
  geom_point(aes(y=LT95))+
  xlab("Location") +
  ylab("Temperature (C)") +
  theme_bw()

###work to do
#1 add std dev for plots
#2 review Lim et al 1998 & Perez and Feely 2020 for other possible types of plots to create


