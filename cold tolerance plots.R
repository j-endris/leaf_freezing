##code to start developing figures for cold tolerance##
##written by Joe Endris##

library(dplyr)
library(ggplot2)
library(ggfortify)
library(multcomp)
library(multcompView)

setwd("~/Library/CloudStorage/GoogleDrive-jendris@my.apsu.edu/.shortcut-targets-by-id/1p5eHgH8eX9-QjkyyA3uRz5Lk7ontMZtO/Rehm lab - General/Trees/1- Freezing/Data/")

outputs <- read.csv("LT50 practice.csv")

scatterplot <- ggplot(outputs, aes(x = Location, y = LT50, shape = Species, color = Species)) +
  geom_point(size = 2) +
  xlab("Location") +
  ylab("Temperature (C)") +
  theme_bw()

