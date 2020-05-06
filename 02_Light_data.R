# -------------
# 02_Light_data
# -------------

# A script for analysis of light data for the bleaching thermal stress experiment:

#   a) Read in rda file
#   b) Calculate averages 
#   c) Plot data 

# Load required packages 

library(lubridate)
library(plyr)
library(dplyr)
library(lattice)
library(openair)
library(ggplot2)
library(tidyr)

# --------------------------------------------------------------------------------------------------------------------

#   a) Read in rda file

# --------------------------------------------------------------------------------------------------------------------

load("Data/B_TS_Light_data.rda")

# --------------------------------------------------------------------------------------------------------------------

#   b) Calculate averages 

# --------------------------------------------------------------------------------------------------------------------

str(Light)

Light_ave <- unite(data = Light, col = "Timepoint", c(1,2))

Light_ave$Timepoint <- dmy_hms(Light_ave$Timepoint)

str(Light_ave)

str(Light_ave)

Light_ave$date <- Light_ave$Timepoint
timeaveLI <- timeAverage(Light_ave, 
                          avg.time = "30 min",
                          data.thresh = 0,
                          statistic = "mean",
                          fill = TRUE)

# --------------------------------------------------------------------------------------------------------------------

#   c) Plot average

# --------------------------------------------------------------------------------------------------------------------

str(timeaveLI)
ggplot(timeaveLI, aes(date, Calib)) +
  geom_line() +
  scale_x_datetime(limits = c(as.POSIXct("2019-02-24 00:00:00"), as.POSIXct("2019-03-25 00:00:00"))) +
  theme_classic()







