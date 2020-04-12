# --------------------------------------
# 09_Reef_slope_environmental_conditions
# --------------------------------------

# A script for plotting of other environmental conditions (e.g. tide and wind data)
# Daily tide height predictions and average 9 am and 3 pm wind speed was obtained from the Australian Government Bureau of Meteorology
# for the Gladstone area and Heron Island Research Station respectively. 

# a. Read in csv files
# b. Tidy and plot data

library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(ggalt)
library(hmisc)

# --------------------------------------------------------------------------------------------------------------------

#   a) Read in csv files 

# --------------------------------------------------------------------------------------------------------------------

wd <- read.csv("Data/Wind_May_2019.csv")

td <- read.csv("Data/Tide_heights_Glad.csv")

# --------------------------------------------------------------------------------------------------------------------

# b. Tidy and plot data

# --------------------------------------------------------------------------------------------------------------------

# Wind data

wd$Date <- as.Date(wd$Date, format = "%d/%m/%Y" )

str(wd)

wd$timepoint <- paste(wd$Date, "_", wd$Time)

wd$timepoint <- ymd_hms(wd$timepoint)

ggplot(wd, aes(timepoint, spd)) +
  geom_point() +
  scale_x_datetime(name = "Date") +
  scale_y_continuous(name = "Wind speed")+
  theme_classic()


# Tidal data 

td$Date <- as.Date(td$Date, format = "%d.%m.%y" )
td$timepoint <- paste(td$Date, "_", td$Time)
td$timepoint <- ymd_hms(td$timepoint)
str(td)


ggplot(td, aes(timepoint, Height)) +
  geom_point ()+
  scale_x_datetime(name = "Date") +
  scale_y_continuous(name = "Tide Height (m)") +
  theme_classic()

spline_int <- as.data.frame(spline(td$timepoint, td$Height))

str(td)

ggplot(spline_int, aes(x = x, y = y)) + 
  geom_line() +
  theme_classic() 

ggplot() + 
  geom_line(data = spline_int, aes(x = x, y = y)) +
  theme_classic() 
