# -------------------------
# 00_Temperature_data_SB_TS 
# -------------------------

# A script for analysis of temperature data for the sub-bleaching thermal stress experiment:

#   a) Read in rda files for each tank
#   b) Calculate averages 
#   c) Plot data 
#   d) Calculate degree heating week (DHW) accumulation for each tank

# Load required packages 

library(lubridate)
library(plyr)
library(dplyr)
library(lattice)
library(openair)
library(ggplot2)
library(tidyr)

# --------------------------------------------------------------------------------------------------------------------

# .rda file format allows the user to save the data as a dataframe - specifically in this case with the date in POSIXct format. 
# N.b Protective refurs to PS SB trajectory (pre-stress single bleaching)
# N.b Single refurs to SB trajectory (single bleaching)

# Code is repeated for each temperature treatment. 

# --------------------------------------------------------------------------------------------------------------------

#   a) Read in rda files for each tank

# --------------------------------------------------------------------------------------------------------------------

load(file = "Data/SB_TS_Control_Tank_1.rda")
load(file = "Data/SB_TS_Control_Tank_2.rda")
load(file = "Data/SB_TS_Protective_Tank_1.rda")
load(file = "Data/SB_TS_Protective_Tank_2.rda")
load(file = "Data/SB_TS_Single_Tank_1.rda")
load(file = "Data/SB_TS_Single_Tank_2.rda")

# --------------------------------------------------------------------------------------------------------------------

#   b) Calculate average temperature per hour for each tank 

# --------------------------------------------------------------------------------------------------------------------

#################
  # Control tanks
#################

# Hourly average for control tank 1 using the timeAverage function in the package openair
CT1_ave <- timeAverage(CT1b, 
                        avg.time = "hour",
                        data.thresh = 0,
                        statistic = "mean",
                        start.date = "2018-04-11 00:00:00",
                        end.date = "2018-05-06 00:00:00")

# Plot the hourly average for control tank 1
ggplot(CT1_ave, aes(date, Temp)) +
  geom_line() +
  scale_x_datetime(limits = c(as.POSIXct("2018-04-11 00:00:00"), as.POSIXct("2018-05-06 00:00:00")), name = "Date")+
  scale_y_continuous(name = "Temperature", limits = c(24,27))+
  theme_classic()

# Hourly average for control tank 2 using the timeAverage function in the package openair
CT2_ave <- timeAverage(CT2b, 
                       avg.time = "hour",
                       data.thresh = 0,
                       statistic = "mean",
                       start.date = "2018-04-11 00:00:00",
                       end.date = "2018-05-06 00:00:00")

# Plot the hourly average for control tank 2
ggplot(CT2_ave, aes(date, Temp)) +
  geom_line() +
  scale_x_datetime(limits = c(as.POSIXct("2018-04-11 00:00:00"), as.POSIXct("2018-05-06 00:00:00")), name = "Date")+
  scale_y_continuous(name = "Temperature", limits = c(24,27))+
  theme_classic()

# Filter the data for out dates of interest for tank 1 and 2
CT1_ave <- CT1_ave %>% 
  filter( date > ymd_hms("2018-04-11 01:00:00")) %>%
  filter( date < ymd_hms("2018-05-06 01:00:00"))

CT2_ave <- CT2_ave %>% 
  filter( date > ymd_hms("2018-04-11 01:00:00")) %>%
  filter( date < ymd_hms("2018-05-06 01:00:00"))

# Add specifiers for each tank so that tanks can be distinguished on merging
CT1_ave$CT1 <- CT1_ave$Temp
CT1_ave$Timepoint <- CT1_ave$date
CT2_ave$CT2 <- CT2_ave$Temp

# Bind the two data frames together and reorder
timeaveCT <- cbind(CT1_ave, CT2_ave$CT2)
timeaveCT <- data.frame(timeaveCT$Timepoint, timeaveCT$CT1, timeaveCT$CT2)

# Find the average temperature across the two tanks per hour per day 

timeaveCT<- mutate(timeaveCT, daily_mean = rowMeans(cbind(timeaveCT.CT1, timeaveCT.CT2)))

# Rename columns
colnames(timeaveCT) <- c("Timepoint", "CT_1", "CT_2", "daily_mean")

str(timeaveCT)

ggplot(timeaveCT, aes(Timepoint,daily_mean)) +
  geom_line() +
  scale_x_datetime(limits = c(as.POSIXct("2018-04-11 00:00:00"), as.POSIXct("2018-05-06 00:00:00")), name = "Date")+
  scale_y_continuous(name = "Temperature")+
  theme_classic()

######################
  # Protective (PS SB)
######################

# Hourly average for protective tank 1 using the timeAverage function in the package openair
PR1_ave <- timeAverage(PR1b, 
                       avg.time = "hour",
                       data.thresh = 0,
                       statistic = "mean",
                       start.date = "2018-04-11 00:00:00",
                       end.date = "2018-05-06 00:00:00")

# Plot the hourly average for protective tank 1
ggplot(PR1_ave, aes(date, Temp)) +
  geom_line() +
  scale_x_datetime(limits = c(as.POSIXct("2018-04-11 00:00:00"), as.POSIXct("2018-05-06 00:00:00")), name = "Date")+
  scale_y_continuous(name = "Temperature", limits = c(24,35))+
  theme_classic()

# Hourly average for protective tank 2 using the timeAverage function in the package openair
PR2_ave <- timeAverage(PR2b, 
                       avg.time = "hour",
                       data.thresh = 0,
                       statistic = "mean",
                       start.date = "2018-04-11 00:00:00",
                       end.date = "2018-05-06 00:00:00")

# Plot the hourly average for protective tank 2
ggplot(PR2_ave, aes(date, Temp)) +
  geom_line() +
  scale_x_datetime(limits = c(as.POSIXct("2018-04-11 00:00:00"), as.POSIXct("2018-05-06 00:00:00")), name = "Date")+
  scale_y_continuous(name = "Temperature", limits = c(24,35))+
  theme_classic()

# Filter the data for out dates of interest for tank 1 and 2
PR1_ave <- PR1_ave %>% 
  filter( date > ymd_hms("2018-04-11 01:00:00")) %>%
  filter( date < ymd_hms("2018-05-06 01:00:00"))

PR2_ave <- PR2_ave %>% 
  filter( date > ymd_hms("2018-04-11 01:00:00")) %>%
  filter( date < ymd_hms("2018-05-06 01:00:00"))

# Add specifiers for each tank so that tanks can be distinguished on merging
PR1_ave$PR1 <- PR1_ave$Temp
PR1_ave$Timepoint <- PR1_ave$date
PR2_ave$PR2 <- PR2_ave$Temp

# Bind the two data frames together and reorder
timeavePR <- cbind(PR1_ave, PR2_ave$PR2)
timeavePR <- data.frame(timeavePR$Timepoint, timeavePR$PR1, timeavePR$PR2)

# Find the average temperature across the two tanks per hour per day 

timeavePR <- mutate(timeavePR, daily_mean = rowMeans(cbind(timeavePR.PR1, timeavePR.PR2)))

# Rename columns
colnames(timeavePR) <- c("Timepoint", "PR_1", "PR_2", "daily_mean")

str(timeavePR)

ggplot(timeavePR, aes(Timepoint, daily_mean)) +
  geom_line() +
  scale_x_datetime(limits = c(as.POSIXct("2018-04-11 00:00:00"), as.POSIXct("2018-05-06 00:00:00")), name = "Date")+
  scale_y_continuous(name = "Temperature")+
  theme_classic()

###############
  # Single (SB)
###############

SI1_ave <- timeAverage(SI1b, 
                       avg.time = "hour",
                       data.thresh = 0,
                       statistic = "mean",
                       start.date = "2018-04-11 00:00:00",
                       end.date = "2018-05-06 00:00:00")

ggplot(SI1_ave, aes(date, Temp)) +
  geom_line() +
  scale_x_datetime(limits = c(as.POSIXct("2018-04-11 00:00:00"), as.POSIXct("2018-05-06 00:00:00")), name = "Date")+
  scale_y_continuous(name = "Temperature", limits = c(24,35))+
  theme_classic()

SI2_ave <- timeAverage(SI2b, 
                       avg.time = "hour",
                       data.thresh = 0,
                       statistic = "mean",
                       start.date = "2018-04-11 00:00:00",
                       end.date = "2018-05-06 00:00:00")

ggplot(SI2_ave, aes(date, Temp)) +
  geom_line() +
  scale_x_datetime(limits = c(as.POSIXct("2018-04-11 00:00:00"), as.POSIXct("2018-05-06 00:00:00")), name = "Date")+
  scale_y_continuous(name = "Temperature", limits = c(24,35))+
  theme_classic()

# Filter the data for out dates of interest for tank 1 and 2
SI1_ave <- SI1_ave %>% 
  filter( date > ymd_hms("2018-04-11 01:00:00")) %>%
  filter( date < ymd_hms("2018-05-06 01:00:00"))

SI2_ave <- SI2_ave %>% 
  filter( date > ymd_hms("2018-04-11 01:00:00")) %>%
  filter( date < ymd_hms("2018-05-06 01:00:00"))

# Add specifiers for each tank so that tanks can be distinguished on merging
SI1_ave$SI1 <- SI1_ave$Temp
SI1_ave$Timepoint <- SI1_ave$date
SI2_ave$SI2 <- SI2_ave$Temp

# Bind the two data frames together and reorder
timeaveSI <- cbind(SI1_ave, SI2_ave$SI2)
timeaveSI <- data.frame(timeaveSI$Timepoint, timeaveSI$SI1, timeaveSI$SI2)

# Find the average temperature across the two tanks per hour per day 

timeaveSI <- mutate(timeaveSI, daily_mean = rowMeans(cbind(timeaveSI.SI1, timeaveSI.SI2)))

# Rename columns
colnames(timeaveSI) <- c("Timepoint", "SI_1", "SI_2", "daily_mean")

str(timeaveSI)

ggplot(timeaveSI, aes(Timepoint, daily_mean)) +
  geom_line() +
  scale_x_datetime(limits = c(as.POSIXct("2018-04-11 00:00:00"), as.POSIXct("2018-05-06 00:00:00")), name = "Date")+
  scale_y_continuous(name = "Temperature")+
  theme_classic()

# --------------------------------------------------------------------------------------------------------------------

#   c) Plot data 

# --------------------------------------------------------------------------------------------------------------------

str(timeaveCT)
str(timeavePR)
str(timeaveSI)


ggplot() +
  geom_line(data = timeaveCT, aes(x =Timepoint, y = daily_mean, colour = "Ambient")) +
  geom_line(data = timeavePR, aes(x =Timepoint, y = daily_mean, colour = "PS SB")) +
  geom_line(data = timeaveSI, aes(x =Timepoint, y = daily_mean, colour = "SB")) +
  scale_x_datetime(limits = c(as.POSIXct("2018-04-11 00:00:00"), as.POSIXct("2018-05-06 00:00:00")), name = "Date")+
  scale_y_continuous(breaks = seq(24,35,1), name = "Temperature (°C)") +
  scale_colour_manual(name = "Treatment", values = c ("dodgerblue4", "seagreen", "firebrick4"), labels = c("Ambient", "PS SB","SB")) +
  theme_classic() +
  theme(axis.text.x = element_text(size = "11"),
        axis.text.y = element_text(size = "11"),
        axis.title.x = element_text(size = "11"),
        axis.title.y = element_text(size = "11"))


# --------------------------------------------------------------------------------------------------------------------

#   d) Calculate degree heating week (DHW) accumulation for each tank

# --------------------------------------------------------------------------------------------------------------------

# Change the file names so that they are specific to the treatment
timeaveCT$daily_mean_CT <- timeaveCT$daily_mean
timeavePR$daily_mean_PR <- timeavePR$daily_mean
timeaveSI$daily_mean_SI<- timeaveSI$daily_mean

# Split Timepoint by space deliminator into two columns 

CT <- separate(timeaveCT, Timepoint, c("date","time"), sep = " ", remove = TRUE,
               convert = FALSE, extra = "warn", fill = "warn")
PR <- separate(timeavePR, Timepoint, c("date","time"), sep = " ", remove = TRUE,
               convert = FALSE, extra = "warn", fill = "warn")
SI <- separate(timeaveSI, Timepoint, c("date","time"), sep = " ", remove = TRUE,
               convert = FALSE, extra = "warn", fill = "warn")

# Calculate the daily mean temperature 
aggdata1 <- ddply(CT, ~date, summarize, daily_mean_temp = mean(daily_mean_CT, na.rm = TRUE))

aggdata2 <- ddply(PR, ~date, summarize, daily_mean_temp = mean(daily_mean_PR, na.rm = TRUE))

aggdata3 <- ddply(SI, ~date, summarize, daily_mean_temp = mean(daily_mean_SI, na.rm = TRUE))

# Calculate degree heating days by caclulating the anomaly on days above the maximum monthly mean for Heron 
# of 27.3 °C
aggdata1$DHD <- ifelse(aggdata1$daily_mean_temp > 27.3, (aggdata1$daily_mean_temp - 27.3), 0)
str(aggdata1)

aggdata2$DHD <- ifelse(aggdata2$daily_mean_temp > 27.3, (aggdata2$daily_mean_temp - 27.3), 0)
str(aggdata2)

aggdata3$DHD <- ifelse(aggdata3$daily_mean_temp > 27.3, (aggdata3$daily_mean_temp - 27.3), 0)
str(aggdata3)

# Create DHWs by dividing DHDs by 7

aggdata1$DHW <- aggdata1$DHD/7   
str(aggdata1)

aggdata2$DHW <- aggdata2$DHD/7   
str(aggdata2)

aggdata3$DHW <- aggdata3$DHD/7   
str(aggdata3)

#Creating cumulative variables to track heat load

aggdata1$totalDHD <- cumsum(aggdata1$DHD)
aggdata1$totalDHW <- cumsum(aggdata1$DHW)


aggdata2$totalDHD <- cumsum(aggdata2$DHD)
aggdata2$totalDHW <- cumsum(aggdata2$DHW)

aggdata3$totalDHD <- cumsum(aggdata3$DHD)
aggdata3$totalDHW <- cumsum(aggdata3$DHW)


# Add a variable that is Day 


aggdata1 <- mutate(aggdata1, Day = c(1:26))
aggdata2 <- mutate(aggdata2, Day = c(1:26))
aggdata3 <- mutate(aggdata3, Day = c(1:26))

library(ggplot2)
ggplot() +
  geom_line(data = aggdata1, aes(x =Day, y = totalDHW, colour = "Ambient")) +
  geom_line(data = aggdata2, aes(x =Day, y = totalDHW, colour = "Protective")) +
  geom_line(data = aggdata3, aes(x =Day, y = totalDHW, colour = "Single")) +
  geom_point(data = aggdata1, aes(x =Day, y = totalDHW, colour = "Ambient")) +
  geom_point(data = aggdata2, aes(x =Day, y = totalDHW, colour = "Protective")) +
  geom_point(data = aggdata3, aes(x =Day, y = totalDHW, colour = "Single")) +
  scale_x_continuous(breaks = seq(0,26,1), name = "Day")+
  scale_y_continuous(breaks = seq(0,4,0.5), name = "Total Degree Heating Weeks")+
  scale_colour_manual(name = "Treatment", values = c ("dodgerblue4", "seagreen", "indianred4"), labels = c("Ambient", "Protective","Single")) +
  theme_classic() +
  theme(axis.text.x = element_text(size = "10"),
        axis.text.y = element_text(size = "10"),
        axis.title.x = element_text(size = "10"),
        axis.title.y = element_text(size = "10"))











