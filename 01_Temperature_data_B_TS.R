# -------------------------
# 01_Temperature_data_B_TS
# -------------------------

# A script for analysis of temperature data for the bleaching thermal stress experiment:

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

# Code is repeated for each temperature treatment. 

# --------------------------------------------------------------------------------------------------------------------

load(file = "Data/B_TS_Heat_Tank_1.rda")
load(file = "Data/B_TS_Heat_Tank_2.rda")

load(file = "Data/B_TS_Control_Tank_1.rda")
load(file = "Data/B_TS_Control_Tank_2.rda")

# --------------------------------------------------------------------------------------------------------------------

#   b) Calculate average temperature per hour for each tank 

# --------------------------------------------------------------------------------------------------------------------

#################
# Control tanks
#################

str(AMB_1)
str(AMB_2)

# Hourly average for control tank 1 using the timeAverage function in the package openair
CT1_ave <- timeAverage(AMB_1, 
                       avg.time = "hour",
                       data.thresh = 0,
                       statistic = "mean",
                       start.date = "2019-03-04 00:00:00",
                       end.date = "2019-03-23 00:00:00")

# Plot the hourly average for bleaching tank 1 data
ggplot(CT1_ave, aes(date, Temp)) +
  geom_line() +
  scale_x_datetime(limits = c(as.POSIXct("2019-03-04 00:00:00"), as.POSIXct("2019-03-23 00:00:00")), name = "Date")+
  scale_y_continuous(name = "Temperature", limits = c(24,35))+
  theme_classic()

# Hourly average for control tank 2 using the timeAverage function in the package openair
CT2_ave <- timeAverage(AMB_2, 
                       avg.time = "hour",
                       data.thresh = 0,
                       statistic = "mean",
                       start.date = "2019-03-04 00:00:00",
                       end.date = "2019-03-23 00:00:00")

ggplot(CT2_ave, aes(date, Temp)) +
  geom_line() +
  scale_x_datetime(limits = c(as.POSIXct("2019-03-04 00:00:00"), as.POSIXct("2019-03-23 00:00:00")), name = "Date")+
  scale_y_continuous(name = "Temperature", limits = c(24,35))+
  theme_classic()

# Filter the data for out dates of interest for tank 1 and 2
CT1_ave <- CT1_ave %>% 
  filter( date > ymd_hms("2019-03-04 00:00:00")) %>%
  filter( date < ymd_hms("2019-03-23 00:00:00"))

CT2_ave <- CT1_ave %>% 
  filter( date > ymd_hms("2019-03-04 00:00:00")) %>%
  filter( date < ymd_hms("2019-03-23 00:00:00"))

# Add specifiers for each tank so that tanks can be distinguished on merging
CT1_ave$CT1 <- CT1_ave$Temp
CT1_ave$Timepoint <- CT1_ave$date
CT2_ave$CT2 <- CT2_ave$Temp

# Bind the two data frames together and reorder
timeaveCT <- cbind(CT1_ave, CT2_ave$CT2)
timeaveCT<- data.frame(timeaveCT$Timepoint, timeaveCT$CT1, timeaveCT$CT2)

# Find the average temperature across the two tanks per hour per day 

timeaveCT<- mutate(timeaveCT, daily_mean = rowMeans(cbind(timeaveCT.CT1, timeaveCT.CT2)))

# Rename columns
colnames(timeaveCT) <- c("Timepoint", "CT_1", "CT_2", "daily_mean")

ggplot(timeaveCT, aes(Timepoint,daily_mean)) +
  geom_line() +
  scale_x_datetime(limits = c(as.POSIXct("2019-03-04 00:00:00"), as.POSIXct("2019-03-23 00:00:00")), name = "Date")+
  scale_y_continuous(name = "Temperature")+
  theme_classic()

###############
# Heat tank (B)
###############

str(BT_1)

# Rename the timepoint as date for the function timeAverage to work 
BT_1$date <- BT_1$Timepoint

# Hourly average for bleaching tank 1 using the timeAverage function in the package openair
BT1_ave <- timeAverage(BT_1, 
                        avg.time = "hour",
                        data.thresh = 0,
                        statistic = "mean",
                        start.date = "2019-03-04 00:00:00",
                        end.date = "2019-03-23 00:00:00")

# Plot the hourly average for bleaching tank 1 data
ggplot(BT1_ave, aes(date, Temp)) +
  geom_line() +
  scale_x_datetime(limits = c(as.POSIXct("2019-03-04 00:00:00"), as.POSIXct("2019-03-23 00:00:00")), name = "Date")+
  scale_y_continuous(name = "Temperature", limits = c(24,35))+
  theme_classic()

# Rename the timepoint as date for the function timeAverage to work 
BT_2$date <- BT_2$Timepoint

# Hourly average for bleaching tank 2 using the timeAverage function in the package openair
BT2_ave <- timeAverage(BT_2, 
                       avg.time = "hour",
                       data.thresh = 0,
                       statistic = "mean",
                       start.date = "2019-03-04 00:00:00",
                       end.date = "2019-03-23 00:00:00")

ggplot(BT2_ave, aes(date, Temp)) +
  geom_line() +
  scale_x_datetime(limits = c(as.POSIXct("2019-03-04 00:00:00"), as.POSIXct("2019-03-23 00:00:00")), name = "Date")+
  scale_y_continuous(name = "Temperature", limits = c(24,35))+
  theme_classic()

# Filter the data for out dates of interest for tank 1 and 2
BT1_ave <- BT1_ave %>% 
  filter( date > ymd_hms("2019-03-04 00:00:00")) %>%
  filter( date < ymd_hms("2019-03-23 00:00:00"))

BT2_ave <- BT1_ave %>% 
  filter( date > ymd_hms("2019-03-04 00:00:00")) %>%
  filter( date < ymd_hms("2019-03-23 00:00:00"))

# Add specifiers for each tank so that tanks can be distinguished on merging
BT1_ave$BT1 <- BT1_ave$Temp
BT1_ave$Timepoint <- BT1_ave$date
BT2_ave$BT2 <- BT2_ave$Temp

# Bind the two data frames together and reorder
timeaveBT <- cbind(BT1_ave, BT2_ave$BT2)
timeaveBT<- data.frame(timeaveBT$Timepoint, timeaveBT$BT1, timeaveBT$BT2)

# Find the average temperature across the two tanks per hour per day 

timeaveBT<- mutate(timeaveBT, daily_mean = rowMeans(cbind(timeaveBT.BT1, timeaveBT.BT2)))

# Rename columns
colnames(timeaveBT) <- c("Timepoint", "BT_1", "BT_2", "daily_mean")

ggplot(timeaveBT, aes(Timepoint,daily_mean)) +
  geom_line() +
  scale_x_datetime(limits = c(as.POSIXct("2019-03-04 00:00:00"), as.POSIXct("2019-03-23 00:00:00")), name = "Date")+
  scale_y_continuous(name = "Temperature")+
  theme_classic()

# --------------------------------------------------------------------------------------------------------------------

#   c) Plot data 

# --------------------------------------------------------------------------------------------------------------------


ggplot() +
  geom_line(data = timeaveBT, aes(x =Timepoint, y = daily_mean, colour = "Ambient"), size = 0.4) +
  geom_line(data = timeaveCT, aes(x =Timepoint, y = daily_mean, colour = "PS SB"), size = 0.4) +
  scale_x_datetime(limits = c(as.POSIXct("2019-03-04 00:00:00"), as.POSIXct("2019-03-23 00:00:00")), name = "Date")+
  scale_y_continuous(breaks = seq(24,35,1), name = "Temperature (°C)") +
  scale_colour_manual(name = "Treatment", values = c ("dodgerblue4", "firebrick4"), labels = c("Ambient", "PS SB","SB")) +
  theme_classic() +
  theme(axis.text.x = element_text(size = "15"),
        axis.text.y = element_text(size = "15"),
        axis.title.x = element_text(size = "15"),
        axis.title.y = element_text(size = "15"))


#ff5e6c

ggplot() +
  geom_line(data = timeaveCT, aes(x =Timepoint, y = daily_mean, colour = "Ambient"), size = 0.4) +
  geom_line(data = timeavePR, aes(x =Timepoint, y = daily_mean, colour = "PS SB"), size = 0.4) +
  geom_line(data = timeaveSI, aes(x =Timepoint, y = daily_mean, colour = "SB"), size = 0.4) +
  scale_x_datetime(limits = c(as.POSIXct("2018-04-11 00:00:00"), as.POSIXct("2018-05-06 00:00:00")), name = "Date")+
  scale_y_continuous(breaks = seq(24,35,1), name = "Temperature (°C)") +
  scale_colour_manual(name = "Treatment", values = c ("#0294A5", "#A79C93", "#C1403D"), labels = c("Ambient", "PS SB","SB")) +
  theme_classic() +
  theme(axis.text.x = element_text(size = "15"),
        axis.text.y = element_text(size = "15"),
        axis.title.x = element_text(size = "15"),
        axis.title.y = element_text(size = "15"))




# --------------------------------------------------------------------------------------------------------------------

#   d) Calculate degree heating week (DHW) accumulation for each tank

# --------------------------------------------------------------------------------------------------------------------

# Change the file names so that they are specific to the treatment
timeaveCT$daily_mean_CT <- timeaveCT$daily_mean
timeaveBT$daily_mean_BT <- timeaveBT$daily_mean


# Split Timepoint by space deliminator into two columns 

CT <- separate(timeaveCT, Timepoint, c("date","time"), sep = " ", remove = TRUE,
               convert = FALSE, extra = "warn", fill = "warn")
BT <- separate(timeaveBT, Timepoint, c("date","time"), sep = " ", remove = TRUE,
               convert = FALSE, extra = "warn", fill = "warn")

# Calculate the daily mean temperature 
aggdata1 <- ddply(CT, ~date, summarize, daily_mean_temp = mean(daily_mean_CT, na.rm = TRUE))

aggdata2 <- ddply(BT, ~date, summarize, daily_mean_temp = mean(daily_mean_BT, na.rm = TRUE))

# Calculate degree heating days by caclulating the anomaly on days above the maximum monthly mean for Heron 
# of 27.3 °C
aggdata1$DHD <- ifelse(aggdata1$daily_mean_temp > 27.3, (aggdata1$daily_mean_temp - 27.3), 0)
str(aggdata1)

aggdata2$DHD <- ifelse(aggdata2$daily_mean_temp > 27.3, (aggdata2$daily_mean_temp - 27.3), 0)
str(aggdata2)

# Create DHWs by dividing DHDs by 7

aggdata1$DHW <- aggdata1$DHD/7   
str(aggdata1)

aggdata2$DHW <- aggdata2$DHD/7   
str(aggdata2)


#Creating cumulative variables to track heat load

aggdata1$totalDHD <- cumsum(aggdata1$DHD)
aggdata1$totalDHW <- cumsum(aggdata1$DHW)

aggdata2$totalDHD <- cumsum(aggdata2$DHD)
aggdata2$totalDHW <- cumsum(aggdata2$DHW)

# Add a variable that is Day 

aggdata1 <- mutate(aggdata1, Day = c(1:19))
aggdata2 <- mutate(aggdata2, Day = c(1:19))

# Create a master plot of DHW data 

ggplot() +
  geom_line(data = aggdata1, aes(x =Day, y = totalDHW, colour = "Ambient")) +
  geom_line(data = aggdata2, aes(x =Day, y = totalDHW, colour = "Bleaching")) +
  geom_point(data = aggdata1, aes(x =Day, y = totalDHW, colour = "Ambient")) +
  geom_point(data = aggdata2, aes(x =Day, y = totalDHW, colour = "Bleaching")) +
  scale_x_continuous(breaks = seq(0,26,1), name = "Day")+
  scale_y_continuous(breaks = seq(0,5,0.5), name = "Total Degree Heating Weeks")+
  scale_colour_manual(name = "Treatment", values = c ("dodgerblue4", "indianred4"), labels = c("Ambient", "Bleaching")) +
  theme_classic() +
  theme(axis.text.x = element_text(size = "10"),
        axis.text.y = element_text(size = "10"),
        axis.title.x = element_text(size = "10"),
        axis.title.y = element_text(size = "10"))


ggplot() +
  #geom_line(data = aggdata1, aes(x =Day, y = totalDHW, colour = "Ambient")) +
  geom_line(data = aggdata2, aes(x =Day, y = totalDHW), size = 0.2) +
  #geom_point(data = aggdata1, aes(x =Day, y = totalDHW, colour = "Ambient")) +
  geom_point(data = aggdata2, aes(x =Day, y = totalDHW)) +
  scale_x_continuous(limits = c(0,20),breaks = seq(0,20,1), name = "Day")+
  scale_y_continuous(limits = c(0,5), breaks = seq(0,5,0.5), name = "eDHW", position = "right")+
  #scale_colour_manual(name = "Treatment", values = c ("dodgerblue4", "indianred4"), labels = c("Ambient", "Bleaching")) +
  theme_classic() +
  theme(axis.text.x = element_text(size = "15"),
        axis.text.y = element_text(size = "15"),
        axis.title.x = element_text(size = "15"),
        axis.title.y = element_text(size = "15"))













