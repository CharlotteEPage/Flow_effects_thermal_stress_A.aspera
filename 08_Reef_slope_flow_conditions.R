# -----------------------------
# 08_Reef_slope_flow_conditions
# -----------------------------

# A script for analysis of flow conditions on the reef slope at Heron Island (Site: Coral Gardens).

# Data collected using drag/tilt Marotte Current Meters. 

# a. Read in data (.rda file)
# b. Plot data (line graphs)
# c. Plot data (wind rose)
# d. Plot data (violin plot)
# e. Plot data (cumulative sum)
# f. Two sample Kolmogorov-Smirnov test 
# g. Granger Test 

library(zoo)
library(xts)
library(Hmisc)
library(lubridate)
library(plyr)
library(lattice)
library(openair)
library(ggplot2)
library(pastecs)
library(tidyr)
library(dplyr)
library(tibbletime)
library(lubridate)
library(ggplot2)
library(tidyr)
library(dplyr)
library(Hmisc)
library(mgcv)
library(pastecs)

# --------------------------------------------------------------------------------------------------------------------

#   a) Read in rda files 

# --------------------------------------------------------------------------------------------------------------------

load("Data/Reef_slope_flow_conditions.rda")

str(lg)

# --------------------------------------------------------------------------------------------------------------------

# b. Plot data (line graphs)

# --------------------------------------------------------------------------------------------------------------------



# --------------------------------------------------------------------------------------------------------------------

# c. Plot data (wind rose)

# --------------------------------------------------------------------------------------------------------------------

str(lg_1)
ggplot(lg_1, aes(dir)) + geom_histogram()

# Create data subset
brks <- c(0,  45,  90, 135, 180, 225, 270, 315, 360)
lbs  <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")

flow_1 <- lg %>%
  #filter( is.na(dir) == FALSE, is.na(speed) == FALSE) %>%  
  mutate(dir2 = (dir + 360/8/2)%%360) %>%
  mutate( dirbin = cut(dir2, breaks=brks, labels=lbs) )  %>%
  group_by(dirbin) %>%
  summarise(dircnt =  n())

ggplot(aes(x=dirbin, y=dircnt), data= flow_1) + geom_bar(width=1, stat = "identity") 

windRose(lg_1, ws = "speed", wd = "dir", breaks = c(0, 0.05, 0.1, 0.15, 0.2), width = 1.5)
windRose(lg_2, ws = "speed", wd = "dir", breaks = c(0, 0.05, 0.1, 0.15, 0.2), width = 1.5)
windRose(lg_3, ws = "speed", wd = "dir", breaks = c(0, 0.05, 0.1, 0.15, 0.2), width = 1.5)


windRose(lg, type = "Log", ws = "speed", wd = "dir", breaks = c(0, 0.05, 0.10, 0.15, 0.20), width = 1.5)
windRose(lg, ws = "speed", wd = "dir", breaks = c(0, 0.05, 0.1, 0.15, 0.2), width = 1.5)

# --------------------------------------------------------------------------------------------------------------------

# d. Plot data (violin plot)

# --------------------------------------------------------------------------------------------------------------------

str(lg)

lg <- group_by(lg, Log)

p <- ggplot(lg, aes (x = Log, y = speed, fill = Log)) + 
  geom_violin(trim = FALSE) + 
  labs(title = "Plot of frequency of speeds by logger", x = "Flow meter", y = "Velocity (ms )") +
  geom_boxplot(width = 0.1, fill = "white") + 
  theme_classic() +
  scale_x_discrete(limits = c("1", "2", "3")) +
  scale_fill_brewer(palette ="Greys")
p

p + scale_color_grey() + theme_classic() + geom_boxplot(width  = 0.1) +  scale_fill_brewer(palette ="Spectral")

# --------------------------------------------------------------------------------------------------------------------

# d. Plot data (cumulative sum)

# --------------------------------------------------------------------------------------------------------------------


lg_1 <- filter(lg, Log == "1")
lg_2 <- filter(lg, Log == "2")
lg_3 <- filter(lg, Log == "3")

lg_1 <- mutate(lg_1, c = cumsum(speed))
plot(lg_1$datetime, lg_1$c)
lg_2 <- mutate(lg_2, c = cumsum(speed))
plot(lg_2$datetime, lg_2$c)
lg_3 <- mutate(lg_3, c = cumsum(speed))
plot(lg_3$datetime, lg_3$c)

ggplot() + 
  geom_line(data=lg_1, aes(x = datetime, y = c), color = "gray77") +
  geom_line(data=lg_2, aes(x = datetime, y = c), color = "gray55") +
  geom_line(data=lg_3, aes(x = datetime, y = c), color = "gray8") +
  xlab("Time") +
  ylab("Cumulative sum flow velocity over time (m s−1)") +
  theme(legend.position="right") +
  theme_classic()



# f. Two sample Kolmogorov-Smirnov test 

# Test of time series are stationary
#Create a time series object in r for each meter 

## create a time series (ts) object from the meter data
ts_log1 <- ts(data= lg_1$speed, frequency=1440,
              start=c(4,11))
ts_log2 <- ts(data= lg_2$speed, frequency=1440,
              start=c(4,11))
ts_log3 <- ts(data= lg_3$speed, frequency=1440,
              start=c(4,11))

plot(ts_log1)


# Check to see if the time series is stationary 

library(tseries)
install.packages("tseries")

kpss.test(lg_1$speed, null = c("Level"))


kpss.test(ts_log1, null = c("Level"))
kpss.test(ts_log2, null = c("Level"))
kpss.test(ts_log3, null = c("Level"))

adf.test(ts_log1)
adf.test(ts_log2)
adf.test(ts_log3)

# Decompose each time series 

decom_1 <- decompose(ts_log1)
plot(decom_1, yax.flip = TRUE)

decom_2 <- decompose(ts_log2)
plot(decom_2, yax.flip = TRUE)

decom_3 <- decompose(ts_log3)

plot(decom_3, yax.flip = TRUE)

# Differencing for stationarity 

diff_log1 <- diff(ts_log1, lag = 1)

adf.test(diff_log1)
kpss.test(diff_log1)

acf(diff_log1)


diff_log2 <- diff(ts_log2, lag = 1)

adf.test(diff_log2)
kpss.test(diff_log2)

acf(diff_log2)


diff_log3 <- diff(ts_log3, lag = 1)

adf.test(diff_log3)
kpss.test(diff_log3)

acf(diff_log3)

## plot the differenced data
plot(diff_log1)

co2.acf <- acf(diff_log1)

# None of the time series are stationary 

# CONVERT THE DATA TO STATIONARY TIME SERIES
lg_1$sp1_rate <- log(lg_1$speed / lag(lg_1$speed))
kpss.test(lg_1$sp1_rate, null = c("Level"))

lg_2$sp2_rate <- log(lg_2$speed / lag(lg_2$speed))
kpss.test(lg_1$sp1_rate, null = c("Level"))

lg_3$sp3_rate <- log(lg_3$speed / lag(lg_3$speed))
kpss.test(lg_1$sp1_rate, null = c("Level"))

# rename loggers for pairwise comparisons

str(lg_1)
lg_a <- lg_1

lg_a$speed_1 <- lg_a$speed

str(lg_2)
lg_b <- lg_2

lg_b$speed_2 <- lg_b$speed

str(lg_3)
lg_c <- lg_3

lg_c$speed_3 <- lg_c$speed



library(lmtest)
# a vs b 

lg_ab <- cbind(lg_a, lg_b$sp2_rate)

grangertest( sp1_rate ~ lg_b$sp2_rate, order = 1,data = lg_ab)
str(lg_b)
lg_ab <- cbind(lg_a, lg_b$speed)

grangertest( speed ~ lg_b$speed, order = 1,dat2 = lg_ab)
grangertest( lg_b$speed ~ speed, order = 1,dat2 = lg_ab)

# b vs c 

lg_bc <- cbind(lg_b, lg_c$speed)

grangertest( speed ~ lg_c$speed, order = 1,dat2 = lg_bc)
grangertest( lg_c$speed ~ speed, order = 1,dat2 = lg_bc)

# a vs c

lg_ac <- cbind(lg_a, lg_c$speed)

grangertest( speed ~ lg_c$speed, order = 1,dat2 = lg_ac)
grangertest( lg_c$speed ~ speed, order = 1,dat2 = lg_ac)


####### Chi-squared test 
# 

ks.test(lg_ac$speed, lg_ac$`lg_c$speed`, alternative = "two.sided")
ks.test(lg_bc$speed, lg_bc$`lg_c$speed`, alternative = "two.sided")
ks.test(lg_ab$speed, lg_ab$`lg_b$speed`, alternative = "two.sided")

pa <- ecdf(lg_ac$speed)
pb <- ecdf(lg_bc$speed)
pc <- ecdf(lg_ac$`lg_c$speed`)

plot(pb)

plot(pb,pa,pc)

ggplot(lg_bc stat_ecdf(geom lg_ac, aes(speed), = "step")+
         l, color = "gray77")abs(stat_ecdf(lg_bc, aes(speed), geom = "step"), color = "gray55")+
  stat_ecdf(lg_ac, aes(lg_ac$`lg_c$speed`), geom = "step"), color = "gray8")+
  title="Empirical Cumulative \n Density Function",
  y = "F(Speed)", x="Speed") +
  them_classic()


plot(ecdf(lg_ac$speed), do.points=FALSE, verticals=TRUE, xlim=range(lg_ac$speed, lg_bc$speed), col = "gray77")
plot(ecdf(lg_bc$speed), do.points=FALSE, verticals=TRUE, add=TRUE, col = "gray55")kplot(ecdf(lg_ac$`lg_c$speed`), do.points=FALSE, verticals=TRUE, add=TRUE, col = "gray8")




















  # Granger test for causality 
  
  str(lg_1)
lg_a <- lg_1

lg_a$speed_1 <- lg_a$speed

str(lg_2)
lg_b <- lg_2

lg_b$speed_2 <- lg_b$speed

str(lg_3)
lg_c <- lg_3

lg_c$speed_3 <- lg_c$speed



# Create a time series object in r for each meter 

## create a time series (ts) object from the meter data
ts_log1 <- ts(data= lg_1$speed, frequency=1440,
              start=c(4,11))
ts_log2 <- ts(data= lg_2$speed, frequency=1440,
              start=c(4,11))
ts_log3 <- ts(data= lg_3$speed, frequency=1440,
              start=c(4,11))

plot(ts_log1)

# Check to see if the time series is stationary 

library(tseries)
install.packages("tseries")

kpss.test(lg_1$speed, null = c("Level"))


kpss.test(ts_log1, null = c("Level"))
kpss.test(ts_log2, null = c("Level"))
kpss.test(ts_log3, null = c("Level"))

adf.test(ts_log1)
adf.test(ts_log2)
adf.test(ts_log3)

# Decompose each time series 

decom_1 <- decompose(ts_log1)
plot(decom_1, yax.flip = TRUE)

decom_2 <- decompose(ts_log2)
plot(decom_2, yax.flip = TRUE)

decom_3 <- decompose(ts_log3)

plot(decom_3, yax.flip = TRUE)

# Differencing for stationarity 

diff_log1 <- diff(ts_log1, lag = 1)

adf.test(diff_log1)
kpss.test(diff_log1)

acf(diff_log1)


diff_log2 <- diff(ts_log2, lag = 1)

adf.test(diff_log2)
kpss.test(diff_log2)

acf(diff_log2)


diff_log3 <- diff(ts_log3, lag = 1)

adf.test(diff_log3)
kpss.test(diff_log3)

acf(diff_log3)



## plot the differenced data
plot(diff_log1)

co2.acf <- acf(diff_log1)

# None of the time series are stationary 


# CONVERT THE DATA TO STATIONARY TIME SERIES
lg_1$sp1_rate <- log(lg_1$speed / lag(lg_1$speed))
kpss.test(lg_1$sp1_rate, null = c("Level"))

lg_2$sp2_rate <- log(lg_2$speed / lag(lg_2$speed))
kpss.test(lg_1$sp1_rate, null = c("Level"))

lg_3$sp3_rate <- log(lg_3$speed / lag(lg_3$speed))
kpss.test(lg_1$sp1_rate, null = c("Level"))

#A more precise statement would be we are checking to see if including
#𝑥 is useful for predicting 𝑦 when 𝑦's own history is already being used for predictio
#n. That is, do not miss the fact the 𝑥 has to be useful beyond (or extra to) the own history of 𝑦.

library(lmtest)
# a vs b 

lg_ab <- cbind(lg_a, lg_b$sp2_rate)

grangertest( sp1_rate ~ lg_b$sp2_rate, order = 1,data = lg_ab)
str(lg_b)
lg_ab <- cbind(lg_a, lg_b$speed)

grangertest( speed ~ lg_b$speed, order = 1,dat2 = lg_ab)
grangertest( lg_b$speed ~ speed, order = 1,dat2 = lg_ab)

# b vs c 

lg_bc <- cbind(lg_b, lg_c$speed)

grangertest( speed ~ lg_c$speed, order = 1,dat2 = lg_bc)
grangertest( lg_c$speed ~ speed, order = 1,dat2 = lg_bc)

# a vs c

lg_ac <- cbind(lg_a, lg_c$speed)

grangertest( speed ~ lg_c$speed, order = 1,dat2 = lg_ac)
grangertest( lg_c$speed ~ speed, order = 1,dat2 = lg_ac)


####### Chi-squared test 
# 

ks.test(lg_ac$speed, lg_ac$`lg_c$speed`, alternative = "two.sided")
ks.test(lg_bc$speed, lg_bc$`lg_c$speed`, alternative = "two.sided")
ks.test(lg_ab$speed, lg_ab$`lg_b$speed`, alternative = "two.sided")

pa <- ecdf(lg_ac$speed)
pb <- ecdf(lg_bc$speed)
pc <- ecdf(lg_ac$`lg_c$speed`)

plot(pb)

plot(pb,pa,pc)

ggplot(lg_bc stat_ecdf(geom lg_ac, aes(speed), = "step")+
    l, color = "gray77")abs(stat_ecdf(lg_bc, aes(speed), geom = "step"), color = "gray55")+
  stat_ecdf(lg_ac, aes(lg_ac$`lg_c$speed`), geom = "step"), color = "gray8")+
  title="Empirical Cumulative \n Density Function",
  y = "F(Speed)", x="Speed") +
  them_classic()


plot(ecdf(lg_ac$speed), do.points=FALSE, verticals=TRUE, xlim=range(lg_ac$speed, lg_bc$speed), col = "gray77")
plot(ecdf(lg_bc$speed), do.points=FALSE, verticals=TRUE, add=TRUE, col = "gray55")kplot(ecdf(lg_ac$`lg_c$speed`), do.points=FALSE, verticals=TRUE, add=TRUE, col = "gray8")
                                                                  
                                                                                                                                      
                                                                                                                                      

























