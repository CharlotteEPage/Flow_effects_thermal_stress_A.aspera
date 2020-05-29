# -----------------------------
# 10_Flow_tank_conditions
# -----------------------------

# This script analyses the flow conditions set within each tank throughout the experimental period. 
# SB TS: Measured using video and dye to calculate speed (0.2 m away from irrigation setup)
# B TS: Measured with an ADV at a single point in the experimental area (0.2 m away from flow straightners)

#   a) Read in rda files
#   b) Calculate average and se flow speeds set in tanks

library(tidyr)
library(dplyr)


# Experiment 1: SB TS 
# --------------------------------------------------------------------------------------------------------------------

#   a) Read in rda files 

# --------------------------------------------------------------------------------------------------------------------

#load(file = "Data/B_TS_Tank_Flow_measurements.rda")

SB_TS_Flow <- read.csv("Data/SB_TS_Tank_Flow_measurements.csv")

str(SB_TS_Flow)

# --------------------------------------------------------------------------------------------------------------------

#   b) Calculate average and se flow speeds set in tanks

# --------------------------------------------------------------------------------------------------------------------

SB_TS_summ <- SB_TS_Flow %>% 
  group_by(Flow) %>%
  summarise(N    = length(Speed),
            mean = mean(Speed),
            sd   = sd(Speed),
            se   = sd / sqrt(N)) 

# Experiment 2: B TS 
# --------------------------------------------------------------------------------------------------------------------

#   a) Read in rda files 

# --------------------------------------------------------------------------------------------------------------------

load(file = "Data/B_TS_Tank_Flow_measurements.rda")

# Rename file
B_TS_Flow <- fl03_200 

# --------------------------------------------------------------------------------------------------------------------

#   b) Calculate average and se flow speeds set in tanks

# --------------------------------------------------------------------------------------------------------------------

B_TS_summ <- B_TS_Flow %>% 
  group_by(Flow) %>%
  summarise(N    = length(Vel_X),
    mean = mean(Vel_X),
    sd   = sd(Vel_X),
    se   = sd / sqrt(N)) 





