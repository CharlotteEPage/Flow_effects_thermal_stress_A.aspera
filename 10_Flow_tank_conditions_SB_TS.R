# -----------------------------
# 10_Flow_tank_conditions_SB_TS
# -----------------------------

# A script for analysis of flow data for the bleaching thermal stress experiment:

#   a) Read in rda files of measured flow data
#   b) Calculate averages and standard errors for each 

# n.b Flow data was measured by filming dye released at the beginning of the experimental area, 
# and timing how long the dye took to move over a specificed distance (30 cm)

# Measurements in the attached csv file are initial measurements taken of flow across the tanks.
# Additional measurements taken across the experimental period can be supplied on request.

# Load required packages 

library(dplyr)
library(tidyr)

# --------------------------------------------------------------------------------------------------------------------

#   a) Read in rda files of measured flow data

# --------------------------------------------------------------------------------------------------------------------

load(file = "Data/B_TS_Tank_Flow_measurements.rda")

# --------------------------------------------------------------------------------------------------------------------

#   b) Calculate averages and standard errors for each 

# --------------------------------------------------------------------------------------------------------------------

fl03_200 <- group_by(fl03_200, Flow)

means <- summarise(fl03_200, mean = mean(Vel_X), sd = sd(Vel_X)) 
means <- mutate(means, se = sd/sqrt(12))