# -------------------------
# 01_Temperature_data_B_TS
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

# Code is repeated for each temperature treatment. 

# --------------------------------------------------------------------------------------------------------------------


