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