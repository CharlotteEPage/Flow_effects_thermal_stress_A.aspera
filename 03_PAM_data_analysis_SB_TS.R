# --------------------------
# 03_PAM_data_analysis_SB_TS
# --------------------------

# A script for analysis of Pulse Amplitude (PAM) Fluorometry data for the sub-bleaching thermal stress experiment:

#   a) Read in rda files 
#   b) Calculate averages 
#   c) Plot data 
#   d) GLMM mixed effects model 
#   e) Sequential Bonferoni post hoc test

# Load required packages 

library(reshape)
library(tidyverse)
library(dplyr)
library(tidyr) 
library(ggplot2)
library(Matrix)
library(lme4)
library(lmerTest)
library(lattice)
library(foreign)
library(reshape2)
library(pbkrtest)
library(olsrr)
library(emmeans)
library(multcomp)


# --------------------------------------------------------------------------------------------------------------------

#   a) Read in rda files 

# --------------------------------------------------------------------------------------------------------------------


load(file = "Data/SB_TS_PAM_data.rda")
str(PAM_data)

"PAM_data_SB_TS_1.csv"

load(file = "Data/PAM_data_SB_TS_1.rda")
PAM_data_1 <- PAM_data

# --------------------------------------------------------------------------------------------------------------------

#   b) Calculate averages 

# --------------------------------------------------------------------------------------------------------------------

# Group the data by day, trajectry and flow
dy_ave <- group_by(PAM_data, Day, Trajectory, Flow)

# Calculate yield and standard deviation
dy_ave <- summarise(dy_ave, Yield_ave = mean(Yield), Yield_sd = sd(Yield, na.rm = TRUE))

# Calculate the standard error
dy_ave <- mutate(dy_ave, Yield_err = Yield_sd/sqrt(length(Yield_sd)))

head(dy_ave)

# --------------------------------------------------------------------------------------------------------------------

#   c) Plot data 

# --------------------------------------------------------------------------------------------------------------------

# Join columns 
dy_ave <- unite(data = dy_ave, col = "Treat", c(2,3))

# Assign colours
group.colors <- c("deepskyblue4","deepskyblue3","green4", "green", "firebrick4", "indianred3")

legend_title <- "Treatment"
str(dy_ave)
ggplot(dy_ave, aes(x= Day, y= Yield_ave, group = Treat)) + 
  geom_errorbar(aes(ymin = Yield_ave - Yield_err, ymax = Yield_ave + Yield_err), width = 0.2) +
  scale_y_continuous(name = "Quantum Yield (Fv/Fm)", limits = c(0.3,0.7), breaks = seq(0.3,0.7,0.05)) +
  scale_x_continuous(name = "Day", breaks = seq(0,22,1)) +
  geom_line(aes(color = Treat)) +
  geom_point(aes(color = Treat)) +
  theme_classic() +
  scale_color_manual(legend_title, values= group.colors) +
  theme(axis.text.x = element_text(size = "10"),
        axis.text.y = element_text(size = "10"),
        axis.title.x = element_text(size = "10"),
        axis.title.y = element_text(size = "10"))

# --------------------------------------------------------------------------------------------------------------------

#   c) GLMM mixed effects model 

# --------------------------------------------------------------------------------------------------------------------

str(PAM_data)

# Make sure that day is specified as a factor 
PAM_data$Day <- as.factor(PAM_data$Day)

# Check for normality in data
hist(PAM_data$Yield)

# Find no significant effect of main effect Tank, so we can just include this within the random
# portion of the model 

# Make a treatment factor so that coral and tank can be nested within 

Model_1 <- lmer (Yield ~ Day * Trajectory * Flow + (1|Tank../Coral), data = PAM_data)

summary(Model_1)
anova(Model_1, type = "I")
hist(resid(Model_1))
qqnorm(resid(Model_1))
qqline(resid(Model_1))
plot(Model_1)

# Bonferroni post hoc test

PAM_data$DT <- interaction(PAM_data$Day, PAM_data$Treatment, PAM_data$Flow)

Model_2 <- lmer (Yield ~ 1 + DT + (1|Tank../Coral), data = PAM_data)

summary(dy_1)

pairwise <- summary(glht(Model_2, linfct = mcp (DT = "Tukey")),test = adjusted("bonferroni"))


# increase max print option 
options(max.print = 1000000000)

# Start writing to an output file
sink("AcroFvFmpairwise_SB_TS")
summary(glht(Model_2, linfct = mcp (DT = "Tukey")),test = adjusted("bonferroni"))
# Stop writing to the file
sink()





