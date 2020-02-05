# --------------------------
# 03_PAM_data_analysis_SB_TS
# --------------------------

# A script for analysis of Pulse Amplitude (PAM) Fluorometry data for the sub-bleaching thermal stress experiment:

#   a) Read in rda files 
#   b) Calculate averages 
#   c) Plot data 
#   d) GLMM mixed effects model 

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

# --------------------------------------------------------------------------------------------------------------------

#   a) Read in rda files 

# --------------------------------------------------------------------------------------------------------------------


load(file = "Data/SB_TS_PAM_data.rda")
str(PAM_data)

# --------------------------------------------------------------------------------------------------------------------

#   b) Calculate averages 

# --------------------------------------------------------------------------------------------------------------------

# Group the data by day, trajectry and flow)
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
  geom_line(aes(color = Treat), size =0.8) +
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

# Check assumptions of the model 


# Run a model 
Model_1 <- lmer(Yield ~ Treatment*Flow*Day + Tank.. + (1|Flow/Treatment/Tank..), data = AP)

lsmeansobject <- lsmeans(Model_5, pairwise ~ Treatment * Flow | Day, adjust = "tukey")
lsmeansobject

library(lattice)
xyplot(resid(Model_4)~fitted(Model_4), type = c("p","smooth"))

# Bonferroni post hoc now 

dy_1$DT <- interaction(dy_1$Day, dy_1$Treatment, dy_1$Flow)


Model_6 <- lmer (value ~ 1 + DT + (1|Flow/Treatment/Tank), data = dy_1)

summary(dy_1)


library(multcomp)
library(foreign)

pairwise <- summary(glht(Model_6, linfct = mcp (DT = "Tukey")),test = adjusted("bonferroni"))




# increase max print option 
options(max.print = 1000000000)

# Start writing to an output file
sink("AcroFvFmpairwise")
summary(glht(Model_6, linfct = mcp (DT = "Tukey")),test = adjusted("bonferroni"))
# Stop writing to the file
sink()





