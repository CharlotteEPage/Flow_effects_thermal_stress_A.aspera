# --------------------------
# 04_PAM_data_analysis_B_TS
# --------------------------

# A script for analysis of Pulse Amplitude (PAM) Fluorometry data for the bleaching thermal stress experiment:

#   a) Read in rda files 
#        i) Dark adapted 
#        ii) Induction Recovery curves 
#   b) Calculate averages 
#        i) Dark adapted 
#        ii) Induction Recovery curves 
#   c) Plot data 
#        i) Dark adapted 
#        ii) Induction Recovery curves 
#   d) GLMM mixed effects model 
#        i) Dark adapted 
#        ii) Induction Recovery curves 

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
library(reshape2)
library(pbkrtest)
library(multcomp)
library(foreign)

# This script downloads all light curve data files, plots graphs, extracts points needed for analysis 
# and does the analysis. 


# --------------------------------------------------------------------------------------------------------------------

#   a) Read in rda files 

# --------------------------------------------------------------------------------------------------------------------

# ---------------------------
# Dark adapted data analysis 
# ---------------------------

# Load dark adapted yeild PAM data 
load(file = "Data/B_TS_PAM_data_1.rda")
str(dy_1)

PAM_data <- dy_1

# ---------------------------
# Light Curve data analysis 
# ---------------------------

# Days of interests for this experiment are 
# a) Day 6 - 9/03/2019
# b) Day 12 - 15/03/2019
# c) Day 15 - 18/03/2019
# a) Day 16 - 19/03/2019
# b) Day 17 - 20/03/2019
# c) Day 18 - 21/03/2019
# c) Day 19 - 22/03/2019
# c) Day 20 - 23/03/2019

# Analysis is conducted on point 27. 
# 1 - Dark adapted yield 
# 15 - After light stress 
# 27 - Recovery after light stress

# Load dark adapted yeild PAM data 
load(file = "Data/B_TS_IR_PAM_data.rda")
str(IR_PAM_data)

# --------------------------------------------------------------------------------------------------------------------

#   b) Calculate averages 

# --------------------------------------------------------------------------------------------------------------------

# ---------------------------
# Dark adapted data analysis 
# ---------------------------

# Group the data by day, trajectory and flow)
PAM_data_plot <- group_by(PAM_data, Day, Treatment, Flow)

# Calculate yield and standard deviation
PAM_data_sum <- summarise(PAM_data_plot, Yield_ave = mean(average_yield), Yield_sd = sd(average_yield, na.rm = TRUE))

# Calculate the standard error
PAM_data_sum <- mutate(PAM_data_sum, Yield_err = Yield_sd/sqrt(length(Yield_sd)))

head(PAM_data_sum)

# ---------------------------
# Light Curve data analysis 
# ---------------------------

str(IR_PAM_data)

# Group the data by day, trajectory and flow)
PAM_IR_data_plot <- group_by(IR_PAM_data, Day, Treat, Flow, No.)

# Calculate yield and standard deviation
PAM_IR_data_sum <- summarise(PAM_IR_data_plot, Yield_ave = mean(yield), Yield_sd = sd(yield, na.rm = TRUE))

# Calculate the standard error
PAM_IR_data_sum  <- mutate(PAM_IR_data_sum , Yield_err = Yield_sd/sqrt(length(Yield_sd)))

head(PAM_IR_data_sum)

# --------------------------------------------------------------------------------------------------------------------

#   c) Plot data 

# --------------------------------------------------------------------------------------------------------------------

# ---------------------------
# Dark adapted data analysis 
# ---------------------------

# Join columns 
PAM_data_sum <- unite(data = PAM_data_sum, col = "Treat", c(2,3))

# Assign colours
group.colors <- c("deepskyblue4","deepskyblue3","firebrick4", "indianred3")

legend_title <- "Treatment"

ggplot(PAM_data_sum, aes(x= Day, y= Yield_ave, group = Treat)) + 
  geom_errorbar(aes(ymin = Yield_ave - Yield_err, ymax = Yield_ave + Yield_err), width = 0.2) +
  scale_y_continuous(name = "Quantum Yield (Fv/Fm)", limits = c(0.1,0.7), breaks = seq(0.1,0.7,0.05)) +
  scale_x_continuous(name = "Day", breaks = seq(0,22,1)) +
  geom_line(aes(color = Treat)) +
  geom_point(aes(color = Treat)) +
  theme_classic() +
  scale_color_manual(legend_title, values= group.colors) +
  theme(axis.text.x = element_text(size = "10"),
        axis.text.y = element_text(size = "10"),
        axis.title.x = element_text(size = "10"),
        axis.title.y = element_text(size = "10"))


group.colors <- c("deepskyblue4","deepskyblue3", "firebrick4", "indianred3" )
group.line <- c("solid", "dashed","solid", "dashed","solid", "dashed")
legend_title <- "Treatment"

ggplot(dy_ave_ave, aes(x= Day, y= Yield_ave, group = Treat)) + 
  geom_errorbar(aes(ymin = Yield_ave - Yield_err, ymax = Yield_ave + Yield_err), width=.1) +
  scale_y_continuous(name = "Yield", limits = c(0.1,0.7), breaks = seq(0.1,0.7,0.1)) +
  geom_line(aes(color = Treat)) +
  geom_point(aes(color = Treat)) +
  theme_classic() +
  scale_color_manual(legend_title, values= group.colors) 

# ---------------------------
# Light Curve data analysis 
# ---------------------------

head(PAM_IR_data_sum)

# Filter out point 27 from No. 

PAM_final_point <- filter(PAM_IR_data_sum, No. == 27) 

PAM_final_points <- unite(data = PAM_final_point, col = "Treat", c(2,3))

group.colors <- c("deepskyblue4","deepskyblue3", "firebrick4", "indianred3" )
group.line <- c("solid", "dashed","solid", "dashed","solid", "dashed")
legend_title <- "Treatment"

ggplot(PAM_final_points , aes(x= Day, y= Yield_ave, group = Treat)) + 
  geom_errorbar(aes(ymin = Yield_ave - Yield_err, ymax = Yield_ave + Yield_err), width=.1) +
  scale_y_continuous(name = "Yield", limits = c(0.1,0.7), breaks = seq(0.1,0.7,0.1)) +
  geom_line(aes(color = Treat)) +
  geom_point(aes(color = Treat)) +
  theme_classic() +
  scale_color_manual(legend_title, values= group.colors) 

# --------------------------------------------------------------------------------------------------------------------

#   c) GLMM mixed effects model 

# --------------------------------------------------------------------------------------------------------------------

# ---------------------------
# Dark adapted data analysis 
# ---------------------------

PAM_data

str(PAM_data)
PAM_data$Flow <- as.factor(PAM_data$Flow)

# Model is singular 
# Try recoding tank levels

str(PAM_data)

PAM_HT <- filter(PAM_data, Treatment == "HT") %>% 
  mutate(Tank = fct_recode(Tank, "1" = "3"))

library(plyr)
PAM_HT <- mutate(PAM_HT, Tank_1 = revalue(Tank, c("1" = "3", "2" = "4")))

PAM_AMB <- filter(PAM_data, Treatment == "AMB")
PAM_AMB <- mutate(PAM_AMB, Tank_1 = revalue(Tank, c("1" = "1", "2" = "2")))

# Bind PAM_HT and PAM_AMB together 

PAM_data_1 <- rbind(PAM_AMB,PAM_HT)

# Make tank a factor 

PAM_data_1$Tank_1 <- as.factor(PAM_data_1$Tank_1)
str(PAM_data_1)

# Look at the distribution of the data
hist(PAM_data_1$average_yield, breaks = 100)
# The data seems to be negatively skewed. transform data

PAM_data_2 <- mutate(PAM_data_1, new = average_yield^(1/3))
hist(PAM_data_2$new, breaks = 100)


library("fitdistrplus")
descdist(PAM_data_1$average_yield, discrete = F, boot = 1000)

# Boundary is singular fit 
library(blme)


str(PAM_data_1)
PAM_data_1$Day <- as.factor(PAM_data_1$Day)

Model_1<- lmer(average_yield ~ Treatment*Flow*Day + (1|Tank/Coral), data = PAM_data_1, REML = FALSE)
Model_2 <- blmer(average_yield ~ Treatment*Flow*Day + (1|Tank/Coral), data = PAM_data_1, REML = FALSE)

summary(Model_1)
anova(Model_1, type = "I")

 summary(Model_2)
anova(Model_2)

# Probably due to low sample numbers (and variable levels)
# Still seems to be singular - instead we will try  Bayesian
# framework using Markov chain Monte Carlo (MCMC) methods in the R package MCMCglmm

Model_1<- blmer(average_yield ~ Treatment*Flow*Day + (1|Coral), data = PAM_data_1, REML = FALSE)


summary(Model_1)
plot(Model_1)
hist(resid(Model_1), breaks = 100)
qqnorm(resid(Model_1))
qqline(resid(Model_1))
anova(Model_1, type = "I")


xyplot(resid(Model_1)~fitted(Model_1), type = c("p","smooth"))

# Bonferroni post hoc - pairwise comparisons

PAM_data$DT <- interaction(PAM_data$Day, PAM_data$Treatment, PAM_data$Flow)

str(PAM_data)

Model_2<- lmer(average_yield ~ 1 + DT + (1|Flow/Treatment/Tank), data = PAM_data)

AIC(Model_1)
AIC(Model_2)

pairwise <- summary(glht(Model_2, linfct = mcp (DT = "Tukey")),test = adjusted("bonferroni"))

pairwise

# increase max print option 
options(max.print = 1000000000)

# Start writing to an output file
sink("AcroFvFmpairwise")
summary(glht(Model_2, linfct = mcp (DT = "Tukey")),test = adjusted("bonferroni"))
# Stop writing to the file
sink()

# ---------------------------
# Light Curve data analysis 
# ---------------------------

IR_PAM_data$Day <- as.factor(IR_PAM_data$Day)
# str(IR_PAM_data)

str(IR_PAM_data)

mod_2<- lmer(yield ~ Day * Flow * Treat + (1|Tank/Coral), data = IR_PAM_data, REML = FALSE)


summary(mod_2)
plot(mod_2)
hist(resid(mod_2), breaks = 10)
qqnorm(resid(mod_2))
qqline(resid(mod_2))
anova(mod_2, type = "I")



hist(resid(mod_2),breaks=100,density=T)
plot(density(resid(mod_2)))
summary(mod_2)



Model_3 <- lm(yield ~ Flow * Treat * Day + (1|Tank/Coral), data = IR_PAM_data)
str(IR_PAM_data)

vcov(Model_3)
summary(Model_3)
plot(Model_3)
hist(resid(Model_3), breaks = 100)
qqnorm(resid(Model_3))
qqline(resid(Model_3))
anova(Model_3, type = "I")

coefficients(Model_3)



Model_7 <- lm(yield ~ Flow * Treat * Day + (1 +Flow|Tank/Coral), data = IR_PAM_data)
str(IR_PAM_data)

vcov(Model_3)
summary(Model_3)
plot(Model_3)
hist(resid(Model_3), breaks = 100)
qqnorm(resid(Model_3))
qqline(resid(Model_3))
anova(Model_3, type = "I")

coefficients(Model_3)

IR_PAM_data$DT <- interaction(IR_PAM_data$Treat , IR_PAM_data$Flow)

Mod <- lmer (yield ~ 1 + DT + (1|Tank/Treat/Flow), data = IR_PAM_data)
summary(Mod)
pairwise <- summary(glht(Mod, linfct = mcp (DT = "Tukey")),test = adjusted("bonferroni"))

pairwise
