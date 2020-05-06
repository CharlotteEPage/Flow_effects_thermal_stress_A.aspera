# -----------------------------------
# 06_Endosymbiont_data_analysis_B_TS
# -----------------------------------

# This script calculates endosymbiont densities for fragments of known 
# surface area (using the wax technique).
# Counts taken using haemocytometer. 
# Pellet resuspended in 15/10 (see dilution column of data) ml of 40 micron filtered sea water, loaded onto 
# 6 haemocytometer grids, 3 squares counted per grid. 

# We want to calculate the number of zooxanthellae per ml of original sample:

# a. Load counts and surface area data into R 
# b. Calculate the mean number of zoox counted for each chamber
# c. Multiply the mean obtained by 250 * 1000 to obtain the number of cells per cm^-2 of diluted sample
# d. Then divide by the surface area of the fragment. 
# e. Plot this data as boxplots with error bars 
# f. Conduct statistical analysis to look for statistical differences 
# between controls and respective treatments at each time point 
# between treatments at respective timepoints

# Libraries to load 
library(tidyr)
library(ggplot2)
library(dplyr)
library(lme4)
library(lmerTest)
library(lme4)
library(lmerTest)
library(multcomp)
library(foreign)
library(tidyr)
library(reshape2)

# --------------------------------------------------------------------------------------------------------------------

#   a) Read in rda files 

# --------------------------------------------------------------------------------------------------------------------

load("Data/B_TS_endosymbiont_sa.rda")
load("Data/B_TS_endosymbiont_counts.rda")

# --------------------------------------------------------------------------------------------------------------------

#   b) Calculate the mean number of zoox counted for each chamber

# --------------------------------------------------------------------------------------------------------------------

# Unite the treatment columns to find the average per sample 
counts <- unite(data = counts, col = "Treat", c(1:5))

# Mean per square per sample 

# Group by the Treatment
counts <- group_by(counts, Treat)

# Average for each sample
counts_ave <- summarize(counts, ave_sample = mean(Count))

# Split columns back 

counts_ave <- counts_ave %>% separate(Treat, 
                                      c("Timepoint", "Treatment","Tank", "Flow", "Coral"))
# --------------------------------------------------------------------------------------------------------------------

#   c) Multiply the mean obtained by 250 and then convert from ml^-2 to cm^-2

# --------------------------------------------------------------------------------------------------------------------

# * 250(mean per little square) * dilution * 1000 (ml2 to cm2)

# Make a new column with counts 250

counts_ave <- mutate(counts_ave, per_ml = ave_sample * 250)

# Make a new column with counts x 1000 (ml2 to cm2)

counts_ave <- mutate(counts_ave, per_cm = per_ml*1000)


counts_ave <- mutate(counts_ave, per_cm = per_cm*15)

# --------------------------------------------------------------------------------------------------------------------

#   d) Then divide by the surface area of the fragment. 

# --------------------------------------------------------------------------------------------------------------------


str(counts_ave)
str(sa)

sa <- unite(data = sa, col = "Treat", c(1:5))
counts_ave <- unite(data = counts_ave, col = "Treat", c(1:5))

str(counts_ave)
str(sa)

full <- merge(counts_ave, sa, by = "Treat")

full$SA <- full$S.A...cm2..est.

density <- mutate(full, density = per_cm/SA)

# --------------------------------------------------------------------------------------------------------------------

# e) Plot this data as box plots with error bars 

# --------------------------------------------------------------------------------------------------------------------

final <- density %>% separate(Treat, 
                              c("Timepoint", "Treatment","Tank", "Flow", "Coral"))

library(tidyr)
str(final)
final <- select(final, Timepoint, Treatment, Tank, Flow, Coral, density)


final2 <- unite(data = final, col = "Treat", c(1,2,4))

ggplot(final2, aes(x = Treat, y = density)) +
  geom_boxplot()

final2 <- mutate (final2, adjusted = density/1000000)

ggplot(final2, aes(x = Treat, y = adjusted)) +
  geom_boxplot()

ggplot(final2, aes(x = Treat, y = adjusted, group = Treat)) +
  geom_boxplot(col=  c("deepskyblue4","deepskyblue3", "firebrick4", "indianred3","deepskyblue4","deepskyblue3", "firebrick4", "indianred3")) +
  theme_classic() 
# --------------------------------------------------------------------------------------------------------------------

# f. Conduct statistical analysis to look for statistical differences 

# --------------------------------------------------------------------------------------------------------------------

# Conduct statistical analysis to look for statistical differences 
# between controls and respective treatments at each time point 

# Mixed effects ANOVA

# Fixed effects: Timepoint, Treatment (flow nested within treatment)
# Random effects: Tank (coral nested within tank)

str(final)
final$Timepoint <- as.factor(final$Timepoint)
final$Treatment <- as.factor(final$Treatment)
final$Flow <- as.factor(final$Flow)
final$Tank <- as.factor(final$Tank)
final$Coral <- as.factor(final$Coral)

str(final)

install.packages("parameters")
library(parameters)

mod1 <-      lmer(density ~ Timepoint * Treatment * Flow + (1|Tank), data = final, REML = FALSE)

summary(mod1)
plot(mod1)
hist(resid(mod1), breaks = 10)
qqnorm(resid(mod1))
qqline(resid(mod1))
anova(mod1, type = "I")

b <- anova(mod1, type = "I")

write.csv(as.matrix(b), file = "ENDO.csv", na = "")



parameters::p_value(mod1)

coefficients(mod1)



final$DT <- interaction(final$Timepoint,final$Treatment , final$Flow)

Mod <- lmer (density ~ 1 + DT + (1|Tank), data = final)
summary(Mod)
pairwise <- summary(glht(Mod, linfct = mcp (DT = "Tukey")),test = adjusted("bonferroni"))

pairwise





mod2 <- lmer(density ~ Timepoint + Treatment + Flow + Timepoint:Treatment + Timepoint:Flow + Flow:Treatment +(1|Tank), data = final)

mod3 <- lmer(density ~ Timepoint + Treatment + Flow + Timepoint:Treatment + Flow:Treatment +(1|Tank), data = final)

mod4 <- lm(density ~ Timepoint + Treatment + Flow + Timepoint:Treatment + Flow:Treatment, data = final)


