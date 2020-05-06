# -----------------------------------
# 05_Endosymbiont_data_analysis_SB_TS
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

load("Data/SB_TS_Endosymbiont_sa.rda")
load("Data/SB_TS_Endosymbiont_counts.rda")

# --------------------------------------------------------------------------------------------------------------------

#   b) Calculate the mean number of zoox counted for each chamber

# --------------------------------------------------------------------------------------------------------------------


# Unite the treatment columns to find the average per sample 
#counts <- unite(data = counts, col = "Treat", c(1:5))

# Mean per square per sample 

# Group by the Treatment
counts <- group_by(counts, Timepoint, Species, Trajectory, Tank, Flow, Coral)

# Times by the dilution factor
counts <- mutate(counts, dil = Count * Dilution)

# Average for each sample
counts_ave <- summarize(counts, ave_sample = mean(dil))

str(counts_ave)
# Split columns back 

#counts_ave <- counts_ave %>% separate(Treat, 
#c("Timepoint", "Treatment","Tank", "Flow", "Coral"))

# --------------------------------------------------------------------------------------------------------------------

#   c) Multiply the mean obtained by 250 and then convert from ml^-2 to cm^-2

# --------------------------------------------------------------------------------------------------------------------

# * 250(mean per little square) * dilution * 1000 (ml2 to cm2)

# Make a new column with counts 250

counts_ave <- mutate(counts_ave, per_ml = ave_sample * 250)

# Make a new column with counts x 1000 (ml2 to cm2)

counts_ave <- mutate(counts_ave, per_cm = per_ml*1000)

# --------------------------------------------------------------------------------------------------------------------

#   d) Then divide by the surface area of the fragment. 

# --------------------------------------------------------------------------------------------------------------------

str(sa)
sa <- unite(data = sa, col = "Time.point", c(1,2,3,6,4,5), sep = "_")
counts_ave <- unite(data = counts_ave, col = "Time.point", c(1:6), sep = "_")

str(counts_ave)
str(sa)

full <- merge(counts_ave, sa, by = "Time.point" )
str(full)


full$SA <- full$S.A...cm2..est.

density <- mutate(full, density = per_cm/SA)


# --------------------------------------------------------------------------------------------------------------------

# e) Plot this data as box plots with error bars 

# --------------------------------------------------------------------------------------------------------------------

str(density)

final <- density %>% separate(Time.point, c("Timepoint", "Species","Treatment","Tank", "Flow", "Coral"), sep = "_")
#select(Timepoint, Treatment, Tank, Flow, Coral, density)

str(final)
final2 <- unite(data = final, col = "Treat", c(1,2,3,5))

ggplot(final2, aes(x = Treat, y = density)) +
  geom_boxplot()


final2 <- mutate (final2, adjusted = density/1000000)

ggplot(final2, aes(x = Treat, y = adjusted)) +
  geom_boxplot()

# Coloured plot
ggplot(final2, aes(x = Treat, y = adjusted, group = Treat)) +
  geom_boxplot(col =  c("deepskyblue4","deepskyblue3","gray35", "gray55", "firebrick4", "indianred3","deepskyblue4","deepskyblue3","gray35", "gray55", "firebrick4", "indianred3")) +
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

mod1 <- lmer(density ~ Timepoint * Treatment * Flow + (1|Tank), data = final, REML = FALSE)

summary(mod1)
plot(mod1)
hist(resid(mod1), breaks = 10)
qqnorm(resid(mod1))
qqline(resid(mod1))
a <- anova(mod1, type = "I")
a
write.csv(as.matrix(a), file = "Endo_anova.csv", na = "")


coefficients(mod1)

final$DT <- interaction(final$Timepoint,final$Treatment , final$Flow)

Mod <- lmer (density ~ 1 + DT + (1|Tank), data = final)
summary(Mod)
pairwise <- summary(glht(Mod, linfct = mcp (DT = "Tukey")),test = adjusted("bonferroni"))

pairwise

# Boundary is singular fit 
library(blme)
install.packages("blme")

str(final)

Mod_2 <- lmer(density ~ Timepoint * Treatment * Flow + (1|Tank), data = final, REML = TRUE)
anova(Mod_2)



