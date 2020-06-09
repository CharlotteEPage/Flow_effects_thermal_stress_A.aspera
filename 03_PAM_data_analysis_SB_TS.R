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
library(betareg)


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
  scale_x_continuous(name = "Day", limits = c(0,24), breaks = seq(0,24,1)) +
  geom_line(aes(color = Treat)) +
  geom_point(aes(color = Treat)) +
  theme_classic() +
  scale_color_manual(legend_title, values= group.colors) +
  theme(axis.text.x = element_text(size = "10"),
        axis.text.y = element_text(size = "10"),
        axis.title.x = element_text(size = "10"),
        axis.title.y = element_text(size = "10"))


# Assign colours
group.colors <- c("deepskyblue4","deepskyblue3","gray35", "gray55", "firebrick4", "indianred3")

legend_title <- "Treatment"
str(dy_ave)
ggplot(dy_ave, aes(x= Day, y= Yield_ave, group = Treat)) + 
 # geom_errorbar(aes(ymin = Yield_ave - Yield_err, ymax = Yield_ave + Yield_err), width = 0.1) +
  scale_y_continuous(name = "Quantum Yield (Fv/Fm)", limits = c(0.3,0.7), breaks = seq(0.3,0.7,0.05)) +
  scale_x_continuous(name = "Day", limits = c(0,24), breaks = seq(0,24,1)) +
  geom_line(aes(color = Treat)) +
  geom_point(aes(color = Treat)) +
  theme_classic() +
  scale_color_manual(legend_title, values = group.colors) +
  theme(axis.text.x = element_text(size = "15"),
        axis.text.y = element_text(size = "15"),
        axis.title.x = element_text(size = "15"),
        axis.title.y = element_text(size = "15")) + 
  geom_pointrange(aes(ymin=Yield_ave - Yield_err, ymax = Yield_ave + Yield_err))

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

pairwise <- summary(glht(Model_2, linfct = mcp (DT = "Tukey")),test = adjusted("bonferroni"))


# increase max print option 
options(max.print = 1000000000)

# Start writing to an output file
sink("AcroFvFmpairwise_SB_TS.txt")
summary(glht(Model_2, linfct = mcp (DT = "Tukey")),test = adjusted("bonferroni"))
# Stop writing to the file
sink()




# Betareg 
Model_3 <- betareg(Yield ~ Day * Trajectory * Flow + (1|Coral), data = PAM_data, link="logit")

summary(MPmod)
#The estimate of the precision parameter in beta regression is akin to 'variance explained' 
#in linear models. A positive estimate for the precision parameter indicates that 
#the inclusion of the explanatory variables (i.e. covariates) increases the precision
##of the model and reduces variance in predicted values

plot(MPmod)
#We expect standardised weighted residuals for beta regression to be centred around zero
#For a continous predictor, we would test for linearity between the dependent and independent variables
#on the log scale (due to logit link). But as these predictors are categorical, this is not possible.
#Instead, look at the residuals (deviance) vs linear predictor plot produced by plot(model)
#Look for any severe deviations from being centred around zero in each factorial group

##Residuals, cook's and leverage diagnostics all skewed by the outlier, sample 555
##Attempt without for reporting purposes

##Pairwise contrasts for the above model us lsmeans from the emmeans package
lsmMP <- lsmeans(MPmod, ~Treatment | Time.Point)
MPcont <- contrast(lsmMP, interaction = "pairwise",adjust="Tukey")
MPcont
##No significant difference, as expected given the results of Leggat et al. (2019)
##When interpreting these contrasts and back-transforming the estimates (exp(est)), remember these
##are calculated off the estimated marginal means in the model. Meaning the estimated % change given
##by back transforming will not precisely match the change in the raw data

summary(dy_1)

pairwise <- summary(glht(Model_2, linfct = mcp (DT = "Tukey")),test = adjusted("bonferroni"))


# increase max print option 
options(max.print = 1000000000)

# Start writing to an output file
sink("AcroFvFmpairwise_SB_TS")
summary(glht(Model_2, linfct = mcp (DT = "Tukey")),test = adjusted("bonferroni"))
# Stop writing to the file
sink()







