# ----------------------------
# 07_Reef_flat_flow_conditions 
# ----------------------------

# A script for analysis of flow conditions on Reef Flat 

# Data collected using an ADV, transects running from beach towards reef slope. 
# 3 transects collected at different sites along Heron Island Reef flat (Scientific Zone) at mid-tide. 

# a. Read in data (.rda file)
# b. Calculate summary statistics + plotting

library(lubridate)
library(plyr)
library(dplyr)
library(lattice)
library(openair)
library(ggplot2)

# --------------------------------------------------------------------------------------------------------------------

#   a) Read in rda files 

# --------------------------------------------------------------------------------------------------------------------

load("Data/Reef_flat_flow_conditions.rda")

# --------------------------------------------------------------------------------------------------------------------

# b. Plotting

# --------------------------------------------------------------------------------------------------------------------

str(sites)

# Average of all sites 

sites_ave <- summarise(sites, mean_flow = mean(VelX), sdev = sd(VelX))
sites_ave <- mutate(sites_ave, err = sdev/sqrt(3))


Legend_title <- "Site"
group.colors <- c("deepskyblue3","deepskyblue4","dimgrey")

ggplot(sites, aes(x = St, y = VelX, group = site, color = site)) + 
  geom_errorbar(aes(ymin = VelX - VxErr, ymax = VelX + VxErr), width = .1) +
  geom_smooth () +
  geom_point () +
  scale_x_continuous(name = "Distance (m)", breaks = seq(0,6,1)) +
  scale_y_continuous(name = "Velocity (ms )", breaks = seq(0.0, 0.5,0.05)) +
  scale_color_manual(Legend_title, values = group.colors) +
  theme_classic()

ggplot(sites, aes(x = site, y = VelX, group = site, color = site)) + 
  geom_boxplot() + 
  scale_y_continuous(name = "Velocity (ms )", breaks = seq(0.0, 0.5,0.05)) +
  scale_color_manual(Legend_title, values = group.colors) +
  theme_classic() + 
  xlab("Site")

str(sites)
sites_ave <- group_by(sites, St)

sites_ave <- summarise(sites_ave, mean_flow = mean(VelX), sdev = sd(VelX))
sites_ave <- mutate(sites_ave, err = sdev/sqrt(3))

ggplot(sites_ave, aes(x = St, y = mean_flow)) + 
  geom_errorbar(aes(ymin = mean_flow - err, ymax = mean_flow + err), width = .1) +
  geom_smooth () +
  geom_point () +
  scale_x_continuous(name = "Distance (m)", breaks = seq(0,6,1)) +
  scale_y_continuous(name = "Velocity (ms )", breaks = seq(0.0, 0.5,0.05)) +
  scale_color_manual(Legend_title, values = group.colors) +
  theme_classic()


# Remove points from 1 and plot 

str(sites)

sites_1 <- filter(sites, St >1)

sites_mean_1 <- summarise (sites_1, mean_flow = mean(VelX), sted = sd(VelX))
sites_mean_1 <- mutate(sites_mean_1, err = sted/sqrt(17))

sites_mean_1

sites$St <- as.factor(sites$St)

mod_1 <- lm(VelX ~ site + St, data = sites)

summary(mod_1)
anova(mod_1)

hist(resid(mod_1))
qqnorm(resid(mod_1))
qqline(resid(mod_1))

# Post hoc analysis on model
summary(glht(mod_1, linfct=mcp(site="Tukey")))
summary(glht(mod_1, linfct=mcp(St="Tukey")))

