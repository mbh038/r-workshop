# wolf data linear regression example
# Michael Hunt
# 23-04-2024


# data from Flagstad et al (2023) DOI: 10.1046/j.1365-294X.2003.01784.x

# load packages
library(tidyverse)
library(here)
library(ggfortify)
library(cowplot)

# load data
filepath <- here("data", "wolf_heterozygosity.csv")
wolf <- read_csv(filepath,skip=1)
glimpse(wolf)

# plot the data
wolf %>%
  ggplot(aes(x=year, y=pc_hetero)) +
  geom_point() +
  labs(x = "Year",
       y = "Individual heterozygosity (%)",
       caption = "Flagstad et al (2023) DOI: 10.1046/j.1365-294X.2003.01784.x") +
  theme_cowplot()

# fit the model
wolf.model <- lm (pc_hetero ~ year, data = wolf)

# diagnostics
autoplot(wolf.model) + theme_cowplot()

# investigate the model
anova(wolf.model)
summary(wolf.model)

# replot the data, now with the model included
# plot the data
wolf %>%
  ggplot(aes(x=year, y=pc_hetero)) +
  geom_point() +
  geom_smooth(method="lm",linewidth=0.4,se=FALSE) +
  labs(x = "Year",
       y = "Individual heterozygosity (%)",
       caption = "Flagstad et al (2023) DOI: 10.1046/j.1365-294X.2003.01784.x") +
  scale_x_continuous(breaks=c(seq(1820,1960,20))) +
  scale_y_continuous(limits=c(0,100),breaks=c(seq(0,100,20))) +
  theme_cowplot()

ggsave(here("figures","wolf_heterzygosity.png"),width=4,height=3)
