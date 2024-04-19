# t-test for ozone
# Mike
# 18-04-2024


# load packages
library(tidyverse)
library(here)
library(ggfortify)
library(cowplot)
library(mbhR)


# load data
filepath <- here("data","ozone.csv")
ozone <- read_csv(filepath)
glimpse(ozone)


# plot the data
ozone %>%
  ggplot(aes(x = garden.location, y = ozone)) +
  geom_boxplot() +
  labs(x = "Garden location",
       y = "Ozone concentration (ppb)") 


# diagnostics


# t-test
t.test(ozone~garden.location,data=ozone)
