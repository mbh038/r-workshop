# your name
# the date
# Script to find correlation coefficients




##
##
## Example of normally distributed data - no evidence of correlation
##
##

# Question: Is tree height correlated with tree diameter at breast height?

# load packages
library(tidyverse)
library(here)
library(cowplot)

# get some data - we choose tree data
# your data file should be a csv file, inside the data subolder of your project
filepath <- here("data","tree_vol.csv")
trees <- read_csv(filepath)
glimpse(trees)

# plot the data

trees %>%
  ggplot(aes(x = diameter, y = height)) + # what we want to plot
  geom_point() + # what kind of plot
  labs(x = "Tree diameter (m)",
       y = "Tree height (m)") + # axis labels
  theme_cowplot() # give the plot a nice 'look'

# does it look as though tree diameter and height are linearly correlated?


# check for normality
shapiro.test(trees$diameter) 
shapiro.test(trees$height)

# if p > 0.05 in both cases, then we can assume normality 

# if both are distributed normally, then we check for Pearson's r, if not
# we check for Spearman's Rank r

# find correlation coefficient
cor.test(trees$diameter,trees$height, method = "pearson")

# interpret the output

# what is r?
# is p < 0.05?

##
##
## Example of normally distributed data - where there is evidence of correlation
##
##

# Question: Is virginica petal width correlated with virginica petal length?

# load packages
library(tidyverse)
library(here)
library(cowplot)

# we choose the iris data set
filepath <- here("data","iris.csv")
iris <- read_csv(filepath)
glimpse(iris)

# let us select the data for the setosa species
virginica <- iris %>%
  filter(Species == "virginica")

# plot the data - petal length vs petal width
virginica %>%
  ggplot(aes(x = Petal.Width, y = Petal.Length)) + # what we want to plot
  geom_point() + # what kind of plot
  labs(x = "Petal diameter (cm)",
       y = "Petal width (cm)") + # axis labels
  theme_cowplot() # give the plot a nice 'look'


# does it look as though petal width and length are linearly correlated?


# check for normality
shapiro.test(virginica$Petal.Length)  # p > 0.05?
shapiro.test(virginica$Petal.Width) # p > 0.05?


# find correlation coefficient - pearson's r
cor.test(virginica$Petal.Length,virginica$Petal.Width, method = "pearson")


##
##
## Example of correlation test where we have to use Spearman's rank r
##
##

# Question: Is WEMWBS score correlated with nep score?

# load packages
library(tidyverse)
library(here)
library(cowplot)

# we choose the marine wellness data set - where the data are ordinal data
filepath <- here("data","marine_wellness.csv")
wellness <- read_csv(filepath)
glimpse(wellness)

# we have wellness scores for 288 individuals according to two scales - WEMWBS and nep

# plot the data - WEMWBS score vs nep score
wellness %>%
  ggplot(aes(x = WEMWBS, y = nep)) + # what we want to plot
  geom_point() + # what kind of plot
  labs(x = "WEMWBS score",
       y = "nep score") + # axis labels
  theme_cowplot() # give the plot a nice 'look'

# does it look as though the relationship between the two scores is monotonic?

# test for correlation
cor.test(wellness$WEMWBS,wellness$nep, method = "spearman", exact=FALSE)
