#################################
##### This script analyzes the simulated data used in chapter 2, 
##### Behaviors, Causality and Prediction
#################################

##### Setup #####
set.seed(1235)
options(scipen=10)

library(tidyverse)
library(ggpubr) # Enabling multiplots w function ggarrange

setwd("C:/Users/Florent/Dropbox/Synchronised/Work_and_projects/Behavioral data science book/R scripts/transfered to Github")

##### First example: stand data #####

#Reading the data
stand_dat <- read_csv("chap2-stand_data.csv")

#Plotting ice-cream sales against temperatures
fig2.1 <- ggplot(stand_dat, aes(x=temps, y=icecream_sales)) + 
  geom_point() + labs(x='Temperature', y='Ice-cream sales') 
fig2.1

#Running linear regressions 
#Biased model (coeff should be 1,000)
model1 <- lm(icecream_sales ~ temps, data=stand_dat)
summary(model1)

#correct model for icecream (coeffs should be 1,000 and 20,000)
model2 <- lm(icecream_sales ~ temps + summer_months, data=stand_dat)
summary(model2)

#Model biased by extra controlling
model3 <- lm(icecream_sales ~ iced_coffee_sales + temps + summer_months, 
             data = stand_dat)
summary(model3)

#Plotting iced coffee sales against ice-cream sales
fig2.2 <- ggplot(stand_dat, aes(x=icecream_sales, y=iced_coffee_sales)) + 
  geom_point() + labs(x='Ice-cream sales', y='Iced coffee sales') 
fig2.2

##### Second example: survey data #####

#Reading the data
survey_dat <- read_csv("chap2-survey_data.csv")

#Reformatting shopped variable to binary
survey_dat <- survey_dat %>%
  mutate(shopped = factor(shopped))

### Plotting relationships between variables in the data

#Scatterplot of chocolate versus vanilla taste 
fig2.3.1 <- ggplot(survey_dat, aes(x=vanilla, y=chocolate)) + geom_point() +
  xlim(c(0,28)) + ylim(c(0,28)) + geom_smooth(method = lm, se = FALSE) +
  labs(x='Taste for vanilla', y='Taste for chocolate') 

#Boxplot of vanilla taste against shopping behavior
fig2.3.2 <- ggplot(survey_dat, aes(shopped, vanilla)) + geom_boxplot() +
  labs(x='Shopped (Y/N)', y='Taste for vanilla') + ylim(c(0,30))

#Boxplot of chocolate taste against shopping behavior
fig2.3.3 <- ggplot(survey_dat, aes(shopped, chocolate)) + geom_boxplot() +
  labs(x='Shopped (Y/N)', y='Taste for chocolate') + ylim(c(0,30))

ggpubr::ggarrange(fig2.3.1, fig2.3.2, fig2.3.3, ncol = 3)

### Plotting same scatterplot but for shoppers only

#Scatterplot of chocolate versus vanilla taste 
fig2.4 <- ggplot(survey_dat %>% filter(shopped=='1'), aes(x=vanilla, y=chocolate)) + geom_point() + 
  labs(x='Taste for vanilla', y='Taste for chocolate') + geom_smooth(method = lm,
                                                                     se = FALSE) 
fig2.4



