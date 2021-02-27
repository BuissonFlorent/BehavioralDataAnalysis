#################################
##### This script analyzes the simulated data used in chapter 1, 
##### The Behavioral Data Analysis Difference
#################################

##### Setup #####

library(tidyverse)
library(rstudioapi)
### Setting the working directory to the parent folder of this script (Rstudio only)
sourceDir <- rstudioapi::getActiveDocumentContext()$path %>% str_extract("^.+/")
setwd(sourceDir)

set.seed(1234)
options(scipen=10)


##### Why we need causal analytics to explain human behavior #####

##### Confound it! The hidden dangers of letting regression sort it out #####

#### First example: stand data ####

#Reading the data
stand_dat <- read_csv("chap1-stand_data.csv")

#Figure 1-3. Sales of ice-cream as a function of observed temperature.
fig1.3 <- ggplot(stand_dat, aes(x=temps, y=icecream_sales)) + 
  geom_point() + labs(x='Temperature', y='Ice-cream sales') 
fig1.3


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

#Figure 1-4. Plot of iced coffee sales versus ice-cream sales
fig1.4 <- ggplot(stand_dat, aes(x=icecream_sales, y=iced_coffee_sales)) + 
  geom_point() + labs(x='Ice-cream sales', y='Iced coffee sales') 
fig1.4

#### Second example: survey data ####

#Reading the data
survey_dat <- read_csv("chap1-survey_data.csv")

#Reformatting shopped variable to binary
survey_dat <- survey_dat %>%
  mutate(shopped = factor(shopped))

### Plotting relationships between variables in the data

## Figure 1-5. (a) Tastes for vanilla and chocolate are uncorrelated in the 
## overall population. (b) Taste for vanilla is higher for people who shop at 
## the ice-cream stand than for people who don’t. (c) Same thing with the taste for chocolate.

#Scatterplot of chocolate versus vanilla taste 
fig1.5.1 <- ggplot(survey_dat, aes(x=vanilla, y=chocolate)) + geom_point() +
  xlim(c(0,28)) + ylim(c(0,28)) + geom_smooth(method = lm, se = FALSE) +
  labs(x='Taste for vanilla', y='Taste for chocolate') 

#Boxplot of vanilla taste against shopping behavior
fig1.5.2 <- ggplot(survey_dat, aes(shopped, vanilla)) + geom_boxplot() +
  labs(x='Shopped (Y/N)', y='Taste for vanilla') + ylim(c(0,30))

#Boxplot of chocolate taste against shopping behavior
fig1.5.3 <- ggplot(survey_dat, aes(shopped, chocolate)) + geom_boxplot() +
  labs(x='Shopped (Y/N)', y='Taste for chocolate') + ylim(c(0,30))

ggpubr::ggarrange(fig1.5.1, fig1.5.2, fig1.5.3, ncol = 3)

### Plotting same scatterplot but for shoppers only

#Figure 1-6 Taste for vanilla and chocolate among shoppers. 
fig1.6 <- ggplot(survey_dat %>% filter(shopped=='1'), aes(x=vanilla, y=chocolate)) + geom_point() + 
  labs(x='Taste for vanilla', y='Taste for chocolate') + geom_smooth(method = lm,
                                                                     se = FALSE) 
fig1.6
















N<- 200

dat <- tibble(
  X = runif(N, 25, 100),
  Y = X * rnorm(N, 3, 2)
)

ggplot(dat, aes(x=X, y=Y)) + 
  geom_point() + 
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE) +
  xlim(c(0,100))