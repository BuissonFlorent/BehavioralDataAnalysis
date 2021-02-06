#################################
##### This script analyzes the simulated data used in chapter 3, 
##### Handling missing data
#################################

##### Setup #####
library(psych)
library(ggpubr)
library(tidyverse)
library(mice)
library(VIM) # For visualization function md.pattern
library(reshape) #For function melt



setwd("C:/Users/Florent/Dropbox/Synchronised/Work_and_projects/Behavioral data science book/R scripts/Chapter 3 - missing data and selection bias")
options(scipen=100)
set.seed(1234)

#Loading the data
complete_data <- read_csv("chap3-complete_data.csv") %>%
  mutate(state = factor(state, levels = c("A", "B", "C")))%>%
  mutate(gender = factor(gender, levels = c("M", "F")))
available_data <- read_csv("chap3-available_data.csv") %>%
  mutate(state = factor(state, levels = c("A", "B", "C"))) %>%
  mutate(gender = factor(gender, levels = c("M", "F")))
available_data_supp <- read_csv("chap3-available_data_supp.csv")


##### section 1: Visualizing missing data #####

#Visualizing patterns of missingness in data
md.pattern(available_data)

#### Subsection 1: Amount of missing data ####

#Quantifying the amount of missing data for a specific variable
min_data <- available_data %>%
  mutate(neuro = ifelse(!is.na(neuro), neuro, min(neuro, na.rm = TRUE)))
max_data <- available_data %>%
  mutate(neuro = ifelse(!is.na(neuro), neuro, max(neuro, na.rm = TRUE)))

summary(lm(bkg_amt~neuro, data=available_data))
summary(lm(bkg_amt~neuro, data=min_data))
summary(lm(bkg_amt~neuro, data=max_data))

#### Subsection 2: Correlation of missingness ####

### Tacoma/Tampa example

tacoma <- read_csv("chap3-tacoma.csv")
tampa <- read_csv("chap3-tampa.csv")

# Tampa: highly correlated missingness
md.pattern(tampa)

# Tacoma: uncorrelated missingness
md.pattern(tacoma)

##Visualizing correlation matrices

# Building the correlation matrices
tampa_miss <- tampa %>%
  select(-ID) %>%
  mutate(across(everything(),is.na))
tampa_cor <- cor(tampa_miss) %>%
  melt()

tacoma_miss <- tacoma %>%
  select(-ID) %>%
  mutate(across(everything(),is.na))
tacoma_cor <- cor(tacoma_miss) %>%
  melt()

#Defining function to visualize correlation matrices
cormat_viz_fun <- function(){
  cor1 <- ggplot(data = tampa_cor, aes(x=X1, y=X2, fill=value)) + 
    geom_tile(color = "white") + theme_minimal() +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab",
                         name="Correlation") +  
    theme(axis.title.x = element_blank(),axis.title.y = element_blank())
  cor2 <- ggplot(data = tacoma_cor, aes(x=X1, y=X2, fill=value)) + 
    geom_tile(color = "white") + theme_minimal() +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Correlation") + 
    theme(axis.title.x = element_blank(), axis.title.y = element_blank())
  
  ggarrange(cor1, cor2, ncol = 2)
}
cormat_viz_fun()

### Back to AirCnC example

# Building the correlation matrix
dat_miss <- available_data %>%
  select(extra, state, bkg_amt,neuro) %>%
  mutate(across(everything(),is.na)) %>%
  mutate(across(everything(),as.numeric))

dat_cor <- cor(dat_miss) %>%
  melt()

cor(dat_miss)
ggplot(data = dat_cor, aes(x=X1, y=X2, fill=value)) + 
  geom_tile(color = "white") + theme_minimal() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Correlation") +  
  theme(axis.title.x = element_blank(),axis.title.y = element_blank())

##### Section 2: Diagnosing Missing Data #####

#### Subsection 1: Causes of Missingness: Rubin’s classification ####

# (no code in this subsection)

#### Subsection 2: Diagnosing MCAR variables ####

# Sources of missingness
md_extra_mod <- glm(is.na(extra)~.,
                    family = binomial(link = "logit"), 
                    data=available_data)
summary(md_extra_mod)

#### Subsection 3: Diagnosing MAR variables ####

# Sources of missingness
md_state_mod <- glm(is.na(state)~.,
                    family = binomial(link = "logit"), 
                    data=available_data)
summary(md_state_mod)

#Plotting distribution of available and missing state by age
md_state_age <- available_data %>%
  select(age, state) %>%
  mutate(status = ifelse(is.na(available_data$state), "missing", "observed")) %>%
  mutate(status = factor(status))

md_state_age_plot <- ggplot(md_state_age, aes(x=age, col=status, lty=status)) +
  geom_density(position='identity', size=1.2) + 
  scale_x_continuous(name = "age",
                     breaks = seq(0, 70, 5),
                     limits=c(0, 70)) +
  scale_linetype_manual(values=c("dotted", "solid"))
md_state_age_plot

md_state_extra <- available_data %>%
  select(extra, state) %>%
  mutate(status = ifelse(is.na(available_data$state), "missing", "observed")) %>%
  mutate(status = factor(status))

md_state_extra_plot <- ggplot(md_state_extra %>% mutate(extra=ifelse(is.na(extra), -30, extra)), aes(x=extra, col=status, lty=status)) +
  geom_density(position='identity', size=1.2) + 
  scale_x_continuous(name = "extra",
                     breaks = seq(-30, 10, 5),
                     limits=c(-30, 10)) +
  scale_linetype_manual(values=c("dotted", "solid"))
md_state_extra_plot


#### Subsection 4: Diagnosing MNAR variables ####

md_neuro_mod <- glm(is.na(neuro)~.,
                    family = binomial(link = "logit"), 
                    data=available_data)
summary(md_neuro_mod)

# Using another child of Neuroticism to confirm MNAR
neuro_dat <- available_data %>%
  dplyr::select(neuro, bkg_amt) %>%
  mutate(insurance = available_data_supp$insurance)

md_neuro_mod2 <- glm(is.na(neuro)~insurance,
                    family = binomial(link = "logit"), 
                    data=neuro_dat)
summary(md_neuro_mod2)

marginplot(available_data[,c('neuro','bkg_amt')])
marginplot(neuro_dat[,c('neuro','insurance')])

neuro_md_viz <- function(dat){
  md_neuro <- dat %>%
    select(neuro, insurance) %>%
    mutate(status = ifelse(is.na(dat$neuro), "missing", "observed")) %>%
    mutate(status = factor(status))
  
  md_neuro_insur_plot <- ggplot(md_neuro, aes(x=insurance, col=status, lty=status)) +
    geom_density(position='identity', size=1.2) + 
    scale_linetype_manual(values=c("dotted", "solid"))
  md_neuro_insur_plot
}

neuro_md_viz(neuro_dat)

neuro_md_viz2 <- function(dat){
  md_neuro <- dat %>%
    select(neuro, bkg_amt) %>%
    mutate(status = ifelse(is.na(dat$neuro), "missing", "observed")) %>%
    mutate(status = factor(status))
  
  md_neuro_insur_plot <- ggplot(md_neuro, aes(x=bkg_amt, col=status, lty=status)) +
    geom_density(position='identity', size=1.2) + 
    scale_linetype_manual(values=c("dotted", "solid"))
  md_neuro_insur_plot
}

neuro_md_viz2(neuro_dat)

#### Subsection 5: Missingness as a gradient ####

# (missing code for spectrum_md_viz function here)







##### Section 3: Handling missing data #####

#### Subsection 1: Introduction to Multiple Imputation (MI) ####

MI_data <- mice(available_data, print = FALSE)
MI_summ <- MI_data  %>%
  with(lm(bkg_amt~age+open+extra+neuro+gender+state)) %>%
  pool() %>%
  summary()
#print(MI_summ)

#### Subsection 2: Default Imputation Method: Predictive Mean Matching ####

summary(MI_data)

densityplot(MI_data, thicker = 3, lty = c(1,rep(2,5)))

#### Subsection 3: From PMM to normal imputation (R only) ####

imp_meth_dist <- c("pmm", rep("norm.nob",3), "", "pmm", "norm.nob")
MI_data_dist <- mice(available_data, print = FALSE, method = imp_meth_dist)
#summary(MI_data_dist)

# Visualizing variable distribution
p1 <- ggplot(available_data, aes(x=age)) + geom_density() + xlab("Age")
p2 <- ggplot(available_data, aes(x=open)) + geom_density() + xlab("Openness") + xlim(c(0,10))
p3 <- ggplot(available_data, aes(x=extra)) + geom_density() + xlab("Extraversion") + xlim(c(0,10))
p4 <- ggplot(available_data, aes(x=neuro)) + geom_density() + xlab("Neuroticism") + xlim(c(0,10))
p5 <- ggplot(available_data, aes(x=bkg_amt)) + geom_density() + xlab("Booking amount")
ggarrange(p1, p2, p3, p4, p5, ncol = 5)

#Visualizing the difference between PMM and normal imputation with deterministically MNAR data

gradient_md_viz <- function(N = 1000){
  dat <- tibble(
    X = rnorm(N,5,1),
    Y = X + rnorm(N,0,1)
  )
  
  dat1 <- dat %>%
    mutate(prob_miss = 0.5) %>%
    mutate(Y = ifelse(runif(N,0,1)>=prob_miss, Y, NA))
  p1a <- ggplot(dat1, aes(x=X,y=Y)) + geom_point() + xlim(c(0,10)) + ylim(c(0,10))
  p1b <- ggplot(dat1, aes(x=X,y=prob_miss)) + geom_point() + 
    ylim(c(0,1)) + ylab("prob. missingness") + xlim(c(0,10))
  
  dat2 <- dat %>%
    mutate(prob_miss = logistic(X-5, a=0.5, c=0.25, z=0.75)) %>%
    mutate(Y = ifelse(runif(N,0,1)>=prob_miss, Y, NA))
  p2a <- ggplot(dat2, aes(x=X,y=Y)) + geom_point() + xlim(c(0,10)) + ylim(c(0,10))
  p2b <- ggplot(dat2, aes(x=X,y=prob_miss)) + geom_point() + 
    ylim(c(0,1)) + ylab("prob. missingness") + xlim(c(0,10))
  
  dat3 <- dat %>%
    mutate(prob_miss = logistic(X-5, a=1.5)) %>%
    mutate(Y = ifelse(runif(N,0,1)>=prob_miss, Y, NA))
  p3a <- ggplot(dat3, aes(x=X,y=Y)) + geom_point() + xlim(c(0,10)) + ylim(c(0,10))
  p3b <- ggplot(dat3, aes(x=X,y=prob_miss)) + geom_point() + 
    ylim(c(0,1)) + ylab("prob. missingness") + xlim(c(0,10))
  
  dat4 <- dat %>%
    mutate(prob_miss = ifelse(X<=5,0,1)) %>%
    mutate(Y = ifelse(runif(N,0,1)>=prob_miss, Y, NA))
  p4a <- ggplot(dat4, aes(x=X,y=Y)) + geom_point() + xlim(c(0,10)) + ylim(c(0,10))
  p4b <- ggplot(dat4, aes(x=X,y=prob_miss)) + geom_point() + 
    ylim(c(0,1)) + ylab("prob. missingness") + xlim(c(0,10))
  
  ggarrange(p1a, p2a, p3a, p4a, p1b, p2b, p3b, p4b, nrow=2, ncol=4)
}

gradient_md_viz(500)

#### Subsection 4: Adding Auxiliary Variables (R and Python) ####

augmented_data <- cbind(available_data, available_data_supp)
MI_data_aux <- mice(augmented_data, print = FALSE)

pred_mat <- MI_data_aux$predictorMatrix
pred_mat

pred_mat[,"insurance"] <- 0
pred_mat[,"active"] <- 0
pred_mat["neuro","insurance"] <- 1
pred_mat["extra","active"] <- 1
pred_mat

#### Subsection 5: Scaling up the number of imputed datasets ####

MI_data <- mice(available_data, print = FALSE, m=20)