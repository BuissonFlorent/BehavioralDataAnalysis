#################################
##### This script analyzes the simulated data used in chapter 6, 
##### Handling missing data
#################################

##### Setup #####

# Common libraries
suppressMessages(suppressWarnings(library(tidyverse)))
library(boot) #Required for Bootstrap simulations
library(rstudioapi) #To load data from local folder
library(ggpubr) #To generate multi-plots

# Chapter-specific libraries
library(mice)
library(VIM) # For visualization function md.pattern()
library(reshape) #For function melt()
library(psych) #For function logistic()

### Setting the working directory to the parent folder of this script (Rstudio only)
sourceDir <- rstudioapi::getActiveDocumentContext()$path %>% str_extract("^.+/")
setwd(sourceDir)

options(scipen=10)
set.seed(1234)

#Loading the data
complete_data <- read_csv("chap6-complete_data.csv") %>%
  mutate(state = factor(state, levels = c("A", "B", "C")))%>%
  mutate(gender = factor(gender, levels = c("M", "F")))
available_data <- read_csv("chap6-available_data.csv") %>%
  mutate(state = factor(state, levels = c("A", "B", "C"))) %>%
  mutate(gender = factor(gender, levels = c("M", "F")))
available_data_supp <- read_csv("chap6-available_data_supp.csv")


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

tacoma <- read_csv("chap6-tacoma.csv")
tampa <- read_csv("chap6-tampa.csv")

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
viz_fun_6.16 <- function(dat){
  md_state_age <- dat %>%
    select(age, state) %>%
    mutate(status = ifelse(is.na(dat$state), "missing", "observed")) %>%
    mutate(status = factor(status))
  
  ggplot(md_state_age, aes(x=age, col=status, lty=status, size=status, alpha=status)) +
    geom_freqpoly(position='identity') + 
    scale_x_continuous(name = "age",
                       breaks = seq(0, 70, 5),
                       limits=c(0, 70)) +
    scale_linetype_manual(values=c("dotted", "solid")) +
    scale_size_manual(values=c(1.25,0.5)) +
    scale_alpha_manual(values=c(1,0.75)) + theme_classic()
}
viz_fun_6.16(available_data)

viz_fun_6.17 <- function(dat){
  md_state_extra <- dat %>%
    select(extra, state) %>%
    mutate(status = ifelse(is.na(dat$state), "missing", "observed")) %>%
    mutate(status = factor(status))
  
  ggplot(md_state_extra %>% mutate(extra=ifelse(is.na(extra), -10, extra)), 
         aes(x=extra, col=status, lty=status, size=status, alpha=status)) +
    geom_freqpoly(position='identity') + 
    scale_x_continuous(name = "extra", limits=c(-15, 10)) +
    scale_linetype_manual(values=c("dotted", "solid")) +
    scale_size_manual(values=c(1.25,0.5)) +
    scale_alpha_manual(values=c(1,0.75)) + theme_classic()
}
viz_fun_6.17(available_data)


#### Subsection 4: Diagnosing MNAR variables ####

md_neuro_mod <- glm(is.na(neuro)~.,
                    family = binomial(link = "logit"), 
                    data=available_data)
summary(md_neuro_mod)

# Using another child of Neuroticism to confirm MNAR
viz_fun_6.18 <- function(dat1, dat2){
  neuro_dat <- dat1 %>%
    dplyr::select(neuro, bkg_amt) %>%
    mutate(insurance = dat2$insurance) %>%
    select(neuro, insurance)
  neuro_dat <- neuro_dat %>%
    mutate(status = ifelse(is.na(neuro_dat$neuro), "missing", "observed")) %>%
    mutate(status = factor(status))
  
  ggplot(neuro_dat, aes(x=insurance, col=status, lty=status, size=status, alpha=status)) +
    geom_freqpoly(position='identity') + 
    scale_x_continuous(name = "insurance") +
    scale_linetype_manual(values=c("dotted", "solid")) +
    scale_size_manual(values=c(1.25,0.5)) +
    scale_alpha_manual(values=c(1,0.75)) + theme_classic()
}
viz_fun_6.18(available_data, available_data_supp)

#### Subsection 5: Missingness as a spectrum ####

viz_fun_6.20 <- function(N = 100){
  dat <- tibble(
    X = runif(N,2.5,7.5),
    Y = X + rnorm(N,0,2)
  )
  dat1 <- dat %>%
    mutate(prob_miss = 0.5) %>%
    mutate(miss = ifelse(runif(N,0,1)>=prob_miss, 'Y', 'N'))
  p1a <- ggplot(dat1, aes(x=X,y=Y)) + geom_point(aes(alpha=0.5, col=miss, shape=miss), show.legend = FALSE) + 
    xlim(c(0,10)) + ylim(c(0,10)) + theme_classic() + scale_shape_manual(values=c(4, 15))
  p1b <- ggplot(dat1, aes(x=X,y=prob_miss)) + geom_line() + 
    ylim(c(0,1)) + ylab("prob. missingness") + xlim(c(0,10)) + theme_classic()
  
  dat2 <- dat %>%
    mutate(prob_miss = logistic(X-5, a=1.5)) %>%
    mutate(miss = ifelse(runif(N,0,1)>=prob_miss, 'Y', 'N'))
  p2a <- ggplot(dat2, aes(x=X,y=Y)) + geom_point(aes(alpha=0.5, col=miss, shape=miss), show.legend = FALSE) + 
    xlim(c(0,10)) + ylim(c(0,10)) + theme_classic() + scale_shape_manual(values=c(4, 15))
  p2b <- ggplot(dat2, aes(x=X,y=prob_miss)) + geom_line() + 
    ylim(c(0,1)) + ylab("prob. missingness") + xlim(c(0,10)) + theme_classic()
  
  dat3 <- dat %>%
    mutate(prob_miss = ifelse(X<=5,0,1)) %>%
    mutate(miss = ifelse(runif(N,0,1)>=prob_miss, 'Y', 'N'))
  p3a <- ggplot(dat3, aes(x=X,y=Y)) + geom_point(aes(alpha=0.5, col=miss, shape=miss), show.legend = FALSE) + 
    xlim(c(0,10)) + ylim(c(0,10)) + theme_classic() + scale_shape_manual(values=c(4, 15))
  p3b <- ggplot(dat3, aes(x=X,y=prob_miss)) + geom_line() + 
    ylim(c(0,1)) + ylab("prob. missingness") + xlim(c(0,10)) + theme_classic()
  
  ggarrange(p1a, p2a, p3a, p1b, p2b, p3b, nrow=2, ncol=3)
}
viz_fun_6.20(200)

viz_fun_6.21 <- function(dat){
  p1 <- ggplot(dat, aes(x=open)) + geom_histogram() + xlim(c(0,10)) + theme_classic()
  p2 <- ggplot(dat, aes(x=extra)) + geom_histogram() + xlim(c(0,10)) + theme_classic()
  p3 <- ggplot(dat, aes(x=neuro)) + geom_histogram() + xlim(c(0,10)) + theme_classic()
  ggarrange(p1, p2, p3, nrow=3, ncol=1)
}
viz_fun_6.21(available_data)

##### Section 4: Handling missing data #####

#### Subsection 1: Introduction to Multiple Imputation (MI) ####

MI_data <- mice(available_data, print = FALSE)
MI_summ <- MI_data  %>%
  with(lm(bkg_amt~age+open+extra+neuro+gender+state)) %>%
  pool() %>%
  summary()
print(MI_summ)

#### Subsection 2: Default Imputation Method: Predictive Mean Matching ####

# Getting summary of defaults
summary(MI_data)

#Visualizing imputed datasets
densityplot(MI_data, thicker = 3, lty = c(1,rep(2,5)))

#### Subsection 3: From PMM to normal imputation (R only) ####

# Creating a vector of imputation methods variable by variable
imp_meth_dist <- c("pmm", rep("norm.nob",3), "", "pmm", "norm.nob")
MI_data_dist <- mice(available_data, print = FALSE, method = imp_meth_dist)
#summary(MI_data_dist)

# Visualizing variable distribution
viz_fun_6.24 <- function(dat){
  p1 <- ggplot(dat, aes(x=age)) + geom_density() + xlab("Age") + theme_classic() 
  p2 <- ggplot(dat, aes(x=open)) + geom_density() + xlab("Openness") + xlim(c(0,10)) + theme_classic()
  p3 <- ggplot(dat, aes(x=extra)) + geom_density() + xlab("Extraversion") + xlim(c(0,10)) + theme_classic()
  p4 <- ggplot(dat, aes(x=neuro)) + geom_density() + xlab("Neuroticism") + xlim(c(0,10)) + theme_classic()
  p5 <- ggplot(dat, aes(x=bkg_amt)) + geom_density() + xlab("Booking amount") + theme_classic()
  ggarrange(p1, p2, p3, p4, p5, ncol = 5)
}
viz_fun_6.24(available_data)

# Visualizing the difference between PMM and normal imputation with deterministically MNAR data
viz_fun_6.25 <- function(MI_data, MI_data_dist){
  sample_complete_dist <- complete(MI_data_dist,1) %>%
    mutate(md_ind = is.na(available_data$neuro)) %>%
    select(age, neuro, md_ind)
  sample_complete <- complete(MI_data,1) %>%
    mutate(md_ind = is.na(available_data$neuro)) %>%
    select(age, neuro, md_ind)
  
  p1 <- ggplot(sample_complete, aes(x=neuro, y=age)) + xlim(c(0,10)) +
    geom_point(data=sample_complete%>%filter(md_ind), shape=4, col='red') + 
    geom_point(data=sample_complete%>%filter(!md_ind), shape=15, alpha=0.25, col='blue') + theme_classic()
  p2 <- ggplot(sample_complete_dist, aes(x=neuro, y=age)) + xlim(c(0,10)) +
    geom_point(data=sample_complete_dist%>%filter(md_ind), shape=4, col='red') + 
    geom_point(data=sample_complete_dist%>%filter(!md_ind), shape=15, alpha=0.25, col='blue') + theme_classic()
  ggarrange(p1,p2, nrow=2)
}
viz_fun_6.25(MI_data, MI_data_dist)

#### Subsection 4: Adding Auxiliary Variables (R and Python) ####

# Adding the auxiliary variables
augmented_data <- cbind(available_data, available_data_supp)
# Standard imputation with the added auxiliary variables
MI_data_aux <- mice(augmented_data, print = FALSE)

# Modifying the predictor matrix to avoid integrating random correlations
pred_mat <- MI_data_aux$predictorMatrix
pred_mat

pred_mat[,"insurance"] <- 0
pred_mat[,"active"] <- 0
pred_mat["neuro","insurance"] <- 1
pred_mat["extra","active"] <- 1
pred_mat

#### Subsection 5: Scaling up the number of imputed datasets ####

# Increasing the number of imputed datasets
MI_data <- mice(available_data, print = FALSE, m=20)