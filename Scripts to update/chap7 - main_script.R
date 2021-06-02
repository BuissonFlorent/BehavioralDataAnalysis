##### Setup #####

# Common libraries
suppressMessages(suppressWarnings(library(tidyverse)))
library(boot) #Required for Bootstrap simulations
library(rstudioapi) #To load data from local folder
library(ggpubr) #To generate multi-plots

#Chapter-specific libraries
library(binom) # To validate CIs analytically with functions binom.coverage() and binom.confint()

### Setting the working directory to the parent folder of this script (Rstudio only)
sourceDir <- rstudioapi::getActiveDocumentContext()$path %>% str_extract("^.+/")
setwd(sourceDir)

set.seed(1234)
options(scipen=10)

### Data generation

times <- c(2,2,3,5,6,9,10,47,61,413)
experience <- c(11,17,18,1,10,4,6,3,8,0)

dat <- tibble(times = times,
              experience = experience)

##### Intro to the Bootstrap #####

# Figure 7-1. Experience and preparation time by baker
ggplot(dat, aes(x=experience, y=times)) + 
  geom_point() + xlab("Months of Experience") + ylab("Baking Times (in minutes)")

# Building linear model
lin_mod_summ <- summary(lm(times~1, data=dat))
est <- lin_mod_summ$coefficients[1,1]
se <- lin_mod_summ$coefficients[1,2]

#Building normal confidence interval
LL <- est-1.96*se
UL <- est+1.96*se

#Building Bootstrap CI
mean_lst <- list()
N <- nrow(dat)
B <- 2000
for(i in 1:B){
  boot_dat <- slice_sample(dat, n=N, replace = TRUE)
  M <- mean(boot_dat$times)
  mean_lst[[i]] <- M
}
mean_summ <- tibble(means = unlist(mean_lst))

LL_b <- as.numeric(quantile(mean_summ$means, c(0.025)))
UL_b <- as.numeric(quantile(mean_summ$means, c(0.975)))
M_b <- mean(mean_summ$means)

# Figure 7-2. Distribution of the means of 2,000 samples
ggplot(mean_summ, aes(x=means)) + geom_histogram()

# Figure 7-3. Distribution of the means of 2,000 samples, 
# with mean of the means (thick line), normal CI bounds (dotted lines) and 
# bootstrap CI bounds (dashed lines).
ggplot(mean_summ, aes(x=means)) + geom_histogram() + 
  #geom_vline(xintercept = est, col ='red', size=2) +
  geom_vline(xintercept = LL, size = 1.25, lty='dotted') +
  geom_vline(xintercept = UL, size = 1.25, lty='dotted') +
  geom_vline(xintercept = LL_b, size = 1.25, lty='dashed', col='blue') +
  geom_vline(xintercept = UL_b, size = 1.25, lty='dashed', col='blue') +
  geom_vline(xintercept = M_b,size = 2, col='blue')

#### Bootstrap for time promise
promise_fun <- function(dat, B = 2000){
  N <- nrow(dat)
  promise_lst <- list()
  for(i in 1:B){
    boot_dat <- slice_sample(dat, n=N, replace = TRUE)
    above180 <- sum(boot_dat$times >= 180)/nrow(boot_dat)
    promise_lst[[i]] <- above180
  }
  promise_summ <- tibble(above180 = unlist(promise_lst))
  return(promise_summ)
}
promise_summ <- promise_fun(dat, B = 2000)

# Figure 7-4. Distribution of the proportion of each sample with a preparation 
# time above 180mn.
ggplot(promise_summ, aes(x=above180)) + geom_histogram() +
  scale_x_continuous(breaks=seq(from=0,to=0.6,by=0.1)) +
  xlab("proportion of sample with preparation time above 180mn")

LL_b <- as.numeric(quantile(promise_summ$above180, c(0.025)))
UL_b <- as.numeric(quantile(promise_summ$above180, c(0.975)))

## Analytical confidence interval
binom.coverage(0.1, 20, conf.level = 0.95, method = "all")
binom.confint(x=2, n=20, conf.level = 0.95)

##### Bootstrap for regression analysis #####

#Running the baseline model
mod <- lm(times~experience, data=dat)
mod_summ <- summary(mod)
mod_summ

est <- mod_summ$coefficients[2,1]
se <- mod_summ$coefficients[2,2]

LL <- est-1.96*se
UL <- est+1.96*se

reg_fun <- function(dat, B = 2000){
  N <- nrow(dat)
  reg_lst <- list()
  for(i in 1:B){
    boot_dat <- slice_sample(dat, n=N, replace = TRUE)
    summ <- summary(lm(times~experience, data=boot_dat))
    coeff <- summ$coefficients['experience','Estimate']
    reg_lst[[i]] <- coeff
  }
  reg_summ <- tibble(coeff = unlist(reg_lst))
  return(reg_summ)
}
reg_summ <- reg_fun(dat, B=4000)

M_b <- mean(reg_summ$coeff)
LL_b <- as.numeric(quantile(reg_summ$coeff, c(0.025)))
UL_b <- as.numeric(quantile(reg_summ$coeff, c(0.975)))

# Figure 7-5. Distribution of the regression coefficients of preparation time 
# on experience, with their mean (thick line), bootstrap CI bounds (thick 
# dashed lines) and normal CI bounds (thin dotted lines) 
# (B=4,000 bootstrap samples)
ggplot(reg_summ, aes(x=coeff)) + geom_histogram(bins=500) + xlim(c(-45,5)) +
  geom_vline(xintercept = M_b, col ='blue', size=2) +
  geom_vline(xintercept = LL, size = 1.25, lty='dotted') +
  geom_vline(xintercept = UL, size = 1.25, lty='dotted') +
  geom_vline(xintercept = LL_b, size = 1.25, lty='dashed', size = 1.5, col='blue') +
  geom_vline(xintercept = UL_b, size = 1.25, lty='dashed', size = 1.5, col='blue') +
  xlab("regression coefficient of preparation time on experience")

### Achieved Significance Level
reg_summ %>% summarise(pval = 2 * sum(coeff > 0)/n())

##### When to use the Bootstrap #####

#Calculating Cook's distance
CD <- cooks.distance(mod)
CD[CD > 1]

#Figure 7-6 Density plot (left) and QQ-plot (right) of regression residuals. 
res_dat <- tibble(res = resid(mod))
p1 <- ggplot(res_dat, aes(res)) + geom_density() + xlab("regression residuals")
p2 <- ggplot(res_dat, aes(sample=res)) + geom_qq() + geom_qq_line() + 
  coord_flip()
ggarrange(p1, p2, ncol=2, nrow=1)

### Repeating regression Bootstrap with only 200 samples
reg_summ2 <- reg_fun(dat, B=200)

M_b2 <- mean(reg_summ2$coeff)
LL_b2 <- as.numeric(quantile(reg_summ2$coeff, c(0.025)))
UL_b2 <- as.numeric(quantile(reg_summ2$coeff, c(0.975)))

# Figure 7-7. Distribution of the regression coefficients of preparation time 
# on experience, with their mean (thick line), bootstrap CI bounds (thick 
# dashed lines) and normal CI bounds (thin dotted lines) 
# (B=200 bootstrap samples)
ggplot(reg_summ2, aes(x=coeff)) + geom_histogram(bins=50) + xlim(c(-45,5)) +
  geom_vline(xintercept = M_b, col ='blue', size=2) +
  geom_vline(xintercept = LL, lty='dotted') +
  geom_vline(xintercept = UL, lty='dotted') +
  geom_vline(xintercept = LL_b, lty='dashed', size = 1.5, col='blue') +
  geom_vline(xintercept = UL_b, lty='dashed', size = 1.5, col='blue') +
  xlab("regression coefficient of preparation time on experience")

##### Tools in R and Python #####

library(boot)

I <- c(1:10)
I
J <- sample(I, 10, replace = TRUE)
J
boot_dat <- dat[J,]

boot_fun <- function(dat, I){
  summ <- summary(lm(times~experience, data=dat[I,]))
  coeff <- summ$coefficients['experience','Estimate']
  return(coeff)
}
boot.out <- boot(data = dat, statistic =boot_fun, R = 2000)
boot.ci(boot.out, conf = 0.95, type = c('norm', 'perc', 'bca'))


### Sidebar: example of Rfast implementation

library(microbenchmark)
library(Rfast)

#The Rfast implementation requires the data to be in matrix format,
#with a column for the constant
mat <- dat %>%
  mutate(const = 1) %>%
  relocate(times) %>%
  data.matrix()

boot_Rfast_fun <- function(mat, I){
  #Adding error catching because the Rfast implementation can be more brittle
  reg_fit <- tryCatch(lmfit(mat[I,2:3], mat[I,1])$be, error = function(e){NA})
  coeff <- tryCatch(reg_fit['experience',], error = function(e){NA})
  return(coeff)
}

#Comparison of the Rfast approach with the standard lm function
microbenchmark(boot(data = dat, statistic = boot_fun, R = 1000),
               boot(data = mat, statistic = boot_Rfast_fun, R = 1000), 
               times = 3)

