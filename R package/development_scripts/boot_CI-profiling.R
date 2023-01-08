##### Setup

#library(boot)
library(foreach)
library(doParallel)
library(tidyverse)
library(profvis)
library(microbenchmark)


set.seed(1)
N <- 1e6
df <- data.frame(
  x = rnorm(mean = 10, n = N)
)

df <- df %>%
  mutate(y = x + rnorm(N))

myFun <- function(df){
  mod <- lm('y~x', data = df)
  coeff <- mod$coefficients[2]
  remove(mod)
  return(coeff)
}
myFun(df)



##### FOR LOOP #####
# performance:
# 17s for B=100
# 184s for B=1000
for_fun <- function(B = 20){
  for_vec <- rep(0, B)
  for(b in 1:B){
    for_vec[b] <- myFun(slice_sample(df, n = nrow(df), replace = TRUE))
  }
  return(for_vec)
}

profvis({
  for_vec <- for_fun(1000)
})
gc()



##### sapply implementation #####
# performance: 16gig and 16s for B=100, similar to FOR loop
sapply_fun <- function(B = 20){
  sapply_vec <- rep(0, B)
  sapply_vec <- sapply(1:B, function(b){
    myFun(slice_sample(df, n = nrow(df), replace = TRUE))
  })
  return(sapply_vec)
}
#sapply_vec <- sapply_fun()

profvis({
  sapply_vec <- sapply_fun(100)
})



##### boot implementation #####
# performance: 21gig and 99s for B=100
boot_fun <- function(B = 20){
  inner_boot_fun <- function(df, J){
    boot_df <- df[J,]
    mod <- lm('y~x', data = boot_df)
    coeff <- mod$coefficients[2]
    remove(mod)
    return(coeff)
  }

  boot.out <- boot(data=df, statistic=inner_boot_fun, R=B)
  confint <- boot.ci(boot.out, conf = 0.90, type = c('perc'))
  CI <- confint$percent[c(4,5)]
  return(CI)
}

profvis({
  boot_vec <- boot_fun(100)
})



##### Foreach implementation #####
# performance with %do%: 16gig and 14.5s for B=100, similar to FOR loop
# performance with %dopar%:
# 15s for B=200
# 73s for B=1000
set.seed(123)
foreach_fun <- function(B = 20, dat){
  myFun <- function(df){
    mod <- lm('y~x', data = df)
    coeff <- mod$coefficients[2]
    remove(mod)
    return(coeff)
  }
  N <- 1e6
  nb_cores <- detectCores() - 1
  cl <- makeCluster(nb_cores)
  registerDoParallel()
  vec <- rep(-1, B)
  inner_df <- dat

  vec <- foreach(i=1:B,
                 .combine='c',
                 .packages = "dplyr",
                 .inorder = FALSE) %dopar% {
                   sample_df <- slice_sample(inner_df, n = N, replace = TRUE)
                   myFun(sample_df)
  }
  stopImplicitCluster()
  return(vec)
}

profvis({
  foreach_vec <- foreach_fun(B=1000, dat = df)
})


