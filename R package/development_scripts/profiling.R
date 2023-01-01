## This script profiles the functions in the package to improve their performance

library(tidyverse)
library(Rcpp)
library(profvis)
library(microbenchmark)
#library(BehavioralDataAnalysis)

Sys.setenv(BINPREF = "C:/rtools42/x86_64-w64-mingw32.static.posix/bin/")




mpg <- ggplot2::mpg
mpg$id <- as.character(1:nrow(mpg))

short_N <- 10
short_dat <- data.frame(
  id = as.character (1:short_N),
  x = runif(short_N),
  y = runif(short_N)
)



long_N <- 1e4
long_dat <- data.frame(
  id = as.character (1:long_N),
  x = runif(long_N),
  y = runif(long_N)
)

# Profiling stratified assignment on mpg dataset
profvis({
  strat_dat <- strat_assign(mpg, 'id')
})


# Profiling stratified assignment on short numeric
d_mat <- short_dat %>% select(-id) %>% stats::dist() %>% as.matrix()



# Profiling stratified assignment on 1e3 rows
profvis({
  long_N <- 1e4
  long_dat <- data.frame(
    id = as.character (1:long_N),
    x = runif(long_N),
    y = runif(long_N)
  )
  strat_dat <- pairing(long_dat, 'id')
})

### Optimizing the calculations of the argpartsort function with Rcpp then RcppParallel
# First, Rcpp implementation
profvis({
  long_N <- 1e7
  vec <- runif(long_N)
  argpartsort(vec, 3)
})
