suppressMessages(suppressWarnings(library(tidyverse)))
library(testthat)
library(rstudioapi) #To load data from local folder

sourceDir <- rstudioapi::getActiveDocumentContext()$path %>% str_extract("^.+/")
setwd(sourceDir)
setwd('./../..')
source('functions.R')
setwd(sourceDir)

input1_dat <- tibble(
  id = c('a', 'b', 'c', 'd', 'e'),
  val = rep(10, 5)
)

input2_dat <- tibble(
  id = c('a', 'b', 'c', 'd', 'e'),
  val = c(10, 10, 10, 10, NA)
)
set.seed(2)
test_CI <- boot.CI(input2_dat, metric)


metric_mean <- function(dat){
  return(mean(dat$val))
}

test_that("function output is correctly formatted", {
  test_CI <- boot.CI(input1_dat, metric)
  expect_equal(length(test_CI), 2)
})

test_that("function returns correct output in simplest cases", {
 expect_equal(boot.CI(input1_dat, metric), c(10, 10))
})



