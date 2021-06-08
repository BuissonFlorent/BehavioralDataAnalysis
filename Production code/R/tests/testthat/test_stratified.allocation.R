suppressMessages(suppressWarnings(library(tidyverse)))
library(testthat)
library(rstudioapi) #To load data from local folder

sourceDir <- rstudioapi::getActiveDocumentContext()$path %>% str_extract("^.+/")
setwd(sourceDir)
setwd('./../..')
source('functions.R')
setwd(sourceDir)

#context("stratified.allocation function")

#Input datasets
simplest_input <- tibble(
  name = c("Anna", "Bernard", "Amelia", "Bob"),
  gender = as.factor(c("F", "M", "F", "M")),
  age = c(40, 20, 41, 21)
) 

character_input <- tibble(
  name = c("Anna", "Amalric", "Barbara", "Bob"),
  color = c("blue", "red", "yellow", "blue")
)

factor_input <- tibble(
  name = c("Anna", "Amalric", "Barbara", "Bob"),
  color = as.factor(c("blue", "red", "yellow", "blue")),
  flavor = as.factor(c("chocolate", "vanilla", "chocolate", "vanilla"))
)

numeric_input <- tibble(
  name = c("Anna", "Amalric", "Barbara", "Bob"),
  age = c(40, 20, 41, 21),
  height = c(6, 5, 6.1, 5.1)
)

prime_input <- tibble(
  name = c("Anna", "Bernard", "Amelia", "Bob", "Charlie"),
  gender = as.factor(c("F", "M", "F", "M", "M")),
  age = c(40, 20, 41, 21, 22)
) 

simplest_output <- stratified.allocation(simplest_input, id.var = 'name')
optimal_output <- stratified.allocation(simplest_input, id.var = 'name', algorithm = 'optimal')
prime_output <- stratified.allocation(prime_input, id.var = 'name')

test_that("function output is correctly formatted", {
  expect_that(simplest_output, is_a("data.frame"))
  expect_that(nrow(simplest_output), equals(nrow(simplest_input)))
  expect_that(nrow(prime_output), equals(nrow(prime_input)))
  expect_identical(simplest_input$name, simplest_output$name)  
})

test_that("function returns correct output in simplest cases", {
  expect_true(sum(simplest_output$group == c("Treatment 1", "Treatment 1", "Treatment 2", "Treatment 2"))|
                sum(simplest_output$group == c("Treatment 2", "Treatment 2", "Treatment 1", "Treatment 1")))
})



