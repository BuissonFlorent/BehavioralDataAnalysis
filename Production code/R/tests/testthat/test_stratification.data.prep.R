library(testthat)
library(rstudioapi) #To load data from local folder

sourceDir <- rstudioapi::getActiveDocumentContext()$path %>% str_extract("^.+/")
setwd(sourceDir)
setwd('./../..')
source('functions.R')
setwd(sourceDir)

context("stratification.data.prep function")

#Input datasets
simplest_input <- tibble(
  name = c("Anna", "Amalric", "Barbara", "Bob"),
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

simplest_output <- stratification.data.prep(simplest_input, id.var = 'name')

test_that("function output is correctly formatted", {
  expect_that(simplest_output, is_a("data.frame"))
  expect_that(nrow(simplest_output), equals(nrow(simplest_input)))
  expect_identical(simplest_input$name, simplest_output$name)  
})

test_that("input validation errors are caught",{
  expect_error(stratification.data.prep(simplest_input, id.var = 'nom'),
               "the id.var string doesn't match any column name")
  expect_error(stratification.data.prep(character_input, id.var = 'name'),
               "please format all data columns to numeric, integer or factor")
})

test_that("output is as expected for simple cases", {
  expect_true(max(simplest_output %>% select_if(is.numeric))<=1)
  expect_true(min(simplest_output %>% select_if(is.numeric))>=0)
})