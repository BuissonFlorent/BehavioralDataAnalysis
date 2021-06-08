suppressMessages(suppressWarnings(library(tidyverse)))
library(blockTools) # For function block()
library(caret) # For one-hot encoding function dummyVars()
library(scales) # For function rescale()

# Function to prep the data for stratified randomization
stratification.data.prep <- function(dat, id.var){
  
  #Isolating the identification variable
  if(!(id.var %in% colnames(dat))) stop("the id.var string doesn't match any column name")
  id <- dat[,id.var]
  id <- id %>% mutate_all(as.character)
  dat <- dat %>% select(-eval(id.var))
  
  #Input validation
  #NEED TO REMOVE SAPPLY FOR ROBUSTNESS?
  if(!all(sapply(dat, function(x) is.numeric(x)|
          is.integer(x)|is.factor(x)))) stop("please format all data columns to numeric, integer or factor")
  
  #Handling the identification variable
  dat_out <- id

  #Handling categorical variables
  if(any(sapply(dat, class)=='factor')){
    cat_vars <- dat %>%
      select_if(is.factor)
    
    #One-hot encoding categorical variables
    cat_vars_out <- data.frame(predict(dummyVars(" ~.", data=cat_vars), 
                                       newdata = cat_vars))
    
    #Putting the variables in the output
    dat_out <- dat_out %>%
      cbind(cat_vars_out)
  }
  
  #Handling numeric variables
  if(any(sapply(dat, class)=='numeric')){
    num_vars <- dat %>%
      select_if(function(x) is.numeric(x)|is.integer(x))
    
    #Normalizing numeric variables
    num_vars_out <- num_vars %>%
      mutate_all(rescale)
    
    #Putting the variables in the output
    dat_out <- dat_out %>%
      cbind(num_vars_out)
  } else {warning("The data has no numeric variables. Results may be unstable.")}
  
  return(dat_out)
}

stratified.allocation <- function(dat, id.var, n.groups = 2, 
                                  group.var.name = "group",
                                  algorithm = "naiveGreedy"){
  
  #Converting id variable to character
  dat[[id.var]] <- as.character(dat[[id.var]])

  #Prepping the data
  prepped_data <- stratification.data.prep(dat, id.var = id.var)
  
  #Getting stratified assignment
  assgt <- prepped_data %>%
    block(id.vars = id.var, n.tr = n.groups, 
          algorithm = algorithm, distance = "euclidean") %>%
    assignment() 
  assgt <- assgt$assg$`1` 

  assgt_long <- assgt %>%
    #Reshaping the data
    pivot_longer(!last_col(), names_to = group.var.name, values_to = id.var) %>%
    #Removing extraneous rows created by mismatch between row number and group number
    na.omit()
  
  #removing the distance variable
  assgt_long <- assgt_long %>%
    select(-contains("Distance"))
  
  return_data <- full_join(dat, assgt_long, by = id.var)
  return(return_data)
}