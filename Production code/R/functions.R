suppressMessages(suppressWarnings(library(tidyverse)))
library(caret) # For one-hot encoding function dummyVars()
library(scales) # For function rescale()

# Bootstrap CI function
boot.CI <- function(dat, metric_fun, B=20, conf.level=0.9){
  
  #Validating the inputs
  if(nrow(dat) == 0) stop("the data provided is empty")
  if(B <= 0) stop("the value provided for the number of Bootstrap simulations is negative")
  if(conf.level <= 0) stop("the value provided for the confidence level is negative")
  
  #Calculating and validating the offset
  offset = round(B * (1 - conf.level) / 2)
  if(B + 1 - 2*offset <= 1) stop("the number of Bootstrap simulations is too small in relation to the confidence level")
  
  # NOTE: towards the end of writing the book, I discovered that sapply generally
  # gives better performance than the boot library
  boot_vec <- sapply(1:B, function(x){
    metric_fun(slice_sample(dat, n = nrow(dat), replace = TRUE))})
  if(any(is.na(boot_vec))) stop("the metric function returned an NA, ")
  boot_vec <- sort(boot_vec, decreasing = FALSE)
  
  lower_bound <- boot_vec[offset]
  upper_bound <- boot_vec[B+1-offset]
  CI <- c(lower_bound, upper_bound)
  return(CI)
}

# Function to prep the data for stratified randomization
stratification.data.prep <- function(dat, id.var){
  
  # Early input validation
  if(nrow(dat) == 0) stop("the data provided is empty")
  
  #Isolating the identification variable
  if(!(id.var %in% colnames(dat))) stop("the id.var string doesn't match any column name")
  id <- dat[,id.var]
  id <- id %>% mutate_all(as.character)
  dat <- dat %>% select(-eval(id.var))
  
  # Further input validation
  #MAYBE NEED TO REMOVE SAPPLY FOR ROBUSTNESS?
  if(!all(sapply(dat, function(x) is.numeric(x)|
          is.integer(x)|is.factor(x)))) stop("please format all data columns to numeric, integer or factor")
  
  #Stopping if there are NA's
  if(nrow(dat %>% na.omit()) != nrow(dat)) stop("please address NA's before using this function")
  
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
  
  # Early input validation
  if(nrow(dat) == 0) stop("the data provided is empty")
  if(!(algorithm %in% c("optGreedy", "optimal", "naiveGreedy",
                        "randGreedy", "sortGreedy"))) stop("the algorithm name provided is not in the list of algorithms implemented")
  
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