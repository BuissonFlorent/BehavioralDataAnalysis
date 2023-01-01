#' Creates a list of pairs of rows close to each other
#'
#' @param df data to use
#' @param id column(s) used for identification
#' @param n.groups number of groups
#' @return a list of pairs, each having n.groups elements
#' @examples
#' df1 <- data.frame(
#' id = 1:6,
#' x = c(1, 1.5, 5, 5.5, 10, 10.5),
#' y = c(1, 1.5, 5, 5.5, 10, 10.5))
#' pairing(df1, 'id')
#' @export


pairing <- function(df, id, n.groups = 2){

  # Early input validation
  if(nrow(df) == 0 | ncol(df) == 0) stop("the data provided is empty")

  #Stopping if there are NA values
  if(nrow(df %>% stats::na.omit()) != nrow(df)) stop("please address NA values before using this function")

  #Isolating the identification variable
  if(!(id %in% colnames(df))) stop("the id string doesn't match any column name")

  # Further input validation
  #MAYBE NEED TO REMOVE SAPPLY FOR ROBUSTNESS?
  if(!all(sapply(df, function(x) is.numeric(x)| is.integer(x)| is.factor(x)| is.character(x)))) stop("please format all data columns to numeric, integer, factor or character (which will be treated as factor)")

  # Extracting the matching variables for distance measurement
  matching_vars <- df[, !names(df) %in% id]

  #Handling numeric variables
  normalized_vars <- data.frame(nrow(matching_vars))
  if(any(sapply(matching_vars, class)=='numeric')){
    num_vars <- matching_vars %>%
      dplyr::select_if(function(x) is.numeric(x)|is.integer(x))

    #Normalizing numeric variables
    normalized_vars <- num_vars %>%
      dplyr::mutate(dplyr::across(.fns = scales::rescale))
  } else {warning("The data has no numeric variables. Results may be unstable.")}

  #Handling categorical variables
  if(any(sapply(matching_vars, class)=='factor'|sapply(matching_vars, class)=='character')){
    cat_vars <- matching_vars %>%
      dplyr::select_if(function(x) is.factor(x)|is.character(x)) %>%
      dplyr::mutate(dplyr::across(.fns = as.factor))

    #One-hot encoding categorical variables
    normalized_cat_vars <- as.data.frame(stats::model.matrix( ~.-1, data = cat_vars))
    normalized_vars <- normalized_vars %>% cbind(normalized_cat_vars)
  }

  # Setting parameters for the matching
  N <- nrow(df)
  pairs_lst_lim <- floor(N/n.groups)
  nb_matches_needed <- n.groups - 1 # Number of matches we want to find for each row

  #Calculating distance matrix
  d_mat <- normalized_vars %>%
    stats::dist(method = 'euclidean', diag = TRUE, upper = TRUE) %>%
    as.matrix()
  diag(d_mat) <- N + 1

  available <- 1:N
  pairs_lst <- list()
  # Apply argpartsort to each column until enough matches have been found
  for(c in 1:N){ # Iterating through the columns
    if(length(pairs_lst) == pairs_lst_lim) { break } # Exiting the loop if we have enough pairs already
    if(!(c %in% available)){ next } # Going to next iteration of the loop if the subject c is not available anymore
    for(search_lim in nb_matches_needed:N){
      closest_candidates <- argpartsort(d_mat[,c], search_lim)
      matches <- intersect(available, closest_candidates)
      if(length(matches) == nb_matches_needed){
        pair <- list(append(matches, c)) # Adding the subject c to its list of matches to form a pair
        pairs_lst <- append(pairs_lst, pair)
        available <- setdiff(available, unlist(pair))
        break
      } else if(length(matches) > nb_matches_needed){
        # Resolving ties
      }
      # Otherwise, redo the loop for search_lim += 1
    }
  }
  return(pairs_lst)
}

##### Auxiliary functions #####
argpartsort <- function(vec, n){
  # Returns the indices of the n smallest values
  return(order(vec)[1:n])
}

# Normalizing all the numeric vectors in matching_vars
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
