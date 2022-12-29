#' Create a stratified assignment (aka. pair matching)
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
  N <- nrow(df)

  # Extracting the matching variables for distance measurement
  matching_vars <- df[, !names(df) %in% id]

  matching_vars <- matching_vars %>% dplyr::mutate(dplyr::across(.fns = min_max_norm))

  # Setting parameters for the matching
  pairs_lst_lim <- floor(N/n.groups)
  pair_len <- n.groups - 1 # Number of matches we want to find for each row

  #Calculating distance matrix
  d_mat <- matching_vars %>%
    stats::dist(method = 'euclidean', diag = TRUE, upper = TRUE) %>%
    as.matrix()
  diag(d_mat) <- N + 1

  available <- 1:N
  pairs_lst <- list()
  # Apply argpartsort to each column until enough matches have been found
  for(c in 1:N){ # Iterating through the columns
    if(length(pairs_lst) == pairs_lst_lim) { break } # Exiting the loop if we have enough pairs already
    if(!(c %in% available)){ next } # Going to next iteration of the loop if the subject c is not available anymore
    for(search_lim in pair_len:N){
      closest_candidates <- argpartsort(d_mat[,c], search_lim)
      matches <- intersect(available, closest_candidates)
      if(length(matches) == pair_len){
        pair <- list(append(matches, c)) # Adding the subject c to its list of matches to form a pair
        pairs_lst <- append(pairs_lst, pair)
        available <- setdiff(available, unlist(pair))
        break
      } else if(length(matches) > pair_len){
        # Resolving ties
      }
      # Otherwise, redo the loop for search_lim += 1
    }
  }
  return(pairs_lst)
}


### segment of code to put in a different function ###

# Assigning experimental groups to the matched pairs
# N_pairs <- length(matches_lst)
# exp_grps <- lapply(1:N_pairs, function(x) list(0,1))
# exp_grps <- lapply(exp_grps, function(x) sample(x))
# exp_grps <- unlist(exp_grps)
# matches_lst <- unlist(matches_lst)
#
# pairs_df <- tibble(
#   id = matches_lst,
#   grp = exp_grps
# )
# pairs_df <- pairs_df[order(pairs_df$id),]


##### Auxiliary functions #####
argpartsort <- function(vec, n){
  # Returns the indices of the n smallest values
  sorted <- sort(vec, n = n)
  max_sorted <- max(sorted[1:n])
  indices <- which(vec <= max_sorted)
  return(indices)
}

# Normalizing all the numeric vectors in matching_vars
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
