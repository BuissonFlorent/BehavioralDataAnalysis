#' Create a stratified assignment (aka. pair matching)
#'
#' @param df data to use
#' @param id column(s) used for identification
#' @param n.groups number of groups
#' @return the original data frame, with an added column for group assignment
#' @examples
#' df1 <- data.frame(
#' id = 1:6,
#' x = c(1, 1.5, 5, 5.5, 10, 10.5),
#' y = c(1, 1.5, 5, 5.5, 10, 10.5))
#' strat_assign(df1, 'id')
#' @export

strat_assign <- function(df, id, n.groups = 2){

  # Generating the matched pairs
  pairs_list <- pairing(df, id = id, n.groups = n.groups)

  # Assigning experimental groups to the matched pairs
  N_pairs <- length(pairs_list)
  assgnmt <- lapply(1:N_pairs, function(x) list(0,1))
  assgnmt <- lapply(assgnmt, function(x) sample(x))
  assgnmt <- unlist(assgnmt)
  pairs_list <- unlist(pairs_list)

  pairs_df <- data.frame(
    id = pairs_list,
    grp = assgnmt
  )
  pairs_df <- pairs_df[order(pairs_df$id),]

  df_out <- merge(df, pairs_df, by='id')

  return(df_out)
}
