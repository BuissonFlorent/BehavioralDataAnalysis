#' Creates a Bootstrap confidence interval for the output of a function
#'
#' @param df data to use
#' @param fct function to run on the data
#' @param B number of Bootstrap sampling iterations to run
#' @param conf.level confidence interval percentage to use
#' @return a vector with two values, the lower and upper bounds of the confidence interval
#' @examples
#' df1 <- data.frame(
#' id = 1:6,
#' x = c(1, 1.5, 5, 5.5, 10, 10.5),
#' y = c(1, 1.5, 5, 5.5, 10, 10.5))
#' boot_CI(df1, function(df) mean(df$x), B=100)
#' @export


boot_CI <- function(df, fct, B = 100, conf.level = 0.90){
  #Validating the inputs
  if(nrow(df) == 0) stop("the data provided is empty")
  if(B <= 0) stop("the value provided for the number of Bootstrap simulations is negative")
  if(conf.level <= 0) stop("the value provided for the confidence level is negative")
  if(conf.level > 1) stop("the value provided for the confidence level is above 1")

  # Validating that the function returns a single-valued output
  if(suppressWarnings(is.na(fct(df)|length(fct(df))>1))) stop("the function doesn't return a single-valued, non-null output.")

  #Calculating and validating the offset
  if(B * (1 - conf.level) / 2 < 1) stop("the number of Bootstrap simulations is too small in relation to the confidence level")
  offset = round(B * (1 - conf.level) / 2)

  # NOTE: towards the end of writing the book, I discovered that sapply generally
  # gives better performance than the boot library
  boot_vec <- sapply(1:B, function(x){
    fct(dplyr::slice_sample(df, n = nrow(df), replace = TRUE))
    })
  if(any(is.na(boot_vec))) stop("the function returned an NA")
  boot_vec <- sort(boot_vec, decreasing = FALSE)

  lower_bound <- boot_vec[offset]
  upper_bound <- boot_vec[B+1-offset]
  CI <- c(lower_bound, upper_bound)
  return(CI)
}
