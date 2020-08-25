#' Changes a numeric variable to a categoric variable
#'
#' The LISTEN datasets do not include variables for income and age categories. 
#' Doing so makes obtaining the distributions of clients across these demographics easier to 
#'
#' @param num_var The numeric variable to be split
#' @param bins The gaps between bits the numeric variable
#' @param labels The names given to the bins. 
#' Enter both paramaters as vectors.
#' @return The new categoric variable
#' @export
#'

cat_split <- function(bins, labels, num_var){
  var <- cut(num_var, bins, labels)
  return var
}