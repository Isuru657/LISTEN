#' Rent Check 
#'
#' Checks expense sources variable to see which clients pay rent.
#' @param input_vec This is the vector containing expense information.
#' @export

rentcheck <- function(input_vec){
  l = length(input_vec)
  for (i in 1:l){
    if (str_contains(input_vec[i], "Rent") | str_contains(input_vec[i], "rent"))
    {
      pays_rent[i]=TRUE;
    }
    else
    {
      pays_rent[i]=FALSE;
    }
  }
  pays_rent <- as.factor(pays_rent) 
}