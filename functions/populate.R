#' Extracts income and expense sources 
#'
#' The clients dataset contains information on expense and income sources for individual clients.
#' It is possible to split the dataset to obtain information on rent. This functoin makes this possible.
#' @param input_vec This is the vector containing strings that need to be split
#' @export

populate <- function(input_vec){
  l <- length(input_vec)
  source_case <- vector(mode="character", length=l)
  amount_case <- vector(mode="character", length=l)
  freq_case <- vector(mode="character", length=l)
  
  for (i in 1:lz){
    source_case[i]=z[[i]][1]
    amount_case[i]=z[[i]][2]
    freq_case[i]=z[[i]][3]
  }
}
  