#' Getting formattable tables
#'
#' This function transforms two columns and three column dataframes into a presentable form to be used in official reports.
#' Dataframes with more variables were not typically used in my analysis.
#'
#'
#' @param table A dataframe
#' @param names Column names of the table
#' @param Number of variables in the table
#' @param keyCol The names of the Column whose values will have a different font
#' @export createVarTable


createVarTable <- function(table, names, varNum, keyCol){

  if (varNum=="Two"){
    colnames(table) <- names
    formattable(table,
                align =c("l", "r"),
                list(`keyCol` = formatter(
                  "span", style = ~ style(color = "grey",font.weight = "bold"))
                )
    )
  }
  if (varNum=="Three"){
    colnames(table) <- names
    formattable(table,
                align =c("l", "c", "r"),
                list(`keyCol` = formatter(
                  "span", style = ~ style(color = "grey",font.weight = "bold"))
                )
    )
  }

}
setwd("/Users/isuruabeysekara/Desktop/LISTEN/ldaf")
devtools::document()
