#' Customizing plots
#'
#' This function allows easily customizable plots.
#'
#'
#' @param plot The graph
#' @param x_axis_label The title of the x axis
#' @param y_axis_label The title of the y axis
#' @param title The title of the plot
#' @return A new labelled plot
#' @export

library(ggplot2)

customize <- function(plot, x_axis_label, y_axis_label, title){
  plot + labs(y=y_axis_label, x=x_axis_label)+ ggtitle(title);
}

devtools::document()
