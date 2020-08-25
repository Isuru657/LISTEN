#' Establishing common theme for plots
#'
#' This function serves to establish consistency across all of LISTEN's plots.
#' It manipulates the theme element provided through ggplot2 to customize the
#' default black and white theme.
#'
#'
#' @param categorycheck String that checks if plot contains categories
#' @return A string that checks if the plot contains categoric variables.
#' @export

library(ggplot2)
theme_graph <- function(categorycheck){
  if (categorycheck=="TRUE"){
    theme_bw(base_size=12, base_family= "Arial") %+replace%
      theme(
        panel.background  = element_blank(),
        legend.background = element_rect(fill="transparent", colour=NA),
        legend.key        = element_rect(fill="transparent", colour=NA),
        plot.title        = element_text(hjust=0.5),
        axis.text.x=element_text(angle=90, hjust=1)
      )
  }
  else {
    theme_bw(base_size=12, base_family= "Arial") %+replace%
      theme(
        panel.background  = element_blank(),
        legend.background = element_rect(fill="transparent", colour=NA),
        legend.key        = element_rect(fill="transparent", colour=NA),
        plot.title        = element_text(hjust=0.5)
      )
  }
}

devtools::document()

