# Establishing common theme for plots
#
#
#
#
#
#
#

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

#Customizing plots
#
#
#
#
#
#
#

customize <- function(plot, x_axis_label, y_axis_label, title){
  plot + labs(y=y_axis_label, x=x_axis_label)+ ggtitle(title);
}

#Getting formattable tables
#
#
#
#
#
#
#

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
