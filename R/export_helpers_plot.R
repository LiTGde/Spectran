#Export plot
plot_exp <- function(...,
                     f,
                     numConv, 
                     export_tab, 
                     feed,
                     plot_height,
                     Plot_multiplikator,
                     font_size,
                     addon = \() {NULL}) {
  #Main Plot
  p1 <- f(..., font_size)
  #Set the Plotheight if a numer is given
  if(!is.na(numConv)) {
    p1 <- p1 + ggplot2::coord_cartesian(ylim = c(0,numConv))
  }
  #Add a conditional table
  if(export_tab & feed != "Alpha_comp") {
    #First, remove the plotlabels (the table has them)
    p1 <- p1 + ggplot2::ggtitle(label = NULL, subtitle = NULL)
    
    #then, add the table
    p1 / Bilder_Tabellen[[feed]] + patchwork::plot_layout(
      heights = ggplot2::unit(
        c(plot_height*Plot_multiplikator, 1 ), 
        c("in", "null")
      )
    )
  }
  #if no table, then add an appropriate subtitle
  else {
    p1 <- p1 + footnote() + addon()
    p1
  }

}
