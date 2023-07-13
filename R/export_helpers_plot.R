#Export plot
plot_exp <- function(...,
                     f,
                     export_tab, 
                     feed,
                     plot_height,
                     plot_width,
                     Plot_multiplikator,
                     addon = \() {NULL},
                     Export,
                     plot_height_new,
                     Spectrum_Name,
                     subtitle,
                     font_size) {
  
  if(export_tab & feed != lang$server(129)) {
  #Main Plot
  p1 <- f(..., font_size = font_size, 
          Spectrum_Name = NULL, subtitle = NULL)
  
  #Add a conditional table
    #First, remove the plotlabels (the table has them)
    # p1 <- p1 + ggplot2::ggtitle(label = NULL, subtitle = NULL)
    
    #then, add the table
    p1 / Export$Table_pics[[feed]] +
    patchwork::plot_layout(
      heights = ggplot2::unit(
        c(plot_height*Plot_multiplikator, 1 ),
        c("in", "null")
      )) +
        patchwork::plot_annotation(
          theme = ggplot2::theme(
            plot.margin = ggplot2::margin(0,0,0,0)
            )
          )
    # )
  }
  #if no table, then add an appropriate subtitle
  else {
    p1 <- f(..., 
            font_size = font_size,
            Spectrum_Name = Spectrum_Name, 
            subtitle = subtitle) + footnote(font_size) + addon()
    p1
  }

}

