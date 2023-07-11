#Footnotes for plots

footnote <- function(font_size) {
  foottext <- paste0(lang$server(43),"**LiTG Spectran** (Zauner, 2023)")
  list(
    # ggplot2::labs(caption = foottext),
    patchwork::plot_annotation(caption = foottext,
                               theme =
                                 ggplot2::theme(plot.caption = 
                                                  ggtext::element_markdown(
                                                    size = font_size/3*2
                                                  ),
                                                plot.margin = 
                                                  ggplot2::margin()))
    # ggplot2::theme(plot.caption = ggtext::element_markdown())
  )
}

#function to store a list of arguments for the plot export
plot_exp_args <- function(...,
                          Analysis, 
                          val, 
                          feed, 
                          Spectrum_Name,
                          numConv) {
  #Conditional plot - needs to be chosen under define Output
  if(val == 1) {
    #Settings for all but the Comparisons Plot
    if(feed != lang$server(129) & feed!= lang$server(126)) {
      
      args <- 
        c(
          Analysis[[ns_plot(feed)]]$args,
          f = rlang::sym(Analysis[[ns_plot(feed)]]$fun),
          list(...),
          feed = feed
        )
      
      
      args$maxE <- if(!is.null(args$maxE)) {
        ifelse(is.na(numConv), args$maxE, numConv)}
      args$Spectrum_Name <- Spectrum_Name
      args
      
    }
    #Settings for the Comparison Plot
    else if(feed != lang$server(126)){
      c(list(
        Sensitivity_Overview = Analysis$Settings$general),
        f = rlang::sym("Plot_Compare"),
        list(...),
        # font_size = font_size,
        feed = feed,
        Spectrum_Name = Spectrum_Name,
        addon =
          \() {ggplot2::ggtitle(
            label = Analysis$Settings$Spectrum_Name,
            subtitle = lang$server(50))}
      )
    }
  }
}

#function to download individual Plots in a (temporary) file for later export
plot_download <- function(..., 
                          Spectrum_Name = NULL,
                          feed = NULL,
                          Export_files,
                          plot_height = NULL,
                          plot_width = NULL,
                          dpi = 300,
                          font_size,
                          Export,
                          export_tab
) {
  
  # #Test to check whether there are any plots to save
  if(!is.null(feed)){
    
    #create a filename for the plot
    filename <-
      paste(Spectrum_Name, "_", feed, "_", Sys.Date(), ".pdf", sep="")
    
    #adjust the plot height if needed
    if(export_tab & feed != lang$server(129)) {
    plot_height_new = plot_height_recalc(plot_height, Export$Table_pics[[feed]])
    } 
    else plot_height_new = plot_height
    
    #save the plot
    ggplot2::ggsave(
      filename,
      plot =
        do.call("plot_exp", args = list(...,
                                        font_size = font_size,
                                        feed = feed,
                                        Spectrum_Name = Spectrum_Name,
                                        plot_height = plot_height,
                                        plot_width = plot_width,
                                        export_tab = export_tab,
                                        Export = Export,
                                        plot_height_new = plot_height_new)),
      device = "pdf",
      width = plot_width,
      units = "in",
      height = plot_height_new,
      dpi = dpi
    )
    
    #append the plot filename to the list of filenames
    Export_files(c(Export_files(), filename))
  }
  # setProgress((length(Plot_data$files)+ length(Plot_data$tables))/Plot_data$n_export, detail = paste(length(Plot_data$files), lang$server(106), length(Plot_data$files), lang$server(107)))
}

#rename the output files with a sequence
renaming <- function(data, Spectrum_Name) {
  stringr::str_replace(
    data, pattern = Spectrum_Name, paste0(Spectrum_Name, "_", 01:length(data))
  )
}

#set column names for the excel export
col_names_export <- function() {
  c(lang$server(120), 
    lang$server(122),
    lang$server(123), 
    lang$server(124)
  )
}

#write to an excel sheet
excel_sheet <- function(wb, data, name){
  if(!is.null(data)){
    data2 <- data
    names(data2) <- 
      openxlsx::addWorksheet(wb, name)
    openxlsx::writeData(wb, sheet = name, data2)
  }}

#select a table and prepare it for export in an excel worksheet
table_export_prep <- 
  function(
    feed,
    val,
    Analysis
  ) {
    if(val == 1) {
      if(feed != lang$server(129) & feed != lang$server(126)) {
        Analysis[[ns_table(feed)]]$internal %>% 
          dplyr::select(!Zeichen) %>% 
          {rbind(col_names_export(), . )}
      }
      else NULL
    }
  }

#function to store a list of arguments for the plot export
table_exp_args <- function(..., Analysis, val, feed, Alpha) {
  #Conditional plot - needs to be chosen under define Output
  if(val == 1 & feed != lang$server(129)) {
    if(feed != lang$server(126)){
    #Settings for all but the Comparisons Plot
    # if(!(feed %in% Specs$Alpha$names)) {
      args <- 
        c(
          Analysis[[ns_table(feed)]]$output,
          f =  rlang::sym(Analysis[[ns_table(feed)]]$fun),
          list(...),
          feed = feed
        )
    }
    #Settings for an alpha table
    else {
      args <- list(
        f = rlang::sym("Table_alpha"),
        ...,
        Table = Analysis[[ns_table("alpha")]]$internal,
        Alpha = Alpha,
        feed = feed,
        subtitle = lang$server(110)
      )
    }
    args$Spectrum_Name <- Analysis$Settings$Spectrum_Name
    args
  }
}

# function to save a table
table_save <- function(file, ..., plot_width) {
  args <- list(...)
  gt::gtsave(data = do.call("table_exp", args = args), 
         filename = file, 
         vwidth = ceiling(plot_width*133)
         )
}

#Function to create a table for download
table_download <- function(...,
                           feed, 
                           f, 
                           Spectrum_Name,
                           export_tab,
                           export_typ,
                           plot_height,
                           Export,
                           Age = NULL){
  if(!is.null(feed)){

    if(!(!export_tab & feed %in% c(Specs$Alpha$names))) {
    #create a filename
    filename <- paste(Spectrum_Name, "_", feed, "_", Sys.Date(), ".png", sep="")
    
    #save the table to the file
    table_save(file = filename, 
               f = f, 
               ..., 
               Spectrum_Name = Spectrum_Name
               )
    
    #put the files in memory
    Export$Table_pics[[feed]] <- png::readPNG(filename, native = TRUE)
    
    #only download certain tables
    if(!any(feed %in% Specs$Alpha$names == TRUE,
            feed %in% c(lang$server(127), lang$server(128)) & 
            Age[[lang$server(125)]] == 1,
            feed %in% c(lang$server(127), lang$server(128)) & 
            Age[[lang$server(127)]] == 1 &
            Age[[lang$server(128)]] == 1) & lang$ui(144) %in% export_typ){
      Export$Tables <- c(Export$Tables, filename)}
    }
    # incProgress(1/sum(Table_data$tabelle[[6]]), detail = paste(lang$server(112), length(Plot_data$tables), lang$server(113)))
  }
}

#plot_height_recalculation with table compbined graph
plot_height_recalc <- function(plot_height, png) {
plot_height*0.9+(dim(png)[1]+40)/240
}