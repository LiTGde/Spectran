#Plot-Hull for all spectral plots

Plot_hull <- function(Spectrum, 
                      Spectrum_Name,
                      maxE,
                      lang_setting = get("lang_setting", 
                                         envir = rlang::caller_env(n = 1)
                                         ), size_font = 15) {
  #Plotdata comes from outside the function
  ggplot2::ggplot(data = Spectrum,
                  ggplot2::aes(
                    x = Wellenlaenge, 
                    y = Bestrahlungsstaerke * 1000
                    )
                  ) +
    #general settings for the labs
    ggplot2::labs(x = lang$server(100), title = Spectrum_Name) +
    ggplot2::ylab(bquote(.(lang$server(40)) ~  ~ mW / (m ^ { 2 } * '*' * nm))) +
    #settings for the scales
    ggplot2::scale_fill_gradientn(colors = ColorP$regenbogen, guide = "none") +
    ggplot2::scale_x_continuous(breaks = c(400, 500, 600, 700, 780)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .1))) +
    ggplot2::coord_cartesian(ylim = c(0, maxE * 1.10)) +
    #settings for the theme
    cowplot::theme_cowplot(font_size = size_font, font_family = "sans") +
    ggplot2::theme(legend.position = "none")
}

#Plot function for the main plots
#create a Plot with the spectrum and sensitivity curves
Plot_Main <- function(
    Spectrum, 
    Spectrum_Name,
    maxE,
    Sensitivity, 
    Sensitivity_Spectrum = NULL,
    subtitle, 
    alpha = 1,
    lang_setting, 
    size_font = 15
    ) {
  #Base plot
  Plot_hull(Spectrum, Spectrum_Name, maxE, lang_setting, size_font) +
    ggplot2::labs(subtitle = subtitle) +
    
    #adding a ridgeline with a path
      {
        if (alpha != 0) {
          ggridges::geom_ridgeline_gradient(ggplot2::aes(
            y = 0,
            height = Bestrahlungsstaerke * 1000,
            fill = Wellenlaenge
          ),
          col = NA)
        }
      }  +
        
    #adding alpha to the ridgeline
      {
        if (alpha %>% dplyr::between(0.01, 0.99)) {
          ggridges::geom_ridgeline(
            ggplot2::aes(y = 0,
                         height = Bestrahlungsstaerke * 1000),
            fill = "white",
            alpha = alpha,
            col = NA
          )
        }
      }  +
        
    ggplot2::geom_path(lwd = 1.2, lty = (if(alpha == 1) 1 else 2)) +
    
    #adding a second ridgeline if a spectral weighing function is added
      {
        if (!is.null(Sensitivity_Spectrum)) {
          ggridges::geom_ridgeline_gradient(
            ggplot2::aes(y = 0,
                         height = Sensitivity_Spectrum * 1000,
                         fill = Wellenlaenge),
            size = 1.2
          )
        }
      }  +
        
    #adding sensitivity curves
    ggplot2::scale_color_manual(values = Specs$Plot$Col)+
    {
      if (length(Sensitivity) != 0) {
        ggplot2::geom_path(
          data = Specs$AS_long %>%
            dplyr::filter(Type %in% Sensitivity),
          ggplot2::aes(
            x = Wellenlaenge,
            y = rel_Sens * maxE,
            col = Type
          ),
          lwd = 0.75
        )
      }
    }+
    {
      if (length(Sensitivity) != 0) {
        ggrepel::geom_label_repel(
          data = Specs$Plot %>% dplyr::filter(Names %in% Sensitivity),
          ggplot2::aes(
            x = Peak,
            y = maxE,
            label = Names,
            col = Names
          ),
          min.segment.length = 0,
          ylim = c(maxE, maxE * 1.17),
          alpha = 0.7,
          parse = TRUE,
          size = 4 / 15 * size_font
        )
      }
    }
}

#Funktionen für die Erzeugung eines Plotoutputs abhängig der Wirkfunktion

# plot_vergleich_exp <- function(...){
#   plot_vergleich(...)+labs(title = spec_name(), subtitle = lang_server[50]) + footnote + footnote_t
# }

#Plotfunction for the R-Testcolors
Plot_Ra <- function(CRI_table, size_font = 15, lang_setting){
  ggplot2::ggplot(data = CRI_table, 
                  ggplot2::aes(x=Testfarbe, y = CRI, fill = Testfarbe))+
    ggplot2::geom_bar(stat = "identity", col = "black")+
    ggplot2::geom_text(ggplot2::aes(label = round(CRI,0), y= CRI-1), 
                       hjust = 1, size = size_font/3)+
    ggplot2::labs(x = lang$server(51), y = lang$server(52))+
    cowplot::theme_cowplot(font_size = size_font, font_family = "sans")+
    ggplot2::scale_fill_manual(values = rev(ColorP$Color_Rendering), 
                               guide = "none")+
    ggplot2::coord_flip(ylim = c(0,100))
}

#Plotfunction for the Spectral Irradiances
Plot_Compare <- function(Sensitivity_Overview, size_font = 15, lang_setting) {
  #Setup
  ggplot2::ggplot(
    data = Sensitivity_Overview, 
    ggplot2::aes(
      x=factor(Abbr, levels = unique(Abbr)), 
      y = E*1000, 
      fill = Names))+
    #Settings
    ggplot2::labs(x = lang$server(49))+
    cowplot::theme_cowplot(font_size = size_font, font_family = "sans")+
    ggplot2::ylab(
      bquote(.(lang$server(130))~.(lang$server(40))~~mW/(m^{2}*'*'*nm))
      )+
    ggplot2::scale_x_discrete(labels = scales::parse_format())+
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .1)))+
    ggplot2::scale_fill_manual(
      values = Specs$Plot$Col,
      guide = "none"
      )+
    #Geoms
    ggplot2::geom_bar(stat = "identity", col = "black")
}

#Combined Plot
Plot_Combi <- function(Spectrum, 
                       Spectrum_Name,
                       maxE,
                       Sensitivity, 
                       Sensitivity_Spectrum,
                       Sensitivity_Overview,
                       subtitle = NULL,
                       alpha = 1,
                       lang_setting, 
                       size_font = 15,
                       Second_plot,
                       Name = NULL,
                       CRI = NULL) {
  p1 <- Plot_Main(Spectrum = Spectrum,
                  Spectrum_Name = Spectrum_Name,
                  maxE = maxE,
            Sensitivity = Sensitivity,
            Sensitivity_Spectrum = Sensitivity_Spectrum,
            subtitle = subtitle,
            alpha = alpha,
            lang_setting = lang_setting,
            size_font = size_font)
    
    if(Second_plot == TRUE & !is.null(Name)) {
      p1 +
      Plot_Compare(Sensitivity_Overview = Sensitivity_Overview, 
              size_font = size_font, 
              lang_setting = lang_setting) +
        gghighlight::gghighlight(
          label_key = Abbr,
          Names == Name,
          unhighlighted_params = list(alpha = 0.2, fill = NULL))+
        
        patchwork::plot_layout(widths = c(2,1))
    }
    else if(Second_plot == TRUE) {
      p1 +
      Plot_Ra(CRI_table = CRI,
              size_font = size_font,
              lang_setting = lang_setting) +
        patchwork::plot_layout(widths = c(2,1))
    }
  else p1
  
}

plot_allgemein <- function(name, Beschreibung, Bedingung, Kuerzel3, Bedingung2, col, size_font = 15, Titel = NULL, Bedingung3 = FALSE, Bedingung4 = TRUE) {
  Typ2 <- Rezeptorkurven[[name]]
  p1 <- Plot_01(size_font) +
    labs(subtitle = Beschreibung)+
    {if(Bedingung4){
      geom_bar(stat = "identity", aes(col = Wellenlaenge, fill = Wellenlaenge, y = Bestrahlungsstaerke*1000), alpha = 0.03, lwd = 1)}}+
    geom_ridgeline_gradient(aes(y = 0, height =Bestrahlungsstaerke*1000*Typ2, fill = Wellenlaenge))+
    geom_path(lwd = 0.75, lty = 5) +
    geom_path(aes(y = Bestrahlungsstaerke*1000*Typ2), lwd = 1)+
    {if(Bedingung){
      geom_path(data = Rezeptorkurven2 %>% filter(Typ %in% name),
                aes(x=Wellenlaenge, y= rel_Empfindlichkeit*max_E(), group = Typ), col = col, lwd = 0.75)}}+
    {if(Bedingung){
      geom_label_repel(data = Bewertung() %>% filter(Bezeichnung %in% name),
                       aes(x=Peak, y= max_E(), label = Bezeichnung), col = col,
                       min.segment.length = 0,ylim = c(max_E(), max_E()*1.17), parse = TRUE, size = 4/15*size_font)}}


  if(Bedingung2) {
    p1 +
      Titel +
      plot_vergleich(size_font)+
      gghighlight::gghighlight(
        Kürzel == Kuerzel3,
        unhighlighted_params = list(alpha = 0.2, fill = NULL))+
      plot_layout(widths = c(2,1))}

  else if(Bedingung3){
    p1 +
      Titel +
      plot_testfarben(size_font)+
      plot_layout(widths = c(2,1))}

  else p1 + Titel

}

# 
# Bewertung_Plots <- function(name, ...) {
#   output[[paste0("Uebersichtsplot_", name)]] <- renderPlot({
#     req(data(), Plotbreite_temp())
#     plot_allgemein(name, ...)
#   },height = 350, width = reactive(Plotbreite_temp()))
#   outputOptions(output, paste0("Uebersichtsplot_", name), suspendWhenHidden = FALSE)}


# #Nimmt das Plot-Resizing wieder vor, sobald sich die Fensterbreite ändert
# observeEvent(input$dimension, {obs$resume()})

#     #Spektraldaten
#     Spectral_data <- reactive({
#       req(data())
#       colorSpec(data = data()$Bestrahlungsstaerke, wavelength = data()$Wellenlaenge, specnames = spec_name())
#     })

#     #Container für die Breite der Ausgabeplots
#     Plotbreite_temp <- reactiveVal(value = 500)
# 
#     obs <- observe({
#       Plotbreite_temp(session$clientData$output_Plotbreite_width)
#     }) %>% bindEvent(session$clientData$output_Plotbreite_width)
#     # 
