#Plot-Hull for all spectral plots

spectral_irradiance_axis_label <- function() {
  title <- if (identical(transmission_language_setting(), "Deutsch"))
    "Spektrale Bestrahlungsst\u00e4rke" else "Spectral irradiance"
  paste0(title, "\n(mW/m\u00b2/nm)")
}

Plot_hull <- function(Spectrum,
                      Spectrum_Name,
                      maxE,
                      font_size = 15) {
  #Plotdata comes from outside the function
  ggplot2::ggplot(data = Spectrum,
                  ggplot2::aes(
                    x = Wellenlaenge, 
                    y = Bestrahlungsstaerke * 1000
                    )
                  ) +
    #general settings for the labs
    ggplot2::labs(x = lang$server(100), title = Spectrum_Name) +
    ggplot2::ylab(spectral_irradiance_axis_label()) +
    #settings for the scales
    ggplot2::scale_fill_gradientn(colors = ColorP[[the$palette]], guide = "none") +
    ggplot2::scale_x_continuous(breaks = c(400, 500, 600, 700, 780)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .1))) +
    ggplot2::coord_cartesian(ylim = c(0, maxE * 1.10)) +
    #settings for the theme
    cowplot::theme_cowplot(font_size = font_size, font_family = "sans") +
    ggplot2::theme(legend.position = "none", 
                   plot.subtitle = ggtext::element_textbox_simple())
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
    font_size = 15
    ) {
  #Base plot
  Plot_hull(Spectrum, Spectrum_Name, maxE, font_size) +
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
        
    ggplot2::geom_path(linewidth = (if(alpha == 1) 1.2 else 0.5),
                       lty = (if(alpha == 1) 1 else 2)) +
    
    #adding a second ridgeline if a spectral weighing function is added
      {
        if (!is.null(Sensitivity_Spectrum)) {
          list(ggridges::geom_ridgeline_gradient(
            ggplot2::aes(y = 0,
                         height = Sensitivity_Spectrum * 1000,
                         fill = Wellenlaenge),
            colour = NA
          ), ggplot2::geom_path(
            ggplot2::aes(y = Sensitivity_Spectrum * 1000), linewidth = 1.2
          ))
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
          size = 4 / 15 * font_size
        )
      }
    }
}

#Funktionen für die Erzeugung eines Plotoutputs abhängig der Wirkfunktion

# plot_vergleich_exp <- function(...){
#   plot_vergleich(...)+labs(title = spec_name(), subtitle = lang_server[50]) + footnote + footnote_t
# }

#Plotfunction for the R-Testcolors
Plot_Ra <- function(CRI_table, font_size = 15, maxE = NULL){
  ggplot2::ggplot(data = CRI_table, 
                  ggplot2::aes(x=Testfarbe, y = CRI, fill = Testfarbe))+
    ggplot2::geom_bar(stat = "identity", col = "black")+
    ggplot2::geom_text(ggplot2::aes(label = round(CRI,0), y= ifelse(CRI <= 18, 18, CRI-1)), 
                       hjust = 1, size = font_size/3)+
    ggplot2::labs(x = lang$server(51), y = lang$server(52))+
    cowplot::theme_cowplot(font_size = font_size, font_family = "sans")+
    ggplot2::scale_fill_manual(values = rev(ColorP$Color_Rendering), 
                               guide = "none")+
    ggplot2::coord_flip(ylim = c(0,100))
}

#Plotfunction for the Spectral Irradiances
Plot_Compare <- function(Sensitivity_Overview, 
                         font_size = 15,
                         Spectrum_Name = NULL,
                         maxE = NULL,
                         subtitle = NULL) {
  #Setup
  ggplot2::ggplot(
    data = Sensitivity_Overview, 
    ggplot2::aes(
      x=factor(Abbr, levels = unique(Abbr)), 
      y = E*1000, 
      fill = Names))+
    #Settings
    ggplot2::labs(x = lang$server(49))+
    cowplot::theme_cowplot(font_size = font_size, font_family = "sans")+
    ggplot2::ylab(
      bquote(.(lang$server(130))~.(lang$server(40))~~mW/(m^{2}))
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
                       Sensitivity_Overview = NULL,
                       subtitle = NULL,
                       alpha = 1,
                       font_size = 15,
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
            font_size = font_size)
    
    if(Second_plot == TRUE & !is.null(Name)) {
      p1 +
      Plot_Compare(Sensitivity_Overview = Sensitivity_Overview, 
              font_size = font_size) +
        gghighlight::gghighlight(
          label_key = Abbr,
          Names == Name,
          unhighlighted_params = list(alpha = 0.2, fill = NULL))+
        
        patchwork::plot_layout(widths = c(2,1))
    }
    else if(Second_plot == TRUE) {
      p1 +
      Plot_Ra(CRI_table = CRI,
              font_size = font_size) +
        patchwork::plot_layout(widths = c(2,1))
    }
  else p1
  
}

#Plot to show age dependency of the pupil
Plot_age_pup <- function(Age,
                         font_size = 15,
                         Spectrum_Name = "filler",
                         subtitle = NULL){
  #calculate the coefficient(s)
  k_pup <- k_pup_fun(Age)

  #draw the plot
  p1 <- 
    #draw the function
    ggplot2::ggplot(data = tibble::tibble(Age = 0:100),
                    ggplot2::aes(x=Age))+
    ggplot2::geom_function(fun = k_pup_fun, lwd = 1, xlim = c(0, 200)) +
    
    #plotsettings
    ggplot2::labs(title = if(!is.null(Spectrum_Name)) lang$server(92))+
    cowplot::theme_cowplot(font_size = font_size, font_family = "sans") +
    ggplot2::ylab(label = lang$server(93)) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(scale = 100)) +
    ggplot2::scale_x_continuous(breaks=c(0, 25, 32, 50, 75, 100)) +
    ggplot2::xlab(label = lang$ui(116)) +
    ggplot2::coord_cartesian(xlim = c(0, 100), ylim = c(0, 1.43)) +
    
    #plot annotations
    ggplot2::annotate(
      "point", x= 32, y = 1, col = "grey60", size = 5) +
    ggplot2::annotate(
      "segment", x= 32, xend = 32, 
      y = -1, yend = 1, col = "grey60", lty = 5) +
    ggplot2::annotate(
      "segment", x= -5, xend = 32, 
      y = 1, yend = 1, col = "grey60", lty = 5) +
    ggplot2::annotate(
      "point", x= Age, y = k_pup_fun(Age), col = "red", size = 5) +
    ggplot2::annotate(
      "segment", x= Age, xend = Age,
      y = -1, yend = k_pup, col = "red", lty = 5) +
    ggplot2::annotate(
      "segment", x= -5, xend = Age, 
      y = k_pup, yend = k_pup, col = "red", lty = 5) +
    ggplot2::annotate(
      "label", x= 0, y = k_pup, col = "red",
      label = paste0(round(k_pup*100,0),"%"), size = 4/15*font_size) +
    ggplot2::annotate(
      "label", x= Age, y = 0, col = "red", 
      label = Age, size = 4/15 * font_size)
  
  p1
}


# Narrow plots keep the legend outside the spectrum. Width is supplied by the
# Shiny output or export settings, so screen and file layouts use the same rule.
age_plot_is_compact <- function(plot_width, font_size = 15) {
  length(plot_width) == 1L && is.finite(plot_width) && plot_width <= font_size / 3
}

#Age-dependent basis plot
Plot_age_basis <- function(
    font_size = 15,
    Spectrum, 
    Spectrum_Name,
    maxE,
    plot_multiplier,
    subtitle,
    plot_width = NULL
    ) {
  compact <- age_plot_is_compact(plot_width, font_size)
  if (compact) font_size <- min(font_size, 12)
  compact_export <- compact && font_size <= 10 && plot_width >= 2.8
  axis_label <- spectral_irradiance_axis_label()
  if (compact) {
    axis_label <- sub("^(Spectral|Spektrale) ", "\\1\n", axis_label)
  }
  #Plotdata comes from outside the function
  ggplot2::ggplot(data = Spectrum,
                  ggplot2::aes(
                    x = Wellenlaenge, 
                    y = Bestrahlungsstaerke * 1000
                    )
                  ) +
    #general settings for the labs
    ggplot2::labs(x = lang$server(100), title = Spectrum_Name) +
    ggplot2::labs(subtitle = subtitle) +
    ggplot2::ylab(axis_label) +
    #settings for the scales
    ggplot2::scale_fill_gradientn(colors = ColorP[[the$palette]],
                                  guide = "none") +
    ggplot2::scale_x_continuous(breaks = c(400, 500, 600, 700, 780)) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, .1))) +
    ggplot2::coord_cartesian(
      xlim = c(380, 780),
      ylim = c(0, maxE*1.1*plot_multiplier)) +
  #settings for the theme
    cowplot::theme_cowplot(font_size, font_family = "sans") +
    ggplot2::theme(
      plot.title = ggtext::element_textbox_simple(face = "bold", hjust = 0,
        margin = ggplot2::margin(b = 5)),
      plot.subtitle = ggtext::element_textbox_simple(hjust = 0,
        margin = ggplot2::margin(b = 8)),
      plot.title.position = if (compact) "plot" else "panel",
      axis.title.y = ggplot2::element_text(size = if (compact) font_size * .85 else font_size),
      legend.position = if (compact) "bottom" else "inside",
      legend.position.inside = c(0.955, 0.07),
      legend.location = if (compact) "plot" else "panel",
      legend.direction = "vertical",
      legend.key.width=grid::unit(if (compact_export) 0.6 else if (compact) 0.85 else 1.25,"cm"),
      legend.text = ggplot2::element_text(size = if (compact_export) font_size * .8 else font_size),
      legend.text.align = 0,
      legend.justification = if (compact) "left" else c(1,0),
      legend.margin = ggplot2::margin(t = 4),
      legend.background = ggplot2::element_rect(fill = "#FFFFFFDD"))

}
  
#Adding rigdes with alpha
Ridges_alpha <- function(plot, y, alpha, alpha_mod) {

    if (alpha != 0) {
      plot <- plot +
        ggridges::geom_ridgeline_gradient(
          ggplot2::aes(
          y = 0,
          height = {{ y }} * 1000,
          fill = Wellenlaenge
        ),
        col = NA) +
        ggridges::geom_ridgeline(
          ggplot2::aes(y = 0,
                       height = {{ y }} * 1000),
          fill = "white",
          alpha = alpha + alpha_mod,
          col = NA
        )
    }
  
  plot 

}
#Adding Outlines
Outlines_alpha <- function(plot, y, lty, size) {

  plot <- 
    plot + ggplot2::geom_path(
        ggplot2::aes(y= {{ y }} *1000, lty = lty, lwd = size))
  
  plot 

}

# Age dependent Summary plot
Plot_age_tot <- function(..., 
                         Spectrum_Name,
                         plot_multiplier,
                         subtitle,
                         maxE,
                         Spectrum,
                         Age,
                         alpha, 
                         Spectrum_mel_wtd,
                         Alter_mel,
                         font_size = 15,
                         plot_width = NULL){

  k_pup <- k_pup_fun(Age)
  Tau_rel <- Tau_rel_fun(Age)

  p1 <- Plot_age_basis(Spectrum = Spectrum, 
                       Spectrum_Name = Spectrum_Name,
                       maxE = maxE, 
                       plot_multiplier = plot_multiplier,
                       font_size = font_size,
                       subtitle = subtitle,
                       plot_width = plot_width)
  #adding the various ridgelines
  p1 <- Ridges_alpha(p1, Bestrahlungsstaerke, alpha, 0.1)
  p1 <- Ridges_alpha(p1, Spectrum_mel_wtd, alpha, -0.1)
  p1 <- Ridges_alpha(p1, Spectrum_mel_wtd*Tau_rel, alpha, -0.3)
  p1 <- Ridges_alpha(p1, Spectrum_mel_wtd*Tau_rel*k_pup, 1, -1)

  #adding the various outlines
  p1 <- Outlines_alpha(p1, Bestrahlungsstaerke, "1", "1")
  p1 <- Outlines_alpha(p1, Spectrum_mel_wtd, "2", "1")
  p1 <- Outlines_alpha(p1, Spectrum_mel_wtd*Tau_rel, "3", "1")
  p1 <- Outlines_alpha(p1, Spectrum_mel_wtd*Tau_rel*k_pup, "4", "2")

  #adding the legend
  p1 <- p1 +
    ggplot2::scale_linetype_manual(
    name = lang$server(82),
    values = c(
      "1" = 3,
      "2" = 4,
      "3" = 2,
      "4" = 1
    ),
    labels = c(
      lang$server(83),
      lang$server(84),
      paste0(lang$server(85), Age, lang$server(86)),
      paste0(lang$server(94), Age, lang$server(86))
    )
  )+
    ggplot2::scale_linewidth_manual(
      values = c(0.5, 1.2),
      guide = NULL
    )+
    ggplot2::guides(
      linetype=ggplot2::guide_legend(
        ncol = if (age_plot_is_compact(plot_width, font_size) &&
          font_size <= 10 && plot_width >= 2.8) 2 else 1,
        byrow = TRUE,
        override.aes = list(linewidth=c(0.5, 0.5, 0.5, 1.2))
      ))
      
  #adding the sensitivity Spectrum
  if(Alter_mel){
    p1 <- p1 +
      ggplot2::geom_path(
        data = Specs$AS_long %>% dplyr::filter(Type %in% "Melanopsin"),
        ggplot2::aes(x = Wellenlaenge,
                     y = rel_Sens * Tau_rel * maxE * k_pup),
        col = Specs$Plot$Col[[1]],
        lwd = 0.75
      ) +
      ggrepel::geom_label_repel(
        data = Specs$Plot %>% dplyr::filter(Names %in% "Melanopsin"),
        ggplot2::aes(
          x = Peak,
          y = maxE * k_pup * Tau_rel[[111]],
          label = Names
        ),
        col = Specs$Plot$Col[[1]],
        min.segment.length = 0,
        ylim = c(0, maxE * plot_multiplier),
        nudge_x = 60, nudge_y = maxE * .2, seed = 1,
        alpha = 0.85,
        parse = TRUE,
        size = 4 / 15 * font_size
      )
    }
  p1
}

# Age dependent Tranmission plot (main plot)
Plot_age_trans_p1 <- function(..., 
                         maxE,
                         Spectrum,
                         Spectrum_Name,
                         subtitle,
                         Age,
                         alpha, 
                         Spectrum_mel_wtd,
                         Alter_mel,
                         Alter_rel,
                         age_scale,
                         plot_multiplier,
                         font_size = 15,
                         plot_width = NULL){
  Tau_rel <- Tau_rel_fun(Age)

  p1 <- Plot_age_basis(Spectrum = Spectrum, 
                       Spectrum_Name = Spectrum_Name,
                       maxE = maxE, 
                       plot_multiplier = plot_multiplier,
                       font_size = font_size,
                       subtitle = subtitle,
                       plot_width = plot_width
                       )
  #adding the various ridgelines
  p1 <- Ridges_alpha(p1, Bestrahlungsstaerke, alpha, 0.1)
  p1 <- Ridges_alpha(p1, Spectrum_mel_wtd, alpha, -0.1)
  p1 <- Ridges_alpha(p1, Spectrum_mel_wtd*Tau_rel, 1, -1)

  #adding the various outlines
  p1 <- Outlines_alpha(p1, Bestrahlungsstaerke, "1", "1")
  p1 <- Outlines_alpha(p1, Spectrum_mel_wtd, "2", "1")
  p1 <- Outlines_alpha(p1, Spectrum_mel_wtd*Tau_rel, "3", "2")

  #adding the legend
  p1 <- p1 +
    ggplot2::scale_linetype_manual(
    name = lang$server(82),
    values = c(
      "1" = 3,
      "2" = 4,
      "3" = 1
    ),
    labels = c(
      lang$server(83),
      lang$server(84),
      paste0(lang$server(85), Age, lang$server(86)),
      paste0(lang$server(94), Age, lang$server(86))
    )
  ) +
    ggplot2::scale_linewidth_manual(
      values = c(0.5, 1.2),
      guide = NULL
    )+
    ggplot2::guides(
      linetype=ggplot2::guide_legend(
        ncol = if (age_plot_is_compact(plot_width, font_size) &&
          font_size <= 10 && plot_width >= 2.8) 2 else 1,
        byrow = TRUE,
        override.aes = list(linewidth=c(0.5, 0.5, 1.2))
      ))
      
  #adding the sensitivity Spectrum
  if(Alter_mel){
    p1 <- p1 +
      ggplot2::geom_path(
        data = Specs$AS_long %>% dplyr::filter(Type %in% "Melanopsin"),
        ggplot2::aes(x = Wellenlaenge,
                     y = rel_Sens * Tau_rel * maxE),
        col = Specs$Plot$Col[[1]],
        lwd = 0.75
      ) +
      ggrepel::geom_label_repel(
        data = Specs$Plot %>% dplyr::filter(Names %in% "Melanopsin"),
        ggplot2::aes(
          x = Peak,
          y = maxE * Tau_rel[[111]],
          label = Names
        ),
        col = Specs$Plot$Col[[1]],
        min.segment.length = 0,
        ylim = c(0, maxE * plot_multiplier),
        nudge_x = 60, nudge_y = maxE * .2, seed = 1,
        alpha = 0.85,
        parse = TRUE,
        size = 4 / 15 * font_size
      )
  }
  #conditionally adding a relative transmission spectrum
  if (Alter_rel) {
    p1 <- p1 +
      ggplot2::geom_hline(
        data = age_scale,
        ggplot2::aes(yintercept = y),
        lty = 2, lwd = 0.5, col = "#ff000020"
      )+
    ggplot2::geom_path(
      ggplot2::aes(y = Tau_rel * maxE),
              col = "red",
              lwd = 0.75) +
    ggplot2::geom_label(
        data = age_scale,
        ggplot2::aes(x = x, y = y, label = label),
        hjust = 0,
        col = "red",
        size = 4  /  15  *  font_size
      ) +
    ggplot2::annotate(
        "text",
        y = maxE  *  0.575  *  plot_multiplier,
        x  =  364,
        label = lang$server(87),
        vjust = 1,
        col = "red",
        size = 5  /  15  *  font_size,
        angle = 90
      )
  }
  
  p1
}

#part 2 of the tranmsission plot
Plot_age_trans_p2 <- function(Age, 
                              Alter_mel, 
                              font_size = 15,
                              compact = FALSE) {
  Tau <- Tau32 %>% dplyr::mutate(Tau = prerecep_filter(Wellenlaenge, Age))
  #Plot
  p1 <-
    ggplot2::ggplot(
      data = Tau, 
      ggplot2::aes(Wellenlaenge, y = Tau)
      )+
    ggplot2::geom_path(
      ggplot2::aes(y=Tau32), lty = 5)+
    ggplot2::geom_path()+
    #Styling
    ggplot2::labs(x = lang$server(100))+
    ggplot2::ylab(if (compact) bquote(tau[lambda]) else
      bquote(.(lang$server(99))~paste(tau)[paste(lambda)]))+
    ggplot2::labs(title = if (compact) lang$server(99) else NULL)+
    ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 100),
      breaks = if (compact) c(0, .5, 1) else ggplot2::waiver())+
    ggplot2::coord_cartesian(ylim = c(0, 1))+
    cowplot::theme_cowplot(font_size = 8/15*font_size)+
    ggplot2::theme(
      legend.position = "none", 
      plot.background = ggplot2::element_rect(fill = "#FFFFFF90"))+
    if(Alter_mel){
      ggplot2::geom_density(
        stat = "identity", 
        data = Specs$AS_long %>% dplyr::filter(Type %in% "Melanopsin"),
        ggplot2::aes(x=Wellenlaenge, y= rel_Sens), 
        fill = Specs$Plot$Col[[1]], alpha = 0.3, col = NA, lwd = 0.75)}
  p1
}

#Combine the plots
Plot_age_trans <- function(..., 
                           Alter_inset, 
                           Age, 
                           Alter_mel, 
                           font_size = 15,
                           subtitle,
                           Spectrum_Name,
                           plot_width = NULL) {

  main <- Plot_age_trans_p1(..., font_size = font_size, Age = Age,
    Alter_mel = Alter_mel, subtitle = subtitle, Spectrum_Name = Spectrum_Name,
    plot_width = plot_width)
  if (!isTRUE(Alter_inset)) return(main)
  if (age_plot_is_compact(plot_width, font_size)) {
    return(patchwork::wrap_plots(main,
      Plot_age_trans_p2(Age, Alter_mel, font_size, compact = TRUE), ncol = 1,
      heights = c(4, 1)))
  }

  patchwork::wrap_plots(main,
    patchwork::inset_element(
      Plot_age_trans_p2(Age, Alter_mel, font_size),
      left = 0.75, bottom = 0.65, right = 0.98, top = 0.98,
      align_to = "panel")) +
    patchwork::plot_annotation(theme = ggplot2::theme(
      plot.margin = ggplot2::margin(0, 0, 0, 0)))
}

# #Nimmt das Plot-Resizing wieder vor, sobald sich die Fensterbreite ändert
# observeEvent(input$dimension, {obs$resume()})

#     #Container für die Breite der Ausgabeplots
#     Plotbreite_temp <- reactiveVal(value = 500)
# 
#     obs <- observe({
#       Plotbreite_temp(session$clientData$output_Plotbreite_width)
#     }) %>% bindEvent(session$clientData$output_Plotbreite_width)
#     # 
