#creates the integral of the spectrum

Spec_int <- function(spectrum, sensitivity) {
  temp <- spectrum$Bestrahlungsstaerke * Specs$AS_wide[[sensitivity]]
  temp <- sum(temp)
  temp
}

#Plot-Hull for all spectral plots

Plot_hull <- function(Spectrum, 
                      lang_setting = get("lang_setting", 
                                         envir = rlang::caller_env(n = 1)
                                         ), size_font = 15) {
  #Plotdata comes from outside the function
  ggplot2::ggplot(data = Spectrum$Spectrum,
                  ggplot2::aes(
                    x = Wellenlaenge, 
                    y = Bestrahlungsstaerke * 1000
                    )
                  ) +
    #general settings for the labs
    ggplot2::labs(x = lang$server(100), title = Spectrum$Name) +
    ggplot2::ylab(bquote(.(lang$server(40)) ~  ~ mW / (m ^ { 2 } * '*' * nm))) +
    #settings for the scales
    ggplot2::scale_fill_gradientn(colors = ColorP$regenbogen, guide = "none") +
    ggplot2::scale_x_continuous(breaks = c(400, 500, 600, 700, 780)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .1))) +
    ggplot2::coord_cartesian(ylim = c(0, Spectrum$maxE * 1.10)) +
    #settings for the theme
    cowplot::theme_cowplot(font_size = size_font, font_family = "sans") +
    ggplot2::theme(legend.position = "none")
}

#Plot function for the main plots
#create a Plot with the spectrum and sensitivity curves
Plot <- function(
    Spectrum, Sensitivity, subtitle, lang_setting, size_font = 15) {
  #Base plot
  Plot_hull(Spectrum, lang_setting, size_font) +
    ggplot2::labs(subtitle = subtitle) +
    
    #adding a ridgeline with a path
    ggridges::geom_ridgeline_gradient(ggplot2::aes(
      y = 0,
      height = Bestrahlungsstaerke * 1000,
      fill = Wellenlaenge
    )) +
    ggplot2::geom_path(lwd = 1.2) +
    
    #adding sensitivity curves
    ggplot2::scale_color_manual(values = Specs$Plot$Col)+
    {
      if (length(Sensitivity) != 0) {
        ggplot2::geom_path(
          data = Specs$AS_long %>% 
            dplyr::filter(Type %in% Sensitivity),
          ggplot2::aes(
            x = Wellenlaenge,
            y = rel_Sens * Spectrum$maxE,
            col = Type
          ),
          lwd = 0.75
        )
      }
    }+
    {
      if (length(Sensitivity) != 0) {
        ggrepel::geom_label_repel(
          data = Specs$Plot %>% filter(Names %in% Sensitivity),
          ggplot2::aes(
            x = Peak,
            y = Spectrum$maxE,
            label = Names,
            col = Names
          ),
          min.segment.length = 0,
          ylim = c(Spectrum$maxE, Spectrum$maxE * 1.17),
          alpha = 0.7,
          parse = TRUE,
          size = 4 / 15 * size_font
        )
      }
    }
}



#Calculates die Photonfluxdensity in quanta/(cm^2*s) from W/m^2
PD <- function(wavelength, irradiance) {
  #constants
  const_h <- 6.626 * 10 ^ -34
  const_c <- 2.998 * 10 ^ 8
  (irradiance * wavelength * 10 ^ -9) / (const_h * const_c) / 100 ^
    2
}

#Calculates the wavelength with maximal energy
LambdaMax <- function(Spectrum) {
  Spectrum %>%
    dplyr::filter(Bestrahlungsstaerke == max(Bestrahlungsstaerke)) %>%
    dplyr::pull(Wellenlaenge) %>%
    base::mean()
}

#Formats the Number-data in the tables
Number_formatting_tables <- function(data, column = Wert) {
  data %>%
    gt::fmt_number(
      columns = ({
        {
          column
        }
      }),
      rows = {
        {
          column
        }
      } >= 100,
      decimals = 0,
      sep_mark = ".",
      dec_mark = ","
    ) %>%
    gt::fmt_number(
      columns = ({
        {
          column
        }
      }),
      rows = {
        {
          column
        }
      } >= 10,
      decimals = 1,
      sep_mark = ".",
      dec_mark = ","
    ) %>%
    gt::fmt_number(
      columns = ({
        {
          column
        }
      }),
      rows = {
        {
          column
        }
      } >= 1,
      decimals = 2,
      sep_mark = ".",
      dec_mark = ","
    ) %>%
    gt::fmt_number(
      columns = ({
        {
          column
        }
      }),
      n_sigfig = 3,
      sep_mark = ".",
      dec_mark = ","
    )
}

#Creating subtitles for tables (used for exporting)
table_subtitle <- function(Type) {
  beginning <- "<b style='float:left'>"
  ending <-
    paste0(
      "</b><span style='float:right'> ",
      lang$server(43) ,
      " <b>LiTG Spectran</b> (Zauner, 2023)</span>"
    )
  title <- paste0(beginning, Type, ending)
  gt::html(title)
}

#Creating a basic table
create_table <- function(
    Spectrum, 
    Table, 
    subtitle,
    Breite = 100, 
    cols_scientific = NA,
    lang_setting
    ){
  #data selection
  Spectrum[[Table]] %>% dplyr::select(!c("Formelzeichen")) %>%
    #table generation
    gt::gt(rowname_col = "Größe") %>% 
    gt::opt_align_table_header(align = "left") %>%
    gt::tab_header(title = htmltools::strong(Spectrum$Name),
                   subtitle = subtitle) %>% 
    #formatting
    Number_formatting_tables() %>% 
    gt::fmt(
      columns = c("Zeichen"), fns = \(x) {purrr::map(x, gt::html)}) %>% 
    gt::fmt_scientific(
      columns = "Wert",
      rows = cols_scientific, 
      sep_mark = ".", 
      dec_mark = ",") %>% 
    gt::tab_options(
      column_labels.hidden = TRUE, table.width = gt::pct(Breite),
      container.padding.y = 0)
}

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
