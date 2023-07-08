
# UI ----------------------------------------------------------------------

export_Output_SettingsUI <- 
  function(id) {
    ns <- shiny::NS(id)
    htmltools::tagList(
      htmltools::h4(htmltools::strong(lang$ui(141))),
      shiny::checkboxGroupInput(
        ns("export_typ"),
        label = lang$ui(142),
        choices = c(
          lang$ui(143),
          lang$ui(144),
          lang$ui(145),
          lang$ui(146)
        ),
        selected = lang$ui(143),
        inline = TRUE
      ),
      shiny::checkboxInput(
        ns("export_tab"), label = lang$ui(147)),
      htmltools::h5(htmltools::strong(lang$ui(148))),
      shiny::splitLayout(
        shiny::numericInput(
          ns("plot_width"),
          label = lang$ui(149),
          min = 1,
          value = 6
        ),
        shiny::numericInput(
          ns("plot_height"),
          label = lang$ui(150),
          min = 1,
          value = 3.2
        )
      ),
      shiny::numericInput(
        ns("font_size"),
        label = lang$ui(151),
        min = 1,
        value = 9
      ),
      shinyFeedback::useShinyFeedback(),
      shiny::textInput(ns("scale_max"),
                       label = paste0(lang$ui(152), " (mW/m²*nm)"),
      )
    )
  }

# Server ------------------------------------------------------------------

export_Output_SettingsServer <- 
  function(
    id,
    Analysis,
    outputs) {
    
    shiny::moduleServer(id, function(input, output, session) {
      
      #Creates a numeric out the text Input
      numConv <- shiny::reactive({as.numeric(input$scale_max)})
      
      #Warning, if the Maximum scale is neither numeric nor empty
      shiny::observeEvent(input$scale_max, {
        shinyFeedback::feedbackDanger(
          "scale_max",
          input$scale_max != "" & (is.na(numConv()) | numConv() <= 0),
          lang$server(102)
        )
      })
      
      # Set a factor for the plotheight, if gets combined with a table
      Plot_multiplikator <- 0.8
      
      #create an export table
      Export <- shiny::reactiveValues(Plot = NULL)
      
      shiny::observe({
        req(Analysis)
        num <- outputs$exp %>% unlist() %>% sum()
        # req(num !=0)
        
        feed <- if(num != 0) Filter(\(x) {x == 1}, outputs$exp) %>% names()
        
        Export$Plot <- if(num !=0) {
          if(feed != "Alpha_comp") {
            # Export[[feed]] <-
            c(
              Analysis[[ns_plot(feed)]]$args,
              f = rlang::sym(Analysis[[ns_plot(feed)]]$fun),
              numConv = numConv(),
              export_tab = input$export_tab,
              feed = feed,
              plot_height = input$plot_height,
              Plot_multiplikator = Plot_multiplikator,
              font_size = input$font_size)
          }
          else {
            c(list(
              Sensitivity_Overview = Analysis$Settings$general),
              f = rlang::sym("Plot_Compare"),
              numConv = numConv(),
              export_tab = input$export_tab,
              feed = feed,
              plot_height = input$plot_height,
              Plot_multiplikator = Plot_multiplikator,
              font_size = input$font_size,
              addon = 
                \() {ggplot2::ggtitle(
                  label = Analysis$Settings$Spectrum_Name,
                  subtitle = lang$server(50))}
            )
          }
        }
      })
      
      #Return value
      Export
      
    })
  }

# test <- list(a = 0, b = 0, c = 0)
# erg <- Filter(\(x) {x == 1}, test)
# sum(unlist(test))
# 
# 
# #Radiometrischer Plot mit/ohne Tabellen
# 
# plot_radio2 <- function(...) {
#   
#   p1 <- plot_radio(...)
#   
#   if(!is.na(numConv())) {
#     p1 <- p1 + coord_cartesian(ylim = c(0,numConv()))
#   }
#   
#   if(input$export_tab) {
#     p1 <- p1+ggtitle(label = NULL, subtitle = NULL)
#     p1 / Bilder_Tabellen[[lang$server(39])] + plot_layout(heights = unit(c(input$plot_height*Plot_multiplikator, 1 ), c("in", "null")))}
#   else {
#     p1 <- p1 + labs(subtitle = lang$server(39)) + footnote + footnote_t
#     p1
#   }
#   
# }
# 
# #Photometrischer / Alphaopischer Plot mit/ohne Tabellen
# 
# plot_allgemein2 <- function(name, Beschreibung, ...) {
#   if(input$export_tab) {
#     adjektiv <- alpha_adjektive[alpha_werte == name]
#     table <- if(name == "V(lambda)") table_vis()
#     else table_alpha(name, adjektiv, Beschreibung)
#     Bild <- if(name == "V(lambda)") Bilder_Tabellen[[lang$server(63])]
#     else Bilder_Tabellen[[name]]
#     
#     p1 <- plot_allgemein(name, Beschreibung, ..., Titel = ggtitle(label = NULL, subtitle = NULL))
#     if(!is.na(numConv())) {
#       p1 <- p1 + coord_cartesian(ylim = c(0,numConv()))
#     }
#     p1 / Bild + plot_layout(heights = unit(c(input$plot_height*Plot_multiplikator, 1 ), c("in", "null")))}
#   
#   
#   else {
#     p1 <- plot_allgemein(name, Beschreibung, ...)
#     if(!is.na(numConv())) {
#       p1 <- p1 + coord_cartesian(ylim = c(0,numConv()))
#     }
#     p1 + footnote + footnote_t
#   }
#   
# }
# 
# #Alter Gesamt-Plot mit/ohne Tabellen
# 
# plot_alter_ges2 <- function(...) {
#   
#   p1 <- plot_alter_ges(...)
#   if(!is.na(numConv())) {
#     p1 <- p1 + coord_cartesian(ylim = c(0,numConv()))
#   }
#   
#   
#   if(input$export_tab) {
#     
#     p1 <- p1+ggtitle(label = NULL, subtitle = NULL)
#     
#     p1 / Bilder_Tabellen[[lang$server(125)]] + plot_layout(heights = unit(c(input$plot_height*Plot_multiplikator, 1 ), c("in", "null")))}
#   
#   else {
#     p1 + footnote + footnote_t
#   }
# }
# 
# #Alter Trans-Plot mit/ohne Tabellen
# plot_alter_trans2 <- function(...) {
#   
#   p1 <- plot_alter_trans_u1(...)
#   if(!is.na(numConv())) {
#     p1 <- p1 + coord_cartesian(ylim = c(0,numConv()))
#   }
#   
#   if(input$export_tab) {
#     
#     p1 <- p1 +ggtitle(label = NULL, subtitle = NULL) + plot_annotation(theme = theme(plot.margin = margin()))+ 
#       if(input$Alter_inset) {
#         inset_element(plot_alter_trans_u2(...), left = 0.75, bottom = 0.65, right = 0.98, top = 0.98, align_to = "full")
#       } 
#     p1 / Bilder_Tabellen[[lang$server(128)]] + plot_layout(heights = unit(c(input$plot_height*Plot_multiplikator, 1 ), c("in", "null")))}
#   
#   else {
#     p1 + plot_annotation(theme = theme(plot.margin = margin())) + footnote + footnote_t + 
#       if(input$Alter_inset) {
#         inset_element(plot_alter_trans_u2(...), left = 0.75, bottom = 0.65, right = 0.98, top = 0.98, align_to = "full")
#       }
#   }
#   
# }
# 
# #Alter Pupillen Plot mit/ohne Tabellen
# plot_alter_pup2 <- function(...) {
#   p1 <- plot_alter_pup(...)
#   
#   if(input$export_tab) {
#     
#     p1 <- p1 + ggtitle(label = NULL, subtitle = NULL)
#     p1 / Bilder_Tabellen[[lang$server(127)]] + plot_layout(heights = unit(c(input$plot_height*Plot_multiplikator, 1 ), c("in", "null")))}
#   
#   
#   else {
#     p1 + footnote + footnote_t
#   }
#   
# }
# 
# #Funktion für das Speichern von Plots
# plotsave <- function(file, name, args, width, height) {
#   ggsave(
#     file,
#     plot = do.call(name, args = args),
#     device = "pdf",
#     width = width,
#     height = height,
#     dpi = 200
#   )
# }
# 
# #Funktion für das Erstellen der Argumentenlliste bei den Plots der alphaopischen Bewertung
# 
# Plot_alpha <- function(name, Bezeichnung, kurzbezeichnung, col){
#   Plot_data[[name]] <- list(
#     name = name,
#     Beschreibung = Bezeichnung,
#     Bedingung = input$Spektren,
#     Kuerzel3 = kurzbezeichnung,
#     Bedingung2 = input$Vergleich_Alpha,
#     col = col,
#     size_font = input$font_size,
#     Bedingung4 = input$Hintergrund2
#   )
#   
# }
# 
# 
# 
# #Tabellen für die Erzeugung von Plots
# 
# Plot_data <- reactiveValues()
# 
# observe({
#   req(data())
#   Plot_data$rad <- list(size_font = input$font_size)
#   
#   Plot_data$vis <-  list(
#     name = "V(lambda)",
#     Beschreibung = lang$server(53),
#     Bedingung = input$"V(lambda)",
#     Kuerzel3 = "V(lambda)",
#     Bedingung2 = FALSE,
#     col = "gold1",
#     size_font = input$font_size,
#     Bedingung3 = input$Testfarben,
#     Bedingung4 = input$Hintergrund1
#   )
#   
#   pmap(list(
#     alpha_werte,
#     Beschreibungen,
#     Kuerzel2,
#     c("#1D63DC", "darkred", "limegreen", "darkviolet", "grey60")
#   ), Plot_alpha)
#   
#   Plot_data$vergleich <- list(
#     size_font = input$font_size
#   )
#   
#   Plot_data$alter_trans<- list(
#     size_font = input$font_size
#   )
#   Plot_data$alter_pup<- list(
#     size_font = input$font_size
#   )
#   Plot_data$alter_ges<- list(
#     size_font = input$font_size
#   )
#   
# })
# 
# 
# 
# #Zusammenstellen der Daten um die Downloadplots zu erzeugen
# observe({
#   req(data())
#   
#   Plot_data$tabelle <- tibble(
#     name = c(lang$server(39), lang$server(63), alpha_werte, lang$server(129), lang$server(125),
#              lang$server(127),
#              lang$server(128)),
#     
#     plot_true = c(
#       input$export_rad,
#       input$export_vis,
#       alpha_werte %in% input$export_alfa,
#       input$export_vergl,
#       c(lang$ui(118), lang$ui(117), "Transmission") %in% input$export_alter
#     ),
#     
#     Funktion = c(
#       "plot_radio2",
#       "plot_allgemein2",
#       rep("plot_allgemein2", length(alpha_werte)),
#       "plot_vergleich_exp",
#       "plot_alter_ges2",
#       "plot_alter_pup2",
#       "plot_alter_trans2"
#       
#     ),
#     Args = list(
#       Plot_data$rad,
#       Plot_data$vis,
#       Plot_data[[alpha_werte[1]]],
#       Plot_data[[alpha_werte[2]]],
#       Plot_data[[alpha_werte[3]]],
#       Plot_data[[alpha_werte[4]]],
#       Plot_data[[alpha_werte[5]]],
#       Plot_data$vergleich,
#       Plot_data$alter_ges,
#       Plot_data$alter_pup,
#       Plot_data$alter_trans
#       
#     ),
#     width = c(rep(input$plot_width, 7),
#               input$plot_height,
#               input$plot_width,
#               input$plot_width,
#               input$plot_width
#     ),
#     Height = c(rep(input$plot_height, 11)
#     )
#     
#   )
# })
# 
# # output$test2 <- renderPrint({
# # 
# #   Alpha_downloads()
# #     # Table_data$alpha %>%  dplyr::select(!c(Zeichen | any_of(input$export_alpha))
# # 
# #   # %>% mutate(Größe = Größe %>% str_replace_all(pattern = "&|\\;", replacement = "")) %>% {rbind(col_names_export_long,.)}
# # 
# # 
# #   # Alpha_downloads()
# # 
# # })
# 
# # Stellt die Plothöhe wieder auf Standard zurück, wenn ohne Tabellen gedruckt wird.
# observe({
#   if(input$export_tab ==FALSE) {
#     Plot_data$tabelle$Height = input$plot_height
#   }
# }) %>% bindEvent(input$export_tab)
# 
# 
# 
# #Funktion für die Grafikerzeugung beim Download
# plot_download <- function(Name, Bedingung, Funktion, Argumente, width, height){
#   if(Bedingung){
#     filename <- paste(spec_name(), "_", Name, "_", Sys.Date(), ".pdf", sep="")
#     plotsave(file = filename, name = Funktion, args = Argumente, width=width, height = height)
#     Plot_data$files <- c(Plot_data$files, filename)
#   }
#   setProgress((length(Plot_data$files)+ length(Plot_data$tables))/Plot_data$n_export, detail = paste(length(Plot_data$files), lang$server(106), length(Plot_data$files), lang$server(107)))
#   
# }
# 
# #Funktion um die Zeilen eines tibble zu extrahieren
# extracto_plot <- function(Zeile){
#   list(
#     Plot_data$tabelle[[1]][Zeile],
#     Plot_data$tabelle[[2]][Zeile],
#     Plot_data$tabelle[[3]][Zeile],
#     Plot_data$tabelle[[4]][Zeile],
#     Plot_data$tabelle[[5]][Zeile],
#     Plot_data$tabelle[[6]][Zeile]
#   )
# }
# 
# #Funktion, um die Downloadknöpfe zu aktivieren/deaktivieren
# down_button_update <- function(name, icon, Bezeichnung, n = Plot_data$n_export) {
#   if(Plot_data$n_export == 0 | length(input$export_typ) == 0) {
#     shinyjs::disable(name)
#     shinyjs::html(name,
#                   sprintf(paste0("<i class='fa fa-circle-exclamation'></i>",
#                                  lang$server(108))
#                   ))
#   }
#   else {
#     shinyjs::enable(name)
#     shinyjs::html(name,
#                   sprintf(paste0("<i class='fa fa-", icon, "'></i> ", Bezeichnung),
#                           n
#                   )
#     )
#   }}
# 
# #Logik, um Downloadknöpfe zu aktivieren/deaktivieren
# observe({
#   Plot_data$n_export <-
#     sum(if (lang$ui(143) %in% input$export_typ)
#       (Plot_data$tabelle$plot_true),
#       if (lang$ui(144) %in% input$export_typ) {
#         Table_data$tabelle$plot_true[Table_data$tabelle$name %in% alpha_werte == FALSE]
#       },
#       lang$ui(145) %in% input$export_typ,
#       lang$ui(146) %in% input$export_typ)
#   
#   down_button_update("download_button", "download", strong(paste0(lang$server(109)," (n=%s)")))
#   
# })
# 
# #Funktion für die Erstellung einer Zusammenfassungstabelle alphaopisch
# table_alpha_sum <- function(Breite = 100, incl = "Zeichen", show = alpha_werte){
#   Table_data$alpha %>% gt()%>%
#     tab_header(title = strong(spec_name()),
#                subtitle = table_subtitle(lang$server(110))) %>%
#     fmt(columns = c("Zeichen", "Größe"), rows = 1, fns = HTML) %>%
#     fmt(columns = c("Zeichen", "Größe"), rows = 2, fns = HTML) %>%
#     fmt(columns = c("Zeichen", "Größe"), rows = 3, fns = HTML) %>%
#     fmt(columns = c("Zeichen", "Größe"), rows = 4, fns = HTML) %>%
#     Number_formatting_tables(Melanopsin) %>% 
#     Number_formatting_tables(Erythropsin) %>% 
#     Number_formatting_tables(Chloropsin) %>% 
#     Number_formatting_tables(Cyanopsin) %>% 
#     Number_formatting_tables(Rhodopsin) %>% 
#     opt_align_table_header(align = "left") %>%
#     tab_source_note(source_note = HTML(paste0(
#       lang$server(72),
#       lang$server(111),
#       lang$server(73), 
#       " <a href = 'https://cie.co.at/publications/cie-system-metrology-optical-radiation-iprgc-influenced-responses-light-0'>CIE S026 (2018)</a> ",
#       lang$server(74),
#       " <a href = 'https://www.beuth.de/de/vornorm/din-ts-5031-100/343737176'>DIN/TS 5031-100:2021-11</a>.")))%>%
#     tab_options(table.width = pct(Breite)) %>%
#     tab_style(style = cell_text(color = "#1D63DC"),
#               locations = cells_column_labels(columns = Melanopsin)) %>%
#     tab_style(style = cell_text(color = "darkred"),
#               locations = cells_column_labels(columns = Erythropsin)) %>%
#     tab_style(style = cell_text(color = "limegreen"),
#               locations = cells_column_labels(columns = Chloropsin)) %>%
#     tab_style(style = cell_text(color = "darkviolet"),
#               locations = cells_column_labels(columns = Cyanopsin)) %>%
#     tab_style(style = cell_text(color = "grey60"),
#               locations = cells_column_labels(columns = Rhodopsin)) %>%
#     cols_hide(!c(Größe, incl, show, Einheit)) %>%
#     cols_label(Zeichen = "",
#                Größe = "") %>%
#     tab_style(
#       style = cell_borders(
#         sides = c("right"),
#         color = "grey70",
#         weight = px(2),
#         style = "solid"
#       ),
#       locations = cells_body(
#         columns = 1,
#         rows = everything()
#       )) %>%
#     cols_label(Zeichen = "",
#                Größe = "") %>%
#     tab_style(
#       style = cell_borders(
#         sides = c("left"),
#         color = "grey70",
#         weight = px(2),
#         style = "solid"
#       ),
#       locations = cells_body(
#         columns = Einheit,
#         rows = everything()
#       ))
#   
# }
# 
# #Funktion für die Argumente der alphaopischen Bewertung
# Table_alpha <- function(name, adjektiv, Beschreibung, Breite){
#   Table_data$args[[name]] <- list(
#     name = name,
#     adjektiv = adjektiv,
#     Beschreibung = Beschreibung,
#     Breite = Breite
#   )
#   
# }
# 
# #Tabellenbreite für den export (in Prozent)
# Breite_export <- 100
# 
# #Erstellt die Argumente für die alphaopische Bewertung
# observe({
#   req(data())
#   pmap(list(alpha_werte, alpha_adjektive, Beschreibungen, Breite_export), Table_alpha)
# })
# 
# #Zusammenstellen der Daten um die Downloadtabellen zu erzeugen
# observe({
#   req(data())
#   Table_data$tabelle <- tibble(
#     name = c(
#       lang$server(39),
#       lang$server(63),
#       alpha_werte,
#       lang$server(126),
#       lang$server(125),
#       lang$server(127),
#       lang_server[128]
#     ),
#     
#     
#     plot_true = c(
#       input$export_rad,
#       input$export_vis,
#       alpha_werte %in% input$export_alfa,
#       sum(alpha_werte %in% input$export_alfa) != 0,
#       (c(lang$ui(118)) %in% input$export_alter) | sum(c(lang$ui(117), "Transmission") %in% input$export_alter) == 2,
#       (c(lang$ui(117)) %in% input$export_alter) & sum(c(lang$ui(118), "Transmission")  %in% input$export_alter) == 0,
#       (c("Transmission") %in% input$export_alter) & sum(c(lang$ui(118), lang$ui(117))  %in% input$export_alter) == 0
#     ),
#     
#     
#     Funktion = c(
#       "table_rad",
#       "table_vis",
#       rep("table_alpha", length(alpha_werte)),
#       "table_alpha_sum",
#       "table_age",
#       "table_pup",
#       "table_trans"
#     ),
#     
#     Args = list(
#       list(Breite = Breite_export),
#       list(Breite = Breite_export),
#       Table_data$args[[alpha_werte[1]]],
#       Table_data$args[[alpha_werte[2]]],
#       Table_data$args[[alpha_werte[3]]],
#       Table_data$args[[alpha_werte[4]]],
#       Table_data$args[[alpha_werte[5]]],
#       list(Breite = Breite_export, show =input$export_alfa),
#       list(Breite = Breite_export),
#       list(Breite = Breite_export),
#       list(Breite = Breite_export)
#     ),
#     
#     Height = c(
#       rep(0, 11)),
#     
#     Table_creation = c(
#       input$export_rad,
#       input$export_vis,
#       alpha_werte %in% input$export_alfa,
#       sum(alpha_werte %in% input$export_alfa) != 0,
#       (c(lang$ui(118)) %in% input$export_alter) | sum(c(lang$ui(117), "Transmission") %in% input$export_alter) == 2,
#       (c(lang$ui(117)) %in% input$export_alter),
#       (c("Transmission") %in% input$export_alter)
#     )
#   )
# })
# 
# 
# #Funktion für das Speichern von Tabellen
# tablesave <- function(file, name, args) {
#   gtsave(data = do.call(name, args = args), filename = file, vwidth = ceiling(input$plot_width*133))
# }
# 
# Bilder_Tabellen <- reactiveValues()
# 
# #Funktion für die Tabellenerzeugung beim Download
# tabelle_download <- function(Name, Bedingung, Funktion, Argumente, Bedingung2){
#   if(Bedingung){
#     filename <- paste(spec_name(), "_", Name, "_", Sys.Date(), ".png", sep="")
#     tablesave(file = filename, name = Funktion, args = Argumente)
#     Bilder_Tabellen[[Name]] <- png::readPNG(filename, native = TRUE)
#     if(input$export_tab) {
#       Plot_data$tabelle$Height[Plot_data$tabelle$name ==Name] <- input$plot_height*0.9+(dim(Bilder_Tabellen[[Name]])[1]+40)/240
#     }
#     if(Name %in% alpha_werte == FALSE & Bedingung2){
#       Plot_data$tables <- c(Plot_data$tables, filename)}
#     incProgress(1/sum(Table_data$tabelle[[6]]), detail = paste(lang$server(112), length(Plot_data$tables), lang$server(113)))
#   }
# }
# 
# #Funktion um die Zeilen eines tibble zu extrahieren
# extracto_table <- function(Zeile){
#   list(
#     Table_data$tabelle[[1]][Zeile],
#     Table_data$tabelle[[6]][Zeile],
#     Table_data$tabelle[[3]][Zeile],
#     Table_data$tabelle[[4]][Zeile],
#     Table_data$tabelle[[2]][Zeile]
#   )
# }
# 
# excel_save <- function(wb, name, Bedingung, data){
#   if(Bedingung){
#     data2 <- data
#     names(data2) <- 
#       addWorksheet(wb, name)
#     writeData(wb, sheet = name, data2)
#   }}
# 
# renaming <- function(data = Plot_data$files) {
#   str_replace(data, pattern = spec_name(), paste0(spec_name(), "_", 01:length(data)))
# }
# 
# Alpha_downloads <- reactive({
#   
#   col_names_export_long <- c(lang$server(120), lang$server(123), input$export_alfa, lang$server(124))
#   
#   Table_data$alpha %>%  dplyr::select(!any_of(c("Zeichen", setdiff(alpha_werte, input$export_alfa)))) %>% mutate(Größe = Größe %>% str_replace_all(pattern = "&|\\;", replacement = "")) %>% {rbind(col_names_export_long,.)}
#   
# })
# 
# #Download-Knopf
# output$download_button <- downloadHandler(
#   filename = function() {
#     paste(spec_name(), "_", Sys.Date(), ".zip", sep="")},
#   content = function(file) {
#     obs$suspend()
#     shinyalert("OK", text = paste0(lang$server(114)), type = "success", timer = 5000, closeOnClickOutside = TRUE)
#     
#     withProgress(message = lang$server(115), value = 0, {
#       owd <- setwd(tempdir())
#       on.exit(setwd(owd))
#       Plot_data$files <- NULL;
#       wb <- NULL
#       if(input$export_tab | lang$ui(144) %in% input$export_typ) {
#         withProgress(message = lang$server(116), value = 0, {
#           
#           pmap(
#             extracto_table(1:dim(Table_data$tabelle)[1]),
#             tabelle_download
#           )
#         })
#       }
#       
#       setProgress(length(Plot_data$tables)/Plot_data$n_export, detail = paste(lang$server(117), length(Plot_data$tables)))
#       if(lang$ui(143) %in% input$export_typ) {
#         pmap(
#           extracto_plot(1:dim(Plot_data$tabelle)[1]),
#           plot_download
#         )}
#       setProgress( (length(Plot_data$files) + length(Plot_data$tables))/Plot_data$n_export, detail = paste(lang$server(118), length(Plot_data$files)))
#       
#       if(lang$ui(144) %in% input$export_typ) {
#         Plot_data$files <- c(Plot_data$files, Plot_data$tables)
#       }
#       
#       if(lang$ui(145) %in% input$export_typ) {
#         filename <- paste(spec_name(), "_", Sys.Date(), ".xlsx", sep="")
#         
#         wb <- createWorkbook(spec_name())
#         
#         col_names_export <- c(lang$server(120), lang$server(122),lang$server(123), lang$server(124))
#         
#         for(i in c(1:2, 8:11)){
#           excel_save(wb, name = Table_data$tabelle$name[i], Bedingung = Table_data$tabelle$plot_true[i], 
#                      data = list(
#                        Table_data$rad %>% select(!Zeichen)%>% {rbind(col_names_export,.)},
#                        Table_data$vis %>% select(!Zeichen)%>% {rbind(col_names_export,.)},
#                        Table_data[[alpha_werte[1]]] %>% select(!Zeichen)%>% {rbind(col_names_export,.)},
#                        Table_data[[alpha_werte[2]]] %>% select(!Zeichen)%>% {rbind(col_names_export,.)},
#                        Table_data[[alpha_werte[3]]] %>% select(!Zeichen)%>% {rbind(col_names_export,.)},
#                        Table_data[[alpha_werte[4]]] %>% select(!Zeichen)%>% {rbind(col_names_export,.)},
#                        Table_data[[alpha_werte[5]]] %>% select(!Zeichen)%>% {rbind(col_names_export,.)},
#                        Alpha_downloads(),
#                        tau_alter$Tabelle %>% select(!Zeichen)%>% {rbind(col_names_export,.)},
#                        tau_alter$Tabelle %>% slice(2) %>% select(!Zeichen)%>% {rbind(col_names_export,.)},
#                        tau_alter$Tabelle%>% slice(3) %>% select(!Zeichen)%>% {rbind(col_names_export,.)}
#                      )[i])
#         }
#         saveWorkbook(wb, filename, overwrite = TRUE)
#         Plot_data$files <- c(Plot_data$files, filename)
#       }
#       setProgress(value = 1, detail = paste(lang$server(119), length(Plot_data$files)))
#       
#       if(lang$ui(146) %in% input$export_typ) {
#         filename <- paste(spec_name(), "_", Sys.Date(), ".csv", sep="")
#         temp_data <- spectrum$data
#         names(temp_data) <- c(lang$server(31), lang$server(32))
#         write_csv(temp_data, filename)
#         Plot_data$files <- c(Plot_data$files, filename)
#       }
#       
#       file.rename(from = Plot_data$files, to = renaming(Plot_data$files))
#       
#       
#       zip(file, renaming(Plot_data$files))
#     })
#     
#   }
# )

