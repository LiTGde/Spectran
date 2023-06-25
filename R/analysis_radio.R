
# UI ----------------------------------------------------------------------

analysis_radioUI <- function(
    id, lang_setting = get("lang_setting", envir = caller_env(n = 1))) {
  tagList(
    # Radiometrie_UI <- p(
    #   checkboxGroupInput(
    #     "sensitivitaeten",
    #     label = lang_ui[95],
    #     choiceNames = c(
    #       alpha_werte,
    #       paste0(lang_ui[89], " V(\\(\\lambda\\))")
    #     ),
    #     choiceValues = c(
    #       "Melanopsin",
    #       "Erythropsin",
    #       "Chloropsin",
    #       "Cyanopsin",
    #       "Rhodopsin",
    #       "V(lambda)"
    #     ),
    #     inline = TRUE
    #   ),
    #   #Übersichtsplot des Spektrums und Tabelle
    #   plotOutput("Uebersichtsplot_A", height = "350px"),
    #   gt::gt_output("Spektraldaten_radio")
    # )
  )
}

# Server ------------------------------------------------------------------

analysis_radioServer <- 
  function(id, 
           lang_setting = get("lang_setting", envir = caller_env(n = 1)),
           Spectrum = NULL
           ) {
  
  moduleServer(id, function(input, output, session) {
    
    #Set up a container for the spectra to go into, if it isn´t already defined
    if (is.null(Spectrum)){
      Spectrum <- 
        reactiveValues(Spectrum_raw = NULL, Name = NULL, Destination = NULL)
    }
    
#     #Plottheme
#     theme_set(theme_cowplot(font_size = 15, font_family = "sans"))
# 
#     #Maxima
#     max_E <- reactive(max(data()$Bestrahlungsstaerke*1000)) %>% bindEvent(data())
# 
#     #Erstellung einer Hülle, welche das Spektrum aufnimmt.
#     spectrum <- reactiveValues()
#     
#     #Füllt die Hülle mit dem Datensatz des Spektrums (data())
#     observe({
#       spectrum$max_E <- max_E()
#       spectrum$data <- data()
#     }) %>% bindEvent(data())
#     
#     
#     #Plotsetup
#     Plot_01 <- function(size_font = 15) {
#       ggplot(data = spectrum$data, aes(x = Wellenlaenge, y= Bestrahlungsstaerke*1000))+
#         labs(x = lang_server[100], title = spec_name())+
#         ylab(bquote(.(lang_server[40])~~mW/(m^{2}*'*'*nm)))+
#         scale_fill_gradientn(colors = regenbogen, guide = "none")+
#         scale_color_gradientn(colors = alpha(regenbogen, alpha = 0.03), guide = "none")+
#         scale_x_continuous(breaks = c(400, 500, 600, 700, 780))+
#         scale_y_continuous(expand = expansion(mult = c(0, .1)))+
#         coord_cartesian(ylim = c(0, spectrum$max_E*1.10))+
#         theme_cowplot(font_size = size_font, font_family = "sans")+
#         theme(legend.position = "none")
#     }
# 
#     #Spektraldaten
#     Spectral_data <- reactive({
#       req(data())
#       colorSpec(data = data()$Bestrahlungsstaerke, wavelength = data()$Wellenlaenge, specnames = spec_name())
#     })
# 
#     #Funktion für den Übersichtsplot
#     plot_radio <- function(size_font = 15) {
#       Farben <- Bewertung() %>% filter((Bezeichnung %in% input$sensitivitaeten)) %>% arrange(Kürzel)
#       Plot_01(size_font) +
#         geom_ridgeline_gradient(aes(y = 0, height =Bestrahlungsstaerke*1000, fill = Wellenlaenge))+
#         geom_path(lwd = 1.2)+
#         labs(subtitle = lang_server[39])+
#         scale_color_manual(values = colors_Wirkspektren)+
#         {if(length(input$sensitivitaeten) != 0){
#           geom_path(data = Rezeptorkurven2 %>% filter(Typ %in% input$sensitivitaeten),
#                     aes(x=Wellenlaenge, y= rel_Empfindlichkeit*max_E(), col = Typ), lwd = 0.75)}}+
#         {if(length(input$sensitivitaeten) != 0){
#           geom_label_repel(data = Bewertung() %>% filter(Bezeichnung %in% Farben$Bezeichnung),
#                            aes(x=Peak, y= max_E(), label = Bezeichnung, col = Bezeichnung),
#                            min.segment.length = 0,ylim = c(max_E(), max_E()*1.17), alpha = 0.7, parse = TRUE, size = 4/15*size_font)}}
#     }
# 
#     #Container für die Breite der Ausgabeplots
#     Plotbreite_temp <- reactiveVal(value = 500)
# 
#     obs <- observe({
#       Plotbreite_temp(session$clientData$output_Plotbreite_width)
#     }) %>% bindEvent(session$clientData$output_Plotbreite_width)
#     # 
#     
#     #Übersichtsplot der Daten
#     output$Uebersichtsplot_A <- renderPlot({
#       req(data(),Plotbreite_temp())
#       plot_radio()
#       } ,height = 350, width = reactive(Plotbreite_temp()))
#     outputOptions(output, "Uebersichtsplot_A", suspendWhenHidden = FALSE)
# 
# 
#     #Photonendichteformel
#     h <- 6.626*10^-34
#     c <- 2.998*10^8
# 
#       #Berechnet die Photonendichte in quanta/(cm^2*s) aus W/m^2
#     PD <- function(wavelength, irradiance) {
#       (irradiance*wavelength*10^-9)/(h*c)/100^2
#     }
# 
#     #Berechnet die Wellenlänge mit maximaler Energie
#     LambdaMax <- reactive({
#       wavelength(Spectral_data())[Spectral_data() == max(Spectral_data())] %>% mean()
#     }) %>% bindEvent(Spectral_data())
# 
#     #Erzeugt eine Tabelle mit der Zusammenfassung aus der Radiometrie
#     Table_data <-  reactiveValues()
# 
#     observe({
#       req(data())
#     Table_data$rad <- tribble(
#       ~Größe, ~Zeichen, ~Formelzeichen, ~Wert, ~Einheit,
#       lang_server[40], "E<sub>e</sub>", "E_e", as.numeric(sum(Spectral_data()*1000, 0)), "mW/m²",
#       lang_server[41], "&lambda;<sub>Emax</sub>", "lambda_Emax", as.numeric(LambdaMax()), "nm",
#       lang_server[42], "N<sub>P</sub>", "N_P", as.numeric(sum(PD(wavelength(Spectral_data()), as.vector(Spectral_data())))), "quanta/(cm²*s)"
#     )
#     })
# 
# Number_formatting_tables <- function(data, column = Wert) {
#   data %>% 
#     fmt_number(columns = ({{ column }}), rows = {{ column }} >= 100, decimals = 0, sep_mark = ".", dec_mark = ",") %>%
#     fmt_number(columns = ({{ column }}), rows = {{ column }} >= 10, decimals = 1, sep_mark = ".", dec_mark = ",") %>%
#     fmt_number(columns = ({{ column }}), rows = {{ column }} >= 1, decimals = 2, sep_mark = ".", dec_mark = ",") %>%
#     fmt_number(columns = ({{ column }}), n_sigfig = 3, sep_mark = ".", dec_mark = ",")
# }
# 
# #Funktion für den Untertitel von Tabellen
# table_subtitle <- function(Typ) {
#   beginning <- "<b style='float:left'>"
#   ending <- paste0("</b><span style='float:right'> ", lang_server[43] ," <b>Spectran</b> (Zauner, 2023)</span>")
#   title <- paste0(beginning, Typ, ending)
#   html(title)
# }
#     #Erzeugt eine Tabelle für die Radiometrie
#     table_rad <- function(Breite = 100){
#       Table_data$rad %>% select(!"Formelzeichen") %>%
#       gt(rowname_col = "Größe") %>%
#         opt_align_table_header(align = "left") %>%
#         Number_formatting_tables() %>% 
#         fmt(columns = c("Zeichen"), rows = 1, fns = HTML) %>%
#         fmt(columns = c("Zeichen"), rows = 2, fns = HTML) %>%
#         fmt(columns = c("Zeichen"), rows = 3, fns = HTML) %>%
#         fmt_scientific(columns = "Wert",rows=3, sep_mark = ".", dec_mark = ",") %>%
#         tab_footnote(
#           footnote = lang_server[44],
#           locations = cells_stub(rows = lang_server[40])
#         ) %>%
#         tab_footnote(
#           footnote = lang_server[45],
#           locations = cells_stub(rows = lang_server[41])
#         ) %>%
#         tab_footnote(
#           footnote = lang_server[46],
#           locations = cells_stub(rows = lang_server[42])
#         ) %>%
#         tab_source_note(
#           source_note = lang_server[47]
#         ) %>%
#         tab_options(column_labels.hidden = TRUE, table.width = pct(Breite)) %>% 
#         tab_header(title = strong(spec_name()),
#                    subtitle = table_subtitle(lang_server[39]))
#     }
# 
#     # Tabelle der radiometrischen Ergebnisse
#     output$Spektraldaten_radio <- render_gt({
#       req(data())
#       table_rad() %>% tab_header(title = strong(spec_name()),
#                                          subtitle = strong(lang_server[39]))
#       #
#     }, height = 300)
#     outputOptions(output, "Spektraldaten_radio", suspendWhenHidden = FALSE)

    
    
  })
}

# App ---------------------------------------------------------------------

analysis_radioApp <- function(lang_setting = "Deutsch") {
  
  ui <- fluidPage(
    analysis_radioUI("import"),
      verbatimTextOutput("Data_ok")
      )
  server <- function(input, output, session) {

    analysis_radioServer("import")
    output$Data_ok <- renderPrint({
      {
        print(Spectrum$Name)
        print(Spectrum$Destination)
        Spectrum$Spectrum_raw %>% head()
      }
    })
    }
  shinyApp(ui, server)
}
