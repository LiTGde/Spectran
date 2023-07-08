
# UI ----------------------------------------------------------------------

exportUI <- 
  function(
    id
    ) {
    ns <- shiny::NS(id)
    htmltools::tagList(
      #header, then box that contains everything
        shiny::h3("Export"),
        shiny::fluidRow(
        shinydashboard::box(
          width = 12,
          title = lang$ui(124),
          #Introduction
          htmltools::p(
                    lang$ui(125),
                    htmltools::em(.noWS = "outside", lang$ui(23)),
                    lang$ui(126),
                    htmltools::em(.noWS = "outside", lang$ui(127)),
                    lang$ui(128)
                  ),
                  shiny::column(
                    width = 6,
                    #Define the outputs
                    export_define_OutputUI(ns("Outputs")),
                    #General Settings
                    export_general_settingsUI1(ns("Analysis_Settings")),
                    htmltools::br()
                  ),
          shiny::column(
                    width = 6,
                    offset = 0,
                    export_general_settingsUI2(ns("Analysis_Settings")),
                    #Output Settings
                    export_Output_SettingsUI(ns("Export_Settings"))
                  ),
          #Download-Button
                  shinydashboard::box( class = "background-rect",
                    width = 12,
                    align = "center",
                    shiny::downloadButton(ns("download_button"), 
                                          label = 
                                            htmltools::strong("Downloads"),
                                          class = "btn-lg"
                                          )
                  )
          )
        )
    )
    }

# Server ------------------------------------------------------------------

exportServer <- 
  function(
    id, 
    Analysis,
    Spectrum
  ) {
  
  shiny::moduleServer(id, function(input, output, session) {
    
    #what plots should be exported
    outputs <- export_define_OutputServer("Outputs")
    
    #manage general settings
    export_general_settingsServer("Analysis_Settings", Analysis, Spectrum)
    
    #create the output files
    Export <- export_Output_SettingsServer("Export_Settings", Analysis, outputs)
    
    #Download-Button
    output$download_button <- downloadHandler(
      #Filename
      filename = function() {
        paste(
          Analysis$Settings$Spectrum_Name, "_", Sys.Date(), ".pdf", sep="")},
      #Content
      content = function(file) {
        
        Plot <- do.call("plot_exp", Export$Plot)
        ggplot2::ggsave(file, plot = Plot)
        
        #Shinyalert with preparation message
        # shinyalert::shinyalert(
        #   "OK", 
        #   text = paste0(lang$server(114)), 
        #   type = "success", 
        #   timer = 5000, 
        #   closeOnClickOutside = TRUE,
        #   showConfirmButton = FALSE
        #   )
        
        # 
        # withProgress(message = lang$server(115), value = 0, {
        #   owd <- setwd(tempdir())
        #   on.exit(setwd(owd))
        #   Plot_data$files <- NULL;
        #   wb <- NULL
        #   if(input$export_tab | lang$ui(144) %in% input$export_typ) {
        #     withProgress(message = lang$server(116), value = 0, {
        # 
        #       pmap(
        #         extracto_table(1:dim(Table_data$tabelle)[1]),
        #         tabelle_download
        #       )
        #     })
        #   }
        # 
        #   setProgress(length(Plot_data$tables)/Plot_data$n_export, detail = paste(lang$server(117), length(Plot_data$tables)))
        #   if(lang$ui(143) %in% input$export_typ) {
        #     pmap(
        #       extracto_plot(1:dim(Plot_data$tabelle)[1]),
        #       plot_download
        #     )}
        #   setProgress( (length(Plot_data$files) + length(Plot_data$tables))/Plot_data$n_export, detail = paste(lang$server(118), length(Plot_data$files)))
        # 
        #   if(lang$ui(144) %in% input$export_typ) {
        #     Plot_data$files <- c(Plot_data$files, Plot_data$tables)
        #   }
        # 
        #   if(lang$ui(145) %in% input$export_typ) {
        #     filename <- paste(spec_name(), "_", Sys.Date(), ".xlsx", sep="")
        # 
        #     wb <- createWorkbook(spec_name())
        # 
        #     col_names_export <- c(lang$server(120), lang$server(122),lang$server(123), lang$server(124))
        # 
        #     for(i in c(1:2, 8:11)){
        #       excel_save(wb, name = Table_data$tabelle$name[i], Bedingung = Table_data$tabelle$plot_true[i],
        #                  data = list(
        #                    Table_data$rad %>% select(!Zeichen)%>% {rbind(col_names_export,.)},
        #                    Table_data$vis %>% select(!Zeichen)%>% {rbind(col_names_export,.)},
        #                    Table_data[[alpha_werte[1]]] %>% select(!Zeichen)%>% {rbind(col_names_export,.)},
        #                    Table_data[[alpha_werte[2]]] %>% select(!Zeichen)%>% {rbind(col_names_export,.)},
        #                    Table_data[[alpha_werte[3]]] %>% select(!Zeichen)%>% {rbind(col_names_export,.)},
        #                    Table_data[[alpha_werte[4]]] %>% select(!Zeichen)%>% {rbind(col_names_export,.)},
        #                    Table_data[[alpha_werte[5]]] %>% select(!Zeichen)%>% {rbind(col_names_export,.)},
        #                    Alpha_downloads(),
        #                    tau_alter$Tabelle %>% select(!Zeichen)%>% {rbind(col_names_export,.)},
        #                    tau_alter$Tabelle %>% slice(2) %>% select(!Zeichen)%>% {rbind(col_names_export,.)},
        #                    tau_alter$Tabelle%>% slice(3) %>% select(!Zeichen)%>% {rbind(col_names_export,.)}
        #                  )[i])
        #     }
        #     saveWorkbook(wb, filename, overwrite = TRUE)
        #     Plot_data$files <- c(Plot_data$files, filename)
        #   }
        #   setProgress(value = 1, detail = paste(lang$server(119), length(Plot_data$files)))
        # 
        #   if(lang$ui(146) %in% input$export_typ) {
        #     filename <- paste(spec_name(), "_", Sys.Date(), ".csv", sep="")
        #     temp_data <- spectrum$data
        #     names(temp_data) <- c(lang$server(31), lang$server(32))
        #     write_csv(temp_data, filename)
        #     Plot_data$files <- c(Plot_data$files, filename)
        #   }
        # 
        #   file.rename(from = Plot_data$files, to = renaming(Plot_data$files))
        # 
        # 
          # utils::zip(file, renaming(Plot_data$files))
        # })

      }
    )
    
    #Return Value
    Export
    
  })
}

# App ---------------------------------------------------------------------

exportApp <- function(lang_setting = "Deutsch", ...) {

    #add a resource path to the www folder
  shiny::addResourcePath(
    "extr", system.file("app/www", package = "Spectran"))
  # on.exit(shiny::removeResourcePath("extr"), add = TRUE)
  
  #set the language for the program
  the$language <- lang_setting
  
  #UI
  ui <-
    shinydashboard::dashboardPage( skin = "yellow",
                                   #Header
                                   UI_Header(),
                                   #Sidebar
                                   UI_Sidebar(),
                                   #Body
                                   shinydashboard::dashboardBody(
                                     #Add a link to the css resource
                                     htmltools::tags$link(
                                       rel = "stylesheet", type="text/css", 
                                       href="extr/style.css"),
                                     shiny::verbatimTextOutput("Data_ok"),
                                     shinydashboard::tabItems(
                                       #add a tab for the introduction
                                       shinydashboard::tabItem(
                                         tabName = "tutorial",
                                         introductionUI("intro")),
                                       #add a tab for the import
                                       shinydashboard::tabItem(
                                         tabName = "import",
                                         importUI("import")),
                                       #add a tab for the analysis
                                       shinydashboard::tabItem(
                                         tabName = "analysis",
                                         analysisUI("analysis")),
                                       #add a tab for the export
                                       shinydashboard::tabItem(
                                         tabName = "export",
                                         exportUI("export")),       
                                       #add a tab for the validity
                                       shinydashboard::tabItem(
                                         tabName = "validity",
                                         validityUI("validity")),
                                       #add a tab for the impressum
                                       shinydashboard::tabItem(
                                         tabName = "impressum",
                                         impressumUI("impressum"))
                                     )
                                   )
    )
  
  #Server
  server <- function(input, output, session) {
    
    #Introduction
    zu_Import <- introductionServer("intro")
    
    #Import
    test.spectrum <-  
      examplespectra$Measurement %>% 
      dplyr::select(Wellenlaenge, equ) %>% 
      dplyr::rename(Bestrahlungsstaerke = 2)
    
    Spectrum <- 
      shiny::reactiveValues(Spectrum = test.spectrum, 
                            Name = "Test", 
                            Origin = "Import",
                            Destination = lang$ui(69))
    
    # Spectrum <- importServer("import")
    
    #Analysis
    Analysis <- analysisServer("analysis",
                               Spectrum = Spectrum)
    
    #Export
    Export <- exportServer("export", Analysis, Spectrum)
    
    # Delete Notifications between tab changes
    notification_remover(shiny::reactive(input$inTabset))
    
    #Update the Navbar, when the Introduction is finished
    shiny::observe({
      shiny::updateNavbarPage(
        session, inputId = "inTabset" ,  selected = "import")
    }) %>% shiny::bindEvent(
      zu_Import(), ignoreInit = TRUE
    )
    
    #Update the Navbar when a Spectrum is imported
    shiny::observe({
      shiny::req(Spectrum$Spectrum, Spectrum$Destination)
      if(Spectrum$Destination == lang$ui(69) &
         input$inTabset == "import") {
        shiny::updateNavbarPage(
          session,
          inputId = "inTabset",
          selected = "analysis")
      }
    }) %>% shiny::bindEvent(Spectrum$Spectrum, Spectrum$Destination,
                            Spectrum$Name)
    
    output$Data_ok <- shiny::renderPrint({
      #     print("Developer Troubleshoot\n")
      # p990rint(list.files(path = "extr/"))
      print(Export$Plot)
      #     print(Spectrum$Other)
      #     print(Spectrum$Spectrum %>% utils::head())
      #     print(Spectrum$Spectrum %>% utils::tail())
    })
    
  }
  shiny::shinyApp(ui, server, ...)
}
