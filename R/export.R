
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
          export_downloadUI(ns("download"))
          )
        )
    )
    }

# Server ------------------------------------------------------------------

exportServer <- 
  function(
    id, 
    Analysis,
    Spectrum,
    Tabactive
  ) {
  
  shiny::moduleServer(id, function(input, output, session) {
    
    #what plots should be exported
    outputs <- export_define_OutputServer("Outputs", Tabactive)
    
    #manage general settings
    export_general_settingsServer("Analysis_Settings", Analysis, Spectrum,
                                  Tabactive)
    
    #create the output files
    Export <- export_Output_SettingsServer("Export_Settings", Analysis, outputs,
                                           Tabactive)
    
    #Handle the download
    export_downloadServer("download", Analysis, Export, Tabactive, outputs)

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
                                   UI_Header(lang_link = FALSE),
                                   #Sidebar
                                   shinydashboard::dashboardSidebar(
                                     width = 190,
                                     shinydashboard::sidebarMenu(
                                       id = "inTabset",
                                       htmltools::br(),
                                       htmltools::p(
                                         htmltools::img(
                                           width = "135px", 
                                           src = "extr/Logo.png"),
                                         align = "center"),
                                       # htmltools::br(),
                                       shinydashboard::menuItem(
                                         lang$ui(1),
                                         tabName = "tutorial",
                                         icon = shiny::icon("circle-info")),
                                       htmltools::br(),
                                       shinydashboard::menuItem(
                                         "Import",
                                         tabName = "import",
                                         icon = shiny::icon("file-import")
                                       ),
                                       shinydashboard::menuItem(
                                         lang$ui(23),
                                         tabName = "analysis",
                                         icon = shiny::icon(
                                           "magnifying-glass-chart")
                                       ),
                                       shinydashboard::menuItem(
                                         "Export",
                                         tabName = "export",
                                         icon = shiny::icon("file-export"),
                                         selected = TRUE
                                       ), 
                                       htmltools::br(),
                                       shinydashboard::menuItem(
                                         lang$ui(153),
                                         tabName = "validity",
                                         icon = shiny::icon("file-circle-check")
                                       ),
                                       shinydashboard::menuItem(
                                         lang$ui(158),
                                         tabName = "impressum",
                                         icon = shiny::icon(
                                           "user", lib = "glyphicon")
                                       )
                                     )
                                   ),
                                   #Body
                                   shinydashboard::dashboardBody(
                                     #Add a link to the css resource
                                     htmltools::tags$link(
                                       rel = "stylesheet", type="text/css", 
                                       href="extr/style.css"),
                                     shiny::verbatimTextOutput("Data_ok"),
                                     gt::gt_output("alpha_table"),
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
                                     ),
                                     shiny::fluidPage(
                                       htmltools::p(shiny::plotOutput(
                                         "Plotbreite", height = "1px")))
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
                               Spectrum = Spectrum,
                               Tabactive = shiny::reactive(input$inTabset))
    
    #Export
    Export <- exportServer("export", Analysis, Spectrum,
                           Tabactive = shiny::reactive(input$inTabset))
    
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
    
    #Update the Navbar when hitting the Export-Button in Analysis
    shiny::observe({
        shiny::updateNavbarPage(
          session,
          inputId = "inTabset",
          selected = "export")
    }) %>% shiny::bindEvent(Analysis$to_export)

    
    output$Plotbreite <- shiny::renderPlot({
    }, bg = "white")
    
    
    output$Data_ok <- shiny::renderPrint({
      #     print("Developer Troubleshoot\n")
      # p990rint(list.files(path = "extr/"))
      # print(Export$CSV)
      # print(Analysis$table_Radiometrie$internal)
      # print(Export$Download_n)
      # print(Export$Download_tables)
      # print(Export$Table)
      # print(Export$Xlsx)
      print(Export$Plot)
      # print(Spectrum$Origin)
      #     print(Spectrum$Spectrum %>% utils::head())
      #     print(Spectrum$Spectrum %>% utils::tail())
    })
    
  }
  shiny::shinyApp(ui, server, ...)
}
