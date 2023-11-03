
# UI ----------------------------------------------------------------------

analysisUI <- 
  function(
    id
    ) {
    ns <- shiny::NS(id)
    htmltools::tagList(
      # shiny::withMathJax(),
      #heading
      htmltools::h3(lang$ui(23)),
      #tabs with ui
      shiny::tabsetPanel(
        shiny::tabPanel(id = "radiometrie",
                 title = lang$ui(119),
                 analysis_radioUI(ns("radio"))),
        shiny::tabPanel(id = "photometrie",
                 title = lang$ui(120),
                 analysis_photoUI(ns("photo"))),
        shiny::tabPanel(id = "alphaopie",
                 title = paste0(Specs$Alpha.ico, lang$ui(121)),
                 analysis_alphaUI(ns("alpha"))),
        shiny::tabPanel(id = "alter",
                 title = lang$ui(122),
                 analysis_ageUI(ns("age"))),
        selected = lang$ui(119)
      ),
      #continue to export
      shiny::fluidRow(
        htmltools::br(),
        shiny::actionButton(
          ns("zu_Export"),
          label = lang$ui(123),
          class = "btn-lg",
          shiny::icon("play", lib = "glyphicon")
        ),
        width = 12,
        align = "center"
      )
                       )
  }

# Server ------------------------------------------------------------------

analysisServer <- 
  function(
    id, 
    Spectrum = NULL,
    Tabactive = NULL
    ) {
  
  shiny::moduleServer(id, function(input, output, session) {

    Analysis <- shiny::reactiveValues()
    
    analysis_setupServer("setup",
                         Spectrum = Spectrum,
                         Analysis = Analysis)
    
    analysis_radioServer("radio",
                         Analysis = Analysis,
                         feed = lang$server(39))

    analysis_photoServer("photo",
                         Analysis = Analysis,
                         feed = lang$server(63),
                         Name = Specs$Plot$Names[[6]],
                         Tabactive = Tabactive)

    analysis_alphaServer("alpha",
                         Analysis = Analysis,
                         Tabactive = Tabactive)

    analysis_ageServer("age",
                         Analysis = Analysis,
                       Tabactive = Tabactive)
    
    #exporting the to_Export button
    shiny::observe({
      Analysis$to_export <- input$zu_Export
    })
    
    #Return value
    Analysis
  })
}

# App ---------------------------------------------------------------------

analysisApp <- function(lang_setting = "Deutsch") {
  
  #set the language for the program
  the$language <- lang_setting
  
  ui <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(),
    shinydashboard::dashboardSidebar(),
    shinydashboard::dashboardBody(
      analysisUI("analysis"),
      shiny::verbatimTextOutput("Data_ok")
      )
    )
  server <- function(input, output, session) {
    
    test.spectrum <-  
      examplespectra$Measurement %>% 
      dplyr::select(Wellenlaenge, equ) %>% 
      dplyr::rename(Bestrahlungsstaerke = 2)
    
    Spectrum <- 
      shiny::reactiveValues(Spectrum = test.spectrum, 
                     Name = "Test", 
                     Origin = "Import",
                     Destination = lang$ui(69))
    
    Analysis <-
      analysisServer("analysis",
                   Spectrum = Spectrum,
                   Tabactive = shiny::reactive("analysis"))
    
    output$Data_ok <- shiny::renderPrint({
      {
        # print(Spectrum$maxE)
        # print(Spectrum$Name)
        # print(Spectrum$Destination)
        # print(Spectrum$photometric$table)
        # print(Spectrum$general)
        # print(Analysis$Plot)
        # print(Analysis$Settings)
        # print(Analysis$table_Age$internal)
        print(Analysis[[ns_plot("Summary")]]$args)
        # print(Spectrum$CRI)
        # print(Spectrum$cS)
        # print(Spectrum$melanopic$plot)
        # Spectrum$photometric$plot
        }
    })
    }
  shiny::shinyApp(ui, server)
}
