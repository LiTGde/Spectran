
# UI ----------------------------------------------------------------------

analysisUI <- 
  function(
    id, 
    lang_setting = get("lang_setting", envir = caller_env(n = 1))
    ) {
    ns <- shiny::NS(id)
    htmltools::tagList(
                       analysis_radioUI(ns("radio"))
                       )
  }

# Server ------------------------------------------------------------------

analysisServer <- 
  function(
    id, 
    lang_setting = get("lang_setting", envir = rlang::caller_env(n = 1)),
    Spectrum = NULL
    ) {
  
  shiny::moduleServer(id, function(input, output, session) {

    Table <- analysis_setupServer("setup",
                         lang_setting = lang_setting,
                         Spectrum = Spectrum)
    
    analysis_radioServer("radio", 
                         lang_setting = lang_setting,
                         Spectrum = Spectrum)
    
  })
}

# App ---------------------------------------------------------------------

analysisApp <- function(lang_setting = "Deutsch") {
  
  ui <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(),
    shinydashboard::dashboardSidebar(),
    shinydashboard::dashboardBody(
      shiny::verbatimTextOutput("Data_ok"),
      analysisUI("analysis")
      )
    )
  server <- function(input, output, session) {

    test.spectrum <-  
      examplespectra$Measurement %>% 
      dplyr::select(Wellenlaenge, led) %>% 
      dplyr::rename(Bestrahlungsstaerke = led)
    
    Spectrum <- 
      shiny::reactiveValues(Spectrum = test.spectrum, 
                     Name = "Test", 
                     Destination = lang$ui(69))
    
    analysisServer("analysis",
                   lang_setting = lang_setting,
                   Spectrum = Spectrum)
    
    output$Data_ok <- shiny::renderPrint({
      {
        print(Spectrum$maxE)
        print(Spectrum$Name)
        print(Spectrum$Destination)
        print(Spectrum$radiometric)
        }
    })
    }
  shiny::shinyApp(ui, server)
}
