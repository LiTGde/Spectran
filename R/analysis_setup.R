
# UI ----------------------------------------------------------------------

analysis_setupUI <- 
  function(
    id, 
    lang_setting = get("lang_setting", envir = caller_env(n = 1))
    ) {
    ns <- shiny::NS(id)
    htmltools::tagList()
  }

# Server ------------------------------------------------------------------

analysis_setupServer <- 
  function(
    id, 
    lang_setting = get("lang_setting", envir = caller_env(n = 1)),
    Spectrum = NULL
    ) {
  
  shiny::moduleServer(id, function(input, output, session) {
    #Set up a container for the spectra to go into, if it isn´t already defined
    if (is.null(Spectrum)){
      Spectrum <- 
        shiny::reactiveValues(
          Spectrum_raw = NULL, Name = NULL, Destination = NULL
          )
    }
    
    Table <- shiny::reactive({
      temp <- Specs$Plot
      temp$Efficacy <- Specs$Efficacy %>% unlist()
      temp$Adjec <- names(temp$Efficacy)
      temp <- temp %>% 
        dplyr::mutate(
          E = purrr::map_dbl(Names, Spec_int, spectrum = Spectrum$Spectrum),
               Ev = E * temp$Efficacy[Adjec])
      temp
    })

    #Return Value
    Table
    
  })
}

# App ---------------------------------------------------------------------

analysis_setupApp <- function(lang_setting = "Deutsch") {
  
  ui <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(),
    shinydashboard::dashboardSidebar(),
    shinydashboard::dashboardBody(
      shiny::verbatimTextOutput("Data_ok"),
      shiny::actionButton("action", label = "Go", class = "btn-lg"),
      analysis_setupUI("setup")
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
                     Destination = "Import")
    
    Table <- analysis_setupServer("setup",Spectrum = Spectrum)
    
    output$Data_ok <- shiny::renderPrint({
      {
        Table()
        }
    }) %>% shiny::bindEvent(input$action, ignoreInit = TRUE)
    }
  shiny::shinyApp(ui, server)
}
