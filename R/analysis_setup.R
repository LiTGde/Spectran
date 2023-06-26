
# UI ----------------------------------------------------------------------

analysis_setupUI <- 
  function(
    id, 
    lang_setting = get("lang_setting", envir = rlang::caller_env(n = 1))
    ) {
    ns <- shiny::NS(id)
    htmltools::tagList()
  }

# Server ------------------------------------------------------------------

analysis_setupServer <- 
  function(
    id, 
    lang_setting = get("lang_setting", envir = rlang::caller_env(n = 1)),
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
    
    #Create a Table that already contains some base information
    Table <- shiny::reactive({
      req(Spectrum$Spectrum,
          Spectrum$Destination == lang$ui(69))
      temp <- Specs$Plot
      temp$Efficacy <- Specs$Efficacy %>% unlist()
      temp$Adjec <- names(temp$Efficacy)
      temp <- temp %>% 
        dplyr::mutate(
          E = purrr::map_dbl(Names, Spec_int, spectrum = Spectrum$Spectrum),
          Ev = E * temp$Efficacy[Adjec])
      temp
    })
    
    #What is the maximum irradiance. This is used as a scaling-factor
    shiny::observe({
      req(Spectrum$Spectrum)
      Spectrum$maxE <- Spectrum$Spectrum$Bestrahlungsstaerke %>% max()*1000
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
      # shiny::actionButton("action", label = "Go", class = "btn-lg"),
      analysis_setupUI("setup"),
      analysis_radioUI("radio")
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
    
    Table <- analysis_setupServer("setup",
                                  lang_setting = lang_setting,
                                  Spectrum = Spectrum)
    
    analysis_radioServer("radio", 
                         lang_setting = lang_setting,
                         Spectrum = Spectrum)
    
    output$Data_ok <- shiny::renderPrint({
      {
        print(Spectrum$maxE)
        print(Spectrum$Name)
        print(Spectrum$Destination)
        print(Spectrum$radiometric)
        Table()
        }
    })
    }
  shiny::shinyApp(ui, server)
}
