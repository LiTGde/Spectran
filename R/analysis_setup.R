
# UI ----------------------------------------------------------------------

analysis_setupUI <- 
  function(
    id 
    ) {
    ns <- shiny::NS(id)
    htmltools::tagList()
  }

# Server ------------------------------------------------------------------

analysis_setupServer <- 
  function(
    id, 
    Spectrum,
    Analysis
    ) {
  
  shiny::moduleServer(id, function(input, output, session) {
    
    #Create a Table that already contains some base information
    Table <- shiny::reactive({
      shiny::req(Spectrum$Spectrum,
          Spectrum$Destination == lang$ui(69))
      #general settings
      temp <- Specs$Plot
      #add Efficacy values
      temp$Efficacy <- Specs$Efficacy %>% unlist()
      #add ajectives
      temp$Adjec <- names(temp$Efficacy)
      #add several irradiance calculations
      temp <- temp %>% 
        dplyr::mutate(
          E = purrr::map_dbl(Names, Spec_int, spectrum = Spectrum$Spectrum),
          Ev = E * temp$Efficacy[Adjec],
          Emax = Spectrum$Spectrum$Bestrahlungsstaerke %>% max()*1000,
          Ewtd = purrr::map(Names, 
                            \(Names) {(Spec_wtd(Spectrum$Spectrum, Names))}
                            )
        )
      
      #return
      temp
    })
    
    #What is the maximum irradiance. This is used as a scaling-factor
    shiny::observe({
      shiny::req(Spectrum$Spectrum)
      Analysis$Settings <- list(
        Spectrum = Spectrum$Spectrum %>% tibble::as_tibble(),
        Spectrum_Name = Spectrum$Name,
        general = Table(),
        Origin = Spectrum$Origin
        )
      
    }) %>% shiny::bindEvent(Spectrum$Spectrum, Spectrum$Name)

  })
}

# App ---------------------------------------------------------------------

analysis_setupApp <- function(lang_setting = "Deutsch") {
  
  #set the language for the program
  the$language <- lang_setting
  
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
                     Destination = lang$ui(69))
    
    Table <- analysis_setupServer("setup",
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
