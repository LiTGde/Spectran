
# UI ----------------------------------------------------------------------

analysis_setupUI <- 
  function(
    id, 
    lang_setting = get("lang_setting", envir = caller_env(n = 1))
    ) {
    ns <- NS(id)
    tagList()
  }

# Server ------------------------------------------------------------------

analysis_setupServer <- 
  function(
    id, 
    lang_setting = get("lang_setting", envir = caller_env(n = 1)),
    Spectrum = NULL
    ) {
  
  moduleServer(id, function(input, output, session) {
    #Set up a container for the spectra to go into, if it isn´t already defined
    if (is.null(Spectrum)){
      Spectrum <- 
        reactiveValues(Spectrum_raw = NULL, Name = NULL, Destination = NULL)
    }
    
    Table <- reactive({
      temp <- Specs$Plot
      temp$Efficacy <- Specs$Efficacy %>% unlist()
      temp$Adjec <- names(temp$Efficacy)
      temp <- temp %>% 
        mutate(E = map_dbl(Names, Spec_int, spectrum = Spectrum$Spectrum),
               Ev = E * temp$Efficacy[Adjec])
      temp
    })

    #Return Value
    Table
    
  })
}

# App ---------------------------------------------------------------------

analysis_setupApp <- function(lang_setting = "Deutsch") {
  
  ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody(verbatimTextOutput("Data_ok"),
                  actionButton("action", label = "Go", class = "btn-lg"),
                  analysis_setupUI("setup")
                  )
       
      )
  server <- function(input, output, session) {

    test.spectrum <-  
      examplespectra$Measurement %>% 
      select(Wellenlaenge, led) %>% 
      rename(Bestrahlungsstaerke = led)
    
    Spectrum <- 
      reactiveValues(Spectrum = test.spectrum, 
                     Name = "Test", 
                     Destination = "Import")
    
    Table <- analysis_setupServer("setup",Spectrum = Spectrum)
    
    output$Data_ok <- renderPrint({
      {
        Table()
        }
    }) %>% bindEvent(input$action, ignoreInit = TRUE)
    }
  shinyApp(ui, server)
}
