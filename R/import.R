
# UI ----------------------------------------------------------------------

importUI <- function(
    id, lang_setting = get("lang_setting", envir = caller_env(n = 1))) {
  tagList(
    withMathJax(),
    tabsetPanel(id = NS(id, "inTabset"),
                tabPanel(
                         title = lang$ui(69),
                         import_dataUI(NS(id, "fileimport"))),
                tabPanel(
                         title = lang$ui(93),
                         import_examplesUI(NS(id, "examples"))),
                tabPanel(
                         title = lang$ui(94),
                         import_eigenUI(NS(id, "eigen")))
    )
  )
}

# Server ------------------------------------------------------------------

importServer <- 
  function(id, 
           lang_setting = get("lang_setting", envir = caller_env(n = 1)),
           Spectrum = NULL
           ) {
  
  moduleServer(id, function(input, output, session) {
    
    #Set up a container for the spectra to go into, if it isn´t already defined
    if (is.null(Spectrum)){
      Spectrum <- 
        reactiveValues(Spectrum = NULL, Name = NULL, Destination = NULL)
    }
    
      import_dataServer("fileimport", 
                        lang_setting = lang_setting,
                        Spectrum = Spectrum)
      import_examplesServer("examples", 
                            lang_setting = lang_setting,
                            Spectrum = Spectrum)
      import_eigenServer("eigen", 
                            lang_setting = lang_setting,
                            Spectrum = Spectrum)
      import_verifierServer("verify_import",
                            lang_setting = lang_setting,
                            Spectrum = Spectrum)

      #Update the Navbar when a Spectrum is imported
    observe({
      updateNavbarPage(
        session,
        inputId = "inTabset",
        selected = Spectrum$Destination)
    }) %>% bindEvent(Spectrum$Spectrum, Spectrum$Destination)

    observe({
      c("is_sufficient", "is_integer", "is_numeric", "belowz", "success") %>% 
        map(removeNotification)
    }) %>% bindEvent(input$inTabset)
    
    #Return value
    Spectrum
  })
}

# App ---------------------------------------------------------------------

importApp <- function(lang_setting = "Deutsch") {
  
  ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody(
    verbatimTextOutput("Data_ok"),
    importUI("import")
    ))
  
  server <- function(input, output, session) {
    Spectrum <- importServer("import")

    output$Data_ok <- renderPrint({
      print("Developer Troubleshoot\n")
      print(Spectrum$Name)
      print(Spectrum$Destination)
      print(Spectrum$Other)
      print(Spectrum$Spectrum %>% head())
      print(Spectrum$Spectrum %>% tail())
    })
    
    }
  shinyApp(ui, server)
}
