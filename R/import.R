
# UI ----------------------------------------------------------------------

importUI <- function(
    id, 
    lang_setting = get("lang_setting", envir = rlang::caller_env(n = 1))
    ) {
  htmltools::tagList(
    shiny::withMathJax(),
    shiny::tabsetPanel(id = shiny::NS(id, "inTabset"),
                shiny::tabPanel(
                         title = lang$ui(69),
                         import_dataUI(shiny::NS(id, "fileimport"))),
                shiny::tabPanel(
                         title = lang$ui(93),
                         import_examplesUI(shiny::NS(id, "examples"))),
                shiny::tabPanel(
                         title = lang$ui(94),
                         import_eigenUI(shiny::NS(id, "eigen")))
    )
  )
}

# Server ------------------------------------------------------------------

importServer <- 
  function(id, 
           lang_setting = get(
             "lang_setting", envir = rlang::caller_env(n = 1)
             ),
           Spectrum = NULL
           ) {
  
    shiny::moduleServer(id, function(input, output, session) {
    
    #Set up a container for the spectra to go into, if it isn´t already defined
    if (is.null(Spectrum)){
      Spectrum <- 
        shiny::reactiveValues(
          Spectrum = NULL, Name = NULL, Destination = NULL
          )
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
      shiny::observe({
        shiny::updateNavbarPage(
        session,
        inputId = "inTabset",
        selected = Spectrum$Destination)
    }) %>% shiny::bindEvent(Spectrum$Spectrum, Spectrum$Destination)

      shiny::observe({
      c("is_sufficient", "is_integer", "is_numeric", "belowz", "success") %>% 
        purrr::map(removeNotification)
    }) %>% shiny::bindEvent(input$inTabset)
    
    #Return value
    Spectrum
  })
}

# App ---------------------------------------------------------------------

importApp <- function(lang_setting = "Deutsch") {
  
  ui <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(),
    shinydashboard::dashboardSidebar(),
    shinydashboard::dashboardBody(
    shiny::verbatimTextOutput("Data_ok"),
    importUI("import")
    ))
  
  server <- function(input, output, session) {
    Spectrum <- importServer("import")

    output$Data_ok <- shiny::renderPrint({
      print("Developer Troubleshoot\n")
      print(Spectrum$Name)
      print(Spectrum$Destination)
      print(Spectrum$Other)
      print(Spectrum$Spectrum %>% utils::head())
      print(Spectrum$Spectrum %>% utils::tail())
    })
    
    }
  shiny::shinyApp(ui, server)
}
