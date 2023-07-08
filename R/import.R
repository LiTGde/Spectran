
# UI ----------------------------------------------------------------------

importUI <- function(
    id) {
  htmltools::tagList(
    shiny::withMathJax(),
    htmltools::h3("Import"),
    shiny::tabsetPanel(
                            id = shiny::NS(id, "inTabset"),
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
    
      import_verifierServer("verify_import",
                            Spectrum = Spectrum)
      import_dataServer("fileimport", 
                        Spectrum = Spectrum)
      import_examplesServer("examples", 
                            Spectrum = Spectrum)
      import_eigenServer("eigen", 
                            Spectrum = Spectrum)

      #Update the Navbar when a Spectrum is imported
      shiny::observe({
        shiny::updateNavbarPage(
        session,
        inputId = "inTabset",
        selected = Spectrum$Destination)
    }) %>% shiny::bindEvent(Spectrum$Spectrum, Spectrum$Destination)

    #remove notifications
      notification_remover(shiny::reactive(input$inTabset))
      
    #set a notification on Name changes
      shiny::observe({
        shiny::showNotification( type = "message",
          htmltools::tagList("Name: ", htmltools::strong(Spectrum$Name)))
      }) %>% shiny::bindEvent(Spectrum$Name)
      
    #Return value
    Spectrum
  })
}

# App ---------------------------------------------------------------------

importApp <- function(lang_setting = "Deutsch") {
  
  #add a resource path to the www folder
  shiny::addResourcePath(
    "extr", system.file("app/www", package = "Spectran"))
  # on.exit(shiny::removeResourcePath("extr"), add = TRUE)
  
  #set the language for the program
  the$language <- lang_setting
  
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
