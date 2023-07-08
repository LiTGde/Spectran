
#' Title
#'
#' @param lang_setting 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
Spectran <- function(lang_setting = "Deutsch", ...) {
    
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
        UI_Header(),
        #Sidebar
        UI_Sidebar(),
        #Body
        shinydashboard::dashboardBody(
            #Add a link to the css resource
            htmltools::tags$link(
                rel = "stylesheet", type="text/css", href="extr/style.css"),
            # verbatimTextOutput("Data_ok"),
            shinydashboard::tabItems(
                #add a tab for the introduction
                shinydashboard::tabItem(tabName = "tutorial",
                         introductionUI("intro")),
                #add a tab for the import
                shinydashboard::tabItem(tabName = "import",
                         importUI("import")),
                #add a tab for the analysis
                shinydashboard::tabItem(tabName = "analysis",
                         analysisUI("analysis")),
                #add a tab for the export
                shinydashboard::tabItem(tabName = "export",
                         exportUI("export")),       
                #add a tab for the validity
                shinydashboard::tabItem(tabName = "validity",
                         validityUI("validity")),
                #add a tab for the impressum
                shinydashboard::tabItem(tabName = "impressum",
                         impressumUI("impressum"))
            )
            )
        )
    
    #Server
    server <- function(input, output, session) {
        
        #Introduction
        zu_Import <- introductionServer("intro")
        
        #Import
        Spectrum <- importServer("import")
        
        #Analysis
        Analysis <- analysisServer("analysis",
                       Spectrum = Spectrum)
        
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
            if(Spectrum$Destination == lang$ui(69)) {
            shiny::updateNavbarPage(
                session,
                inputId = "inTabset",
                selected = "analysis")
            }
        }) %>% shiny::bindEvent(Spectrum$Spectrum, 
                                Spectrum$Destination,
                                Spectrum$Name)
        
        output$Data_ok <- shiny::renderPrint({
        #     print("Developer Troubleshoot\n")
            # p990rint(list.files(path = "extr/"))
            print(Spectrum$Other)
        #     print(Spectrum$Other)
        #     print(Spectrum$Spectrum %>% utils::head())
        #     print(Spectrum$Spectrum %>% utils::tail())
        })
        
    }
    shiny::shinyApp(ui, server, ...)
}

