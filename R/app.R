
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
    
    ui <- shinydashboard::dashboardPage(
        shinydashboard::dashboardHeader(),
        shinydashboard::dashboardSidebar(),
        shinydashboard::dashboardBody(
            shiny::verbatimTextOutput("Data_ok"),
            shiny::tabsetPanel(
                shiny::tabPanel(title = "Import",
                         importUI("import")),
                shiny::tabPanel(title = "analysis",
                         analysisUI("analysis"))
            )
            
        ))
    
    server <- function(input, output, session) {
        Spectrum <- importServer("import")
        
        analysisServer("analysis", lang_setting = lang_setting,
                       Spectrum = Spectrum)
        
        output$Data_ok <- shiny::renderPrint({
            print("Developer Troubleshoot\n")
            print(Spectrum$Name)
            print(Spectrum$Destination)
            print(Spectrum$Other)
            print(Spectrum$Spectrum %>% utils::head())
            print(Spectrum$Spectrum %>% utils::tail())
        })
        
    }
    shiny::shinyApp(ui, server, ...)
}