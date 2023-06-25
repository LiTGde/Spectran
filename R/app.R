
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
    
    ui <- dashboardPage(
        dashboardHeader(),
        dashboardSidebar(),
        dashboardBody(
            verbatimTextOutput("Data_ok"),
            tabsetPanel(
                tabPanel(title = "Import",
                         importUI("import")),
                tabPanel(title = "analysis",
                         analysisUI("analysis"))
            )
            
        ))
    
    server <- function(input, output, session) {
        Spectrum <- importServer("import")
        
        analysisServer("analysis", lang_setting = lang_setting,
                       Spectrum = Spectrum)
        
        output$Data_ok <- renderPrint({
            print("Developer Troubleshoot\n")
            print(Spectrum$Name)
            print(Spectrum$Destination)
            print(Spectrum$Other)
            print(Spectrum$Spectrum %>% head())
            print(Spectrum$Spectrum %>% tail())
        })
        
    }
    shinyApp(ui, server, ...)
}