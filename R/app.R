#library calls - will be removed later
library(shiny)
library(tidyverse)
library(rlang)
library(shiny)
library(shinyWidgets)
library(htmltools)
library(shinydashboard)
library(shinyFeedback)
library(gt)
library(cowplot)
library(shinyalert)
library(colorSpec)
library(ggridges)
library(ggrepel)



Spectran <- function(lang_setting = "Deutsch", ...) {
    
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
    shinyApp(ui, server, ...)
}