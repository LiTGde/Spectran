

#' Unlock the Spectrum: Easy, Educational, and Engaging Analysis of Light Spectra
#'
#' @param lang_setting A language for the application. Currently **Deutsch** for German and **English** (default) are implemented. Expects a *character*.
#' @param lang_link Only relevant for the App deployed on *Shinyapps.io*. Handles whether a link to the German/English Version of the App is present in the header. Expects a *logical* (default FALSE)
#' @param color_palette A color palette for the application. Currently `**Lang**` (default), `**Lang_bright**`, `**Dan_Bruton**`, and `**Rainbow**` are implemented. Expects a `*character*`. In terms of `color accuracy`, the decending order is likely `**Dan_Bruton**`, `**Lang**`, `**Lang_bright**`, and `**Rainbow**`. However, all of them are wrong in the sense, that monochromatic light can not well be recreated with RGB colors. Look at the documentation for [ColorP] for more information about these palettes.
#' @param ... Any other settings that get passed to shinyApp
#'
#' @return Open a viewer with the shiny app
#' @export
#' 
#' @examples
#' if(interactive()) {
#' Spectran()}
#' 
#' #try another language
#' if(interactive()) {
#' Spectran(lang_setting = "Deutsch")}
#' 
#' #or try another color palette
#' if(interactive()) {
#' Spectran(color_palette = "Dan_Bruton")}
#'
Spectran <- function(lang_setting = "English", 
                     lang_link = FALSE,
                     color_palette = "Lang",
                     ...) {
    
    #add a resource path to the www folder
    shiny::addResourcePath(
        "extr", system.file("app/www", package = "Spectran"))
    # on.exit(shiny::removeResourcePath("extr"), add = TRUE)
    
    #set the language and color palette for the program
    the$language <- lang_setting
    the$palette <- color_palette

    #create an Environment that holds the plotwidths of users
    theuser <- new.env(parent = emptyenv())
    theuser$Plotbreite_temp <- 200
    
    
    #UI
    ui <-
        shinydashboard::dashboardPage( 
            skin = "yellow",
        #Header
        UI_Header(lang_link),
        #Sidebar
        UI_Sidebar(),
        #Body
        shinydashboard::dashboardBody(
            #Add a link to the css resource
            htmltools::tags$link(
                rel = "stylesheet", type="text/css", href="extr/style.css"),
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
            ),
            shiny::fluidPage(
            (shiny::plotOutput("Plotbreite", height = "1px"))),
            waiter::useWaiter(),
            # waiter::waiterOnBusy(html = waiter::spin_solar(),
            #                      color = "#2874A625", fadeout = 100),
            waiter::autoWaiter(html = waiter::spin_solar(),
                               color = "#2874A625", fadeout = 100),
            waiter::waiterPreloader(
                html = waiter::spin_solar(), color = "#2874A625", fadeout = 500)
            )
        )
    
    #Server
    server <- function(input, output, session) {
        
        #allow reconnect
        session$allowReconnect(TRUE)
        
        #Introduction
        zu_Import <- introductionServer("intro")
        
        #Import
        Spectrum <- importServer("import")
        
        #Analysis
        Analysis <- analysisServer("analysis",
                       Spectrum = Spectrum,
                       Tabactive = shiny::reactive(input$inTabset))
        
        #Export
        Export <- exportServer("export", Analysis, Spectrum,
                               Tabactive = shiny::reactive(input$inTabset))
        
        output$Plotbreite <- shiny::renderPlot({
        }, 
        bg = "transparent")
        # bg = "white")
        
        # Delete Notifications between tab changes
        notification_remover(shiny::reactive(input$inTabset))
        
        #Update the Navbar, when the Introduction is finished
        shiny::observe({
            shiny::updateNavbarPage(
                session, inputId = "inTabset" ,  selected = "import")
        }) %>% shiny::bindEvent(
            zu_Import(), ignoreInit = TRUE
        )
        
        #Enable/Disable the Analysis and Import Menus, when not ready
        output$analysis <- shinydashboard::renderMenu({
            if(!is.null(Analysis$Settings$Spectrum)) {
            shinydashboard::menuItem(
            lang$ui(23),
            tabName = "analysis",
            icon = shiny::icon(
                "magnifying-glass-chart")
            )
            }
            else {
                shinydashboard::menuItem(
                    htmltools::HTML(
                        paste0("<span style='color:grey;'>",
                            lang$ui(23),
                            "</span>")),
                    tabName = "import",
                    icon = shiny::icon(
                        "lock")
                )
            }
        })
        output$export <- shinydashboard::renderMenu({
            if(!is.null(Analysis$Settings$Spectrum)) {
            shinydashboard::menuItem(
            "Export",
            tabName = "export",
            icon = shiny::icon("file-export")
            )
            }
            else {
                shinydashboard::menuItem(
                    htmltools::HTML(
                        paste0("<span style='color:grey;'>",
                               "Export",
                               "</span>")),
                    tabName = "import",
                    icon = shiny::icon(
                        "lock")
                )
            }
        })
        
        #Update the Navbar when hitting the Export-Button in Analysis
        shiny::observe({
            shiny::updateNavbarPage(
                session,
                inputId = "inTabset",
                selected = "export")
        }) %>% shiny::bindEvent(Analysis$to_export)
        
        
        #Update the Navbar when a Spectrum is imported
        shiny::observe({
            shiny::req(Spectrum$Spectrum, Spectrum$Destination)
            if(Spectrum$Destination == lang$ui(69) &
               input$inTabset == "import") {
                shiny::updateNavbarPage(
                    session,
                    inputId = "inTabset",
                    selected = "analysis")
            }
        }) %>% shiny::bindEvent(Spectrum$Analysis, Analysis$Settings$Spectrum)
        
        #close the waiting screen
        # waiter::waiter_hide()
        
    }
    shiny::shinyApp(ui, server, ...)
}

