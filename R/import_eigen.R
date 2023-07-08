
# UI ----------------------------------------------------------------------

import_eigenUI <- 
  function(
    id) {

    ns <- shiny::NS(id)
    
    htmltools::tagList(
      shiny::withMathJax(),
      shiny::fluidRow(
        #Starting with an explainer box
        shinydashboard::box(
          width = 12,
          shinydashboard::box(
            title = lang$ui(73),
            htmltools::p(
              lang$ui(74),
              htmltools::em(.noWS = "outside", lang$ui(69)),
              lang$ui(75),
              htmltools::em(.noWS = "outside", lang$ui(63)),
              lang$ui(76),
              htmltools::br(),
              htmltools::br(),
              htmltools::strong(.noWS = "outside", lang$ui(77)),
              lang$ui(78),
              htmltools::br(),
              htmltools::strong(.noWS = "outside", lang$ui(79)),
              lang$ui(80),
              htmltools::br(),
              htmltools::strong(.noWS = "outside", lang$ui(81)),
              lang$ui(82),
              htmltools::br(),
              htmltools::strong(.noWS = "outside", lang$ui(83)),
              lang$ui(84),
              htmltools::br(),
              htmltools::br(),
              htmltools::em(.noWS = "outside", lang$ui(85)),
              lang$ui(86), 
              htmltools::br(),
              htmltools::strong(.noWS = "outside", lang$ui(87))
            ),
            collapsed = TRUE,
            collapsible = TRUE,
            width = 12,
            align = "left"
          ),
          shiny::fluidPage(
            #choosing which action spectra are shown in the plot
            shiny::checkboxGroupInput(
              ns("sensitivitaeten"),
              label = lang$ui(88),
              choiceNames = c(unname(Specs$Alpha$names),
                              paste(lang$ui(89), Specs$Vlambda)
                              ),
              choiceValues = unname(Specs$Plot$Names),
              inline = TRUE
            ),
            #setting an individual illuminance for the resulting spectrum
            shiny::column(
              width = 6,
              shiny::checkboxInput(ns("is_illu_eigen"),
                            lang$ui(90),
                            value = TRUE)
            ),
            shiny::column(width = 6, shiny::uiOutput(ns("eigene_Skala"))),
            #setting a name for the spectrum generated here
            shiny::textInput(ns("name_id"), 
                      label = lang$ui(60), 
                      value = lang$ui(61), 
                      width = "80%")),
          import_eigen_plotUI(ns("plot"))
          ),
      shiny::fluidRow(
        #Import Button
        shiny::actionButton(
          ns("uebernahme"),
          lang$ui(91),
          class = "btn-lg",
          shiny::icon("play", lib = "glyphicon")
          ),
        "  ",
        #Reset Button
        shiny::actionButton(
          ns("zurueck"),
          lang$ui(92),
          shiny::icon("fast-backward", lib = "glyphicon")
          ),
        align = "center"
        )
    )
    )
    }

# Server ------------------------------------------------------------------

import_eigenServer <- 
  function(
    id, 
    Spectrum = NULL
    ) {
  
    shiny::moduleServer(id, function(input, output, session) {
    
    #Set up a container for the spectra to go into, if it isn´t already defined
    if (is.null(Spectrum)){
      Spectrum <- 
        shiny::reactiveValues(
          Spectrum_raw = NULL, Name = NULL, Destination = NULL
          )
    }
    
    eigen_Spectrum <- import_eigen_plotServer("plot",
                            Spectrum = Spectrum,
                            sensitivitaeten = 
                              shiny::reactive(input$sensitivitaeten),
                            default = shiny::reactive(input$zurueck),
                            import = shiny::reactive(input$uebernahme))
    
    #Show the Scale-Value
    output$eigene_Skala <- shiny::renderUI({
      shiny::req(input$is_illu_eigen == TRUE)
      ns <- session$ns
      shiny::fluidRow(
        htmltools::div(
          style = "display:inline-block",

          shiny::numericInput(
            ns("illu_eigen"),
            lang$server(35),
            value = Spectrum$Illu %||% 100,
            min = 0, width = "100%"
          )
        ),
        " lux")
    })
    shiny::outputOptions(output, "eigene_Skala", suspendWhenHidden = FALSE)

    #Change the Name
    shiny::observe({
      shiny::updateTextInput(session, "name_id", value = Spectrum$Name)
    })

    #Import the Spectrum
    shiny::observe ({
      Ev <- 
        eigen_Spectrum()$Bestrahlungsstaerke %>% 
        Calc_lux(Specs$AS_wide, Specs$Efficacy)
      
      Spectrum$Other <- 
      
      if(input$is_illu_eigen) {
        Spectrum$Spectrum_raw <-  
          eigen_Spectrum() %>% 
          dplyr::mutate(
            Bestrahlungsstaerke = Bestrahlungsstaerke/Ev*input$illu_eigen)
      }      
      else if (
        all(
          !is.null(Spectrum$Spectrum)
          )
        ) {
        Spectrum$Spectrum_raw <-
          eigen_Spectrum() %>%
          dplyr::mutate(
            Bestrahlungsstaerke =
            Bestrahlungsstaerke*Spectrum$Emax_orig
          )
      }
      else {
        Spectrum$Spectrum_raw <- 
          eigen_Spectrum() %>% 
          dplyr::mutate(
            Bestrahlungsstaerke = Bestrahlungsstaerke/Ev*100)
      }
      
      Spectrum$Origin <- "Construction"
      Spectrum$Destination <- lang$ui(69)
      Spectrum$Name <- Name_suffix(Spectrum$Origin, input$name_id)
      
    }) %>% shiny::bindEvent(input$uebernahme)
    
    #Remove Scaling when Importing a spectrum for adjustment
    shiny::observe({
      if(all((Spectrum$Destination) == lang$ui(94),
             !is.null((Spectrum$Spectrum)))) {
        
        shiny::updateCheckboxInput(session, "is_illu_eigen", value = FALSE)
      }
    }) %>% shiny::bindEvent(Spectrum$Destination, Spectrum$Spectrum)
    
    
    #Return Value
    Spectrum

  })
}

# App ---------------------------------------------------------------------

import_eigenApp <- function(lang_setting = "Deutsch") {
  
  #add a resource path to the www folder
  shiny::addResourcePath(
    "extr", system.file("app/www", package = "Spectran"))
  # on.exit(shiny::removeResourcePath("extr"), add = TRUE)
  
  #set the language for the program
  the$language <- lang_setting
  
  ui <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(),
    shinydashboard::dashboardSidebar(),
    shinydashboard::dashboardBody(shiny::verbatimTextOutput("Data_ok"),
                  import_eigenUI("eigen")
                  )
       
      )
  server <- function(input, output, session) {

    Spectrum <- import_eigenServer("eigen")
    output$Data_ok <- shiny::renderPrint({
      {
        print(cat("Developer Troubleshoot\n"))
        print(Spectrum$Name)
        print(Spectrum$Destination)
        print(Spectrum$Other)
        Spectrum$Spectrum_raw %>% utils::head()
        }
    })
    }
  shiny::shinyApp(ui, server)
}
