
# UI ----------------------------------------------------------------------

import_examplesUI <- 
  function(
    id) {
    ns <- shiny::NS(id)
    
    CCT_Daylight <- list(
      htmltools::tagList(htmltools::div(
        style = "display:inline-block",
        shiny::numericInput(
          ns("CCT_norm"),
          label = lang$ui(72),
          value = 6500,
          min = 4000,
          max = 25000,
          width = "100%",
          step = 1000
        )
      ),
      " K",
      htmltools::p("created by ", 
                   htmltools::a("colorSpec", 
                                target = "_blank",
                                href = URL_colorSpec)
                   ))
      )

    examplespectraUI <- purrr::map(examplespectra_descriptor, \(x) { x %>% 
      dplyr::transmute(id = ns(Name),
             title = Beschreibung,
             left = list(list("video", URL)),
             mid = list(list("plot", Name)),
             right = list(list("download", download)))})
        
    htmltools::tagList(
      shinyFeedback::useShinyFeedback(),
      shiny::fluidRow(
        shinydashboard::box(
          width = 12,
          htmltools::p(
            #general information
            lang$ui(64),
            htmltools::a(
              .noWS = "outside",
              "Jeti Specbos 1201",
              href = "https://www.jeti.com",
              target = "_blank"
            ),
            lang$ui(65),
            htmltools::br(),
            lang$ui(66),
            htmltools::strong(.noWS = "outside", lang$ui(67)),
            lang$ui(68)
          ),
          #Setting illuminance
          shiny::column(
            width = 6,
            htmltools::div(style = "display:inline-block",
                           shiny::numericInput(
                            ns("illu_eigen"),
                             lang$ui(70),
                             value = 100,
                             min = 0
                             )),
            " lux"
          ),
          #Setting the option to download or import
          shiny::column(width = 6,
                        shinyWidgets::radioGroupButtons(
                          ns("down_import"),
                   label = "",
                   selected = "Import",
                   choices = c("Download", "Import"),
                   checkIcon = list(
                     yes = shiny::icon("ok", 
                                lib = "glyphicon"),
                     no = shiny::icon("remove",
                               lib = "glyphicon"))
                 )
                 )
        ),
        #Box for the Norm Daylight spectrum
        import_examples_boxUI(
          ns("norm"),
          title = lang$ui(71),
          left = c("", CCT_Daylight),
          mid = c("plot", "norm"),
          right = list("download", c(norm = "Download/Import"))
        ),

        #Boxes for all other spectra
        purrr::pmap(
          examplespectraUI[[the$language]],
          import_examples_boxUI)

    )
    )
  }

# Server ------------------------------------------------------------------

import_examplesServer <- 
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
    
    #Server logic for all spectra except the adjustable Daylight
    #Illuminant
    purrr::pmap(list(
      id = examplespectra_descriptor[[the$language]]$Name,
      examplespectra_descriptor =
        DF2list(examplespectra_descriptor[[the$language]])
    ),
    import_examples_boxServer,
    illu_eigen = shiny::reactive(input$illu_eigen),
    down_import = shiny::reactive(input$down_import),
    examplespectra = examplespectra,
    Spectrum = Spectrum
    )
    
    # #Adjusting the names of the spectra
    # names(Spectrum) <- examplespectra_descriptor[[the$language]]$Name
    
    #Server logic for the adjustable Daylight Illuminant
    import_examples_boxServer(
      id = "norm",
      Spectrum = Spectrum,
      examplespectra_descriptor = tibble::tibble(
        download = list(list("norm") %>% stats::setNames(lang$ui(71)))),
      illu_eigen = shiny::reactive(input$illu_eigen),
      down_import = shiny::reactive(input$down_import),
      daylight_CCT = shiny::reactive(input$CCT_norm) #only necessary for the
      #spectrum daylight
    )

    # Error message, should the cct be outside the valid area
    shiny::observeEvent(input$CCT_norm, {
      shinyFeedback::feedbackDanger("CCT_norm",
                     !(input$CCT_norm <=25000 & input$CCT_norm >=4000),
                     lang$server(34))
      })

    
    #Return Value
    Spectrum

  })
}

# App ---------------------------------------------------------------------

import_examplesApp <- function(lang_setting = "Deutsch") {
  
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
                  import_examplesUI("examples")
                  )
       
      )
  server <- function(input, output, session) {

    Spectrum <- import_examplesServer("examples")
    output$Data_ok <- shiny::renderPrint({
      {
        print(Spectrum$Name)
        print(Spectrum$Destination)
        Spectrum$Spectrum_raw %>% utils::head()
        }
    })
    }
  shiny::shinyApp(ui, server)
}
