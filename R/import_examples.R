
# UI ----------------------------------------------------------------------

import_examplesUI <- 
  function(
    id, 
    lang_setting = get("lang_setting", envir = caller_env(n = 1))
    ) {
    ns <- NS(id)
    
    CCT_Daylight <- list(
      tagList(div(
        style = "display:inline-block",
        numericInput(
          ns("CCT_norm"),
          label = lang$ui(72),
          value = 6500,
          min = 4000,
          max = 25000,
          width = "100%"
        )
      ),
      " K"))

    examplespectraUI <- map(examplespectra_descriptor, \(x) { x %>% 
      transmute(id = ns(Name),
             title = Beschreibung,
             left = list(list("video", URL)),
             mid = list(list("image", Name)),
             right = list(list("download", download)))})
        
    tagList(
      useShinyFeedback(),
      fluidPage(
        box(
          width = 12,
          p(
            #general information
            lang$ui(64),
            a(
              .noWS = "outside",
              "Jeti Specbos 1201",
              href = "https://www.jeti.com",
              target = "_blank"
            ),
            lang$ui(65),
            br(),
            lang$ui(66),
            strong(.noWS = "outside", lang$ui(67)),
            lang$ui(68)
          ),
          #Setting illuminance
          column(
            width = 6,
            div(style = "display:inline-block",
                numericInput(
                  ns("illu_eigen"),
                  lang$ui(70),
                  value = 100,
                  min = 0
                )),
            " lux"
          ),
          #Setting the option to download or import
          column(width = 6,
                 radioGroupButtons(
                   ns("down_import"),
                   label = "",
                   selected = "Import",
                   choices = c("Download", "Import"),
                   checkIcon = list(
                     yes = icon("ok", 
                                lib = "glyphicon"),
                     no = icon("remove",
                               lib = "glyphicon"))
                 )
                 )
        ),
        #Box for the Norm Daylight spectrum
        import_examples_boxUI(
          ns("norm"),
          title = lang$ui(71),
          left = c("", CCT_Daylight),
          mid = c("image", "norm"),
          right = list("download", c(norm = "Download/Import"))
        ),

        #Boxes for all other spectra
        pmap(
          examplespectraUI[[lang_setting]],
          import_examples_boxUI,
          lang_setting = lang_setting)
      )
    )
  }

# Server ------------------------------------------------------------------

import_examplesServer <- 
  function(
    id, 
    lang_setting = get("lang_setting", envir = caller_env(n = 1)),
    Spectrum = NULL
    ) {
  
  moduleServer(id, function(input, output, session) {
    #Set up a container for the spectra to go into, if it isn´t already defined
    if (is.null(Spectrum)){
      Spectrum <- 
        reactiveValues(Spectrum_raw = NULL, Name = NULL, Destination = NULL)
    }
    
    #Server logic for all spectra except the adjustable Daylight
    #Illuminant
    pmap(list(
      id = examplespectra_descriptor[[lang_setting]]$Name,
      examplespectra_descriptor =
        DF2list(examplespectra_descriptor[[lang_setting]])
    ),
    import_examples_boxServer,
    lang_setting = lang_setting,
    illu_eigen = reactive(input$illu_eigen),
    down_import = reactive(input$down_import),
    examplespectra = examplespectra,
    Spectrum = Spectrum
    )
    
    # #Adjusting the names of the spectra
    # names(Spectrum) <- examplespectra_descriptor[[lang_setting]]$Name
    
    #Server logic for the adjustable Daylight Illuminant
    import_examples_boxServer(
      id = "norm",
      Spectrum = Spectrum,
      examplespectra_descriptor = tibble(
        download = list(list("norm") %>% setNames(lang$ui(71)))),
      lang_setting = lang_setting,
      illu_eigen = reactive(input$illu_eigen),
      down_import = reactive(input$down_import),
      daylight_CCT = reactive(input$CCT_norm) #only necessary for the daylight
      #spectrum
    )

    # Error message, should the cct be outside the valid area
    observeEvent(input$CCT_norm, {
      feedbackDanger("CCT_norm",
                     !(input$CCT_norm <=25000 & input$CCT_norm >=4000),
                     lang$server(34))
      })

    
    #Return Value
    Spectrum

  })
}

# App ---------------------------------------------------------------------

import_examplesApp <- function(lang_setting = "Deutsch") {
  
  ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody(verbatimTextOutput("Data_ok"),
                  import_examplesUI("examples")
                  )
       
      )
  server <- function(input, output, session) {

    Spectrum <- import_examplesServer("examples")
    output$Data_ok <- renderPrint({
      {
        print(Spectrum$Name)
        print(Spectrum$Destination)
        Spectrum$Spectrum_raw %>% head()
        }
    })
    }
  shinyApp(ui, server)
}
