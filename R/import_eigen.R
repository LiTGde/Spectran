
# UI ----------------------------------------------------------------------

import_eigenUI <- 
  function(
    id, 
    lang_setting = get("lang_setting", envir = caller_env(n = 1))
    ) {

    ns <- NS(id)
    
    tagList(
      withMathJax(),
      fluidPage(
        br(),
        #Starting with an explainer box
        box(
          width = 12,
          box(
            title = lang$ui(73),
            p(
              lang$ui(74),
              em(.noWS = "outside", lang$ui(69)),
              lang$ui(75),
              em(.noWS = "outside", lang$ui(63)),
              lang$ui(76),
              br(),
              br(),
              strong(.noWS = "outside", lang$ui(77)),
              lang$ui(78),
              br(),
              strong(.noWS = "outside", lang$ui(79)),
              lang$ui(80),
              br(),
              strong(.noWS = "outside", lang$ui(81)),
              lang$ui(82),
              br(),
              strong(.noWS = "outside", lang$ui(83)),
              lang$ui(84),
              br(),
              br(),
              em(.noWS = "outside", lang$ui(85)),
              lang$ui(86), 
              br(),
              strong(.noWS = "outside", lang$ui(87))
            ),
            collapsed = TRUE,
            collapsible = TRUE,
            width = 12,
            align = "left"
          ),
          fluidPage(
            #choosing which action spectra are shown in the plot
            checkboxGroupInput(
              ns("sensitivitaeten"),
              label = lang$ui(88),
              choiceNames = c(unname(Specs$Alpha$names),
                              paste(lang$ui(89), Specs$Vlambda)
                              ),
              choiceValues = unname(Specs$Plot$Names),
              inline = TRUE
            ),
            #setting an individual illuminance for the resulting spectrum
            column(
              width = 6,
              checkboxInput(ns("is_illu_eigen"),
                            lang$ui(90),
                            value = TRUE)
            ),
            column(width = 6, uiOutput(ns("eigene_Skala"))),
            #setting a name for the spectrum generated here
            textInput(ns("name_id"), 
                      label = lang$ui(60), 
                      value = lang$ui(61), 
                      width = "80%")),
          import_eigen_plotUI(ns("plot"))
          ),
        #Import Button
        actionButton(
          ns("uebernahme"),
          lang$ui(91),
          class = "btn-lg",
          icon("play", lib = "glyphicon")
          ),
        "  ",
        #Reset Button
        actionButton(
          ns("zurueck"),
          lang$ui(92),
          icon("fast-backward", lib = "glyphicon")
          ),
        align = "center"
        )
    )
    }

# Server ------------------------------------------------------------------

import_eigenServer <- 
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
    
    eigen_Spectrum <- import_eigen_plotServer("plot",
                            lang_setting = lang_setting,
                            Spectrum = Spectrum,
                            sensitivitaeten = reactive(input$sensitivitaeten),
                            default = reactive(input$zurueck),
                            import = reactive(input$uebernahme))
    
    #Show the Scale-Value
    output$eigene_Skala <- renderUI({
      req(input$is_illu_eigen)
      ns <- session$ns
      fluidRow(
        div(
          style = "display:inline-block",

          numericInput(
            ns("illu_eigen"),
            lang$server(35),
            value = Spectrum$Illu %||% 100,
            min = 0, width = "100%"
          )
        ),
        " lux")
    })
    outputOptions(output, "eigene_Skala", suspendWhenHidden = FALSE)

    #Change the Name
    observe({
      updateTextInput(session, "name_id", value = Spectrum$Name)
    })

    #Import the Spectrum
    observe ({
      Ev <- 
        eigen_Spectrum()$Bestrahlungsstaerke %>% 
        Calc_lux(Specs$AS_wide, Specs$Efficacy)

      if(input$is_illu_eigen) {
        Spectrum$Spectrum_raw <-  
          eigen_Spectrum() %>% 
          mutate(Bestrahlungsstaerke = Bestrahlungsstaerke/Ev*input$illu_eigen)
      }      
      else if (all((Spectrum$Destination) == lang$ui(94),
                  !is.null((Spectrum$Spectrum)))) {
        Spectrum$Spectrum_raw <-
          eigen_Spectrum() %>%
          mutate(
            Bestrahlungsstaerke =
            Bestrahlungsstaerke*max(Spectrum$Spectrum$Bestrahlungsstaerke)
          )
      }
      else {
        Spectrum$Spectrum_raw <- eigen_Spectrum()/Ev*100
      }
      
      Spectrum$Destination <- lang$ui(69)
      Spectrum$Name <- input$name_id
      
    }) %>% bindEvent(input$uebernahme)
    
    #Remove Scaling when Importing a spectrum for adjustment
    observe({
      if(all((Spectrum$Destination) == lang$ui(94),
             !is.null((Spectrum$Spectrum)))) {
        
        updateCheckboxInput(session, "is_illu_eigen", value = FALSE)
      }
    }) %>% bindEvent(Spectrum$Destination, Spectrum$Spectrum)
    
    
    #Return Value
    Spectrum

  })
}

# App ---------------------------------------------------------------------

import_eigenApp <- function(lang_setting = "Deutsch") {
  
  ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody(verbatimTextOutput("Data_ok"),
                  import_eigenUI("eigen")
                  )
       
      )
  server <- function(input, output, session) {

    Spectrum <- import_eigenServer("eigen", lang_setting = lang_setting)
    output$Data_ok <- renderPrint({
      {
        print(cat("Developer Troubleshoot\n"))
        print(Spectrum$Name)
        print(Spectrum$Destination)
        print(Spectrum$Other)
        Spectrum$Spectrum_raw %>% head()
        }
    })
    }
  shinyApp(ui, server)
}
