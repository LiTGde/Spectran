
# UI ----------------------------------------------------------------------

analysis_radioUI <- function(
    id, lang_setting = get("lang_setting", envir = rlang::caller_env(n = 1))) {
  
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    shiny::withMathJax(),
      shiny::checkboxGroupInput(
        ns("sensitivitaeten"),
        label = lang$ui(95),
        choiceNames = c(
          unname(Specs$Alpha$names),
            paste(lang$ui(89), Specs$Vlambda)
          ),
        choiceValues = unname(Specs$Plot$Names),
        inline = TRUE
      ),
      #Übersichtsplot des Spektrums und Tabelle
      shiny::plotOutput(ns("plot"), height = "350px"),
      gt::gt_output(ns("table"))
  )
}

# Server ------------------------------------------------------------------

analysis_radioServer <- 
  function(id, 
           lang_setting = get("lang_setting", 
                              envir = rlang::caller_env(n = 1)
                              ),
           Spectrum = NULL
           ) {
  
  shiny::moduleServer(id, function(input, output, session) {
    
    #Plot
    output$plot <- shiny::renderPlot({
      shiny::req(Spectrum$Spectrum,
          Spectrum$Destination == lang$ui(69))
      
      Plot(Spectrum = Spectrum, 
           Sensitivity = input$sensitivitaeten, 
           subtitle = lang$server(39),
           lang_setting)
      
      } ,height = 350)
    shiny::outputOptions(output, "plot", suspendWhenHidden = FALSE)

    #create an (internal) Table
    shiny::observe({
      shiny::req(Spectrum$Spectrum,
                 Spectrum$Destination == lang$ui(69))
      Spectrum$radiometric <- tibble::tribble(
      #columns
      ~Groesse, ~Zeichen, ~Formelzeichen, 
      ~Wert, ~Einheit,
      #Irradiance
      lang$server(40), "E<sub>e</sub>", "E_e", 
      sum(Spectrum$Spectrum$Bestrahlungsstaerke*1000), "mW/m\u00b2",
      #peak wavelength
      lang$server(41), "&lambda;<sub>Emax</sub>", "lambda_Emax", 
      LambdaMax(Spectrum$Spectrum), "nm",
      #photon flux density
      lang$server(42), "N<sub>P</sub>", "N_P", 
      sum(PD(Spectrum$Spectrum[[1]], Spectrum$Spectrum[[2]])), "quanta/(cm\u00b2*s)"
    )
    })

    #Creates a Table for Radiometry
    table_rad <- function(...){
      create_table(...) %>% 
        gt::tab_footnote(
          footnote = lang$server(44),
          locations = gt::cells_stub(rows = lang$server(40))
        ) %>%
        gt::tab_footnote(
          footnote = lang$server(45),
          locations = gt::cells_stub(rows = lang$server(41))
        ) %>%
        gt::tab_footnote(
          footnote = lang$server(46),
          locations = gt::cells_stub(rows = lang$server(42))
        ) %>%
        gt::tab_source_note(
          source_note = lang$server(47)
        )
    }

    # Table (Output for Radiometry)
    output$table <- gt::render_gt({
      shiny::req(Spectrum$radiometric)
      table_rad(Spectrum = Spectrum, 
                Table = "radiometric",
                subtitle = lang$server(39),
                Breite = 100,
                cols_scientific = c(3),
                lang_setting = lang_setting)
    }, height = 300)
    shiny::outputOptions(output, "table", suspendWhenHidden = FALSE)

  })
}

# App ---------------------------------------------------------------------
