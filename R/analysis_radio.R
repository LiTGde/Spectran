
# UI ----------------------------------------------------------------------

analysis_radioUI <- function(
    id
    ) {
  
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    shiny::withMathJax(),
    shiny::fluidRow(
      shinydashboard::box( width = 12,
    #Inputs to the page
      shiny::checkboxGroupInput(
        ns("sensitivitaeten"),
        label = lang$ui(95),
        choiceNames = c(
          unname(Specs$Alpha$names),
            paste(lang$ui(89), Specs$Vlambda)
          ),
        choiceValues = unname(Specs$Plot$Names),
        inline = TRUE,
        selected = c(Specs$Plot$Names[c(1,6)])
      ),
      #Outputs
      shiny::plotOutput(ns("plot"), height = "350px"),
      gt::gt_output(ns("table"))
  )))
}

# Server ------------------------------------------------------------------

analysis_radioServer <- 
  function(id, 
           Analysis,
           feed
           ) {
  
  shiny::moduleServer(id, function(input, output, session) {
    
    #checking the sensitivity box, when export demands it
    shiny::observe({
      shiny::updateCheckboxGroupInput(
        session, "sensitivitaeten", selected = 
          if(Analysis$action_spectra) unname(Specs$Plot$Names)
        else NA
        )
    }) %>% shiny::bindEvent(Analysis$action_spectra, ignoreInit = TRUE)
    
    #creating all the plotdata settings
    shiny::observe({
      shiny::req(Analysis$Settings$Spectrum)
      
      Plotdata <- list(
        Spectrum = Analysis$Settings$Spectrum,
        Spectrum_Name = Analysis$Settings$Spectrum_Name,
        maxE = Analysis$Settings$general$Emax[[1]],
        Sensitivity = input$sensitivitaeten,
        subtitle = lang$server(39)
      )
      
      Analysis[[ns_plot(feed)]]$args <- Plotdata
      Analysis[[ns_plot(feed)]]$fun <- "Plot_Main"
    })
    
    #Plot
    output$plot <- shiny::renderPlot({
      shiny::req(Analysis[[ns_plot(feed)]])

      do.call(Analysis[[ns_plot(feed)]]$fun, Analysis[[ns_plot(feed)]]$args)

      } ,
      height = 350,
      width = \() {session$clientData$output_Plotbreite_width}
      )
    shiny::outputOptions(output, 
                         "plot", 
                         suspendWhenHidden = FALSE)

    
    #create an (internal) Table
    shiny::observe({
      shiny::req(Analysis$Settings$Spectrum)
      
      Analysis[[ns_table(feed)]]$internal <- tibble::tribble(
      #columns
      ~Groesse, ~Zeichen, ~Formelzeichen,
      ~Wert, ~Einheit,
      #Irradiance
      lang$server(40), "E<sub>e</sub>", "E_e",
      sum(Analysis$Settings$Spectrum[[2]])*1000, "mW/m\u00b2",
      #peak wavelength
      lang$server(41), "&lambda;<sub>Emax</sub>", "lambda_Emax",
      LambdaMax(Analysis$Settings$Spectrum), "nm",
      #photon flux density
      lang$server(42), "N<sub>P</sub>", "N_P",
      sum(
        PD(Analysis$Settings$Spectrum[[1]],
           Analysis$Settings$Spectrum[[2]])), "photons/(cm\u00b2*s)"
    )
    })
      
    #create the Settings for the output Table
      shiny::observe({
        shiny::req(Analysis[[ns_table(feed)]]$internal)
        
        Analysis[[ns_table(feed)]]$output <-
        list(
          Table = Analysis[[ns_table(feed)]]$internal,
          Spectrum_Name = Analysis$Settings$Spectrum_Name,
          subtitle = lang$server(39),
          Breite = 100,
          cols_scientific = c(3)
          )
        
        Analysis[[ns_table(feed)]]$fun <- "table_rad"
        
    })


    # Table (Output for Radiometry)
    output$table <- gt::render_gt({
      shiny::req(Analysis[[ns_table(feed)]]$output)
      
      do.call(Analysis[[ns_table(feed)]]$fun, Analysis[[ns_table(feed)]]$output)

            })
    shiny::outputOptions(output, "table", suspendWhenHidden = FALSE)

  })
}

# App ---------------------------------------------------------------------
