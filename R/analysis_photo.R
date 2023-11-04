
# UI ----------------------------------------------------------------------

analysis_photoUI <- function(
    id
    ) {
  
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    shiny::withMathJax(),
    shinyjs::useShinyjs(),
    shiny::fluidRow(
      shinydashboard::box( width = 12,
    #Inputs
    shiny::checkboxInput(
      ns("Sensitivity"), 
      label = paste0(lang$ui(96), " ", Specs$Vlambda, lang$ui(97)), 
      width = "100%",
      value = TRUE
    ),
    shiny::checkboxInput(
      ns("Hintergrund"),
      label = lang$ui(168),
      value = TRUE,
      width = "100%"
    ),
    shiny::checkboxInput(
      ns("CIE_grenzen"),
      label = lang$ui(98),
      value = TRUE,
      width = "100%"
    ),
    shiny::checkboxInput(
      ns("Testfarben"),
      label = htmltools::tags$p(lang$ui(99), id=ns("r_label")),
      value = FALSE,
      width = "100%"
    ),
    #Outputs
    shiny::plotOutput(ns("plot"), height = "350px"),
    gt::gt_output(ns("table"))
  )))
}

# Server ------------------------------------------------------------------

analysis_photoServer <- 
  function(id, 
           Analysis,
           feed,
           Name,
           Tabactive
  ) {
    
    shiny::moduleServer(id, function(input, output, session) {
      
      shiny::observe({
        Analysis$Tabactive <- Tabactive()
      })
      
      #checking the sensitivity box, when export demands it
      shiny::observe({
          shiny::updateCheckboxInput(
            session, "Sensitivity", value = Analysis$action_spectra)
      }) %>% shiny::bindEvent(Analysis$action_spectra)
      
      #Warning message, if no color-rendering index can be shown
      shiny::observe({
        shiny::req(Analysis$Settings,
                   cS$cS)
        
        input$CIE_grenzen
        input$Testfarben
        checkbox_update("Testfarben",
                        sum(cS$CRI == 0))
      })
      
      #package is needed for the S3 object to work properly
      withr::local_package("colorSpec")
      
      #Color Rendering preparation
      CRI <- tibble::tibble(Testfarbe = paste0("R", 1:14),
                            CRI = 0) %>% 
        dplyr::mutate(Testfarbe = factor(Testfarbe, levels = rev(Testfarbe)))
      
      #ColorSpec Object
      cS <- shiny::reactiveValues(cs = NULL, CRI = CRI)
      
      shiny::observe({
        shiny::req(Analysis$Settings$Spectrum)
        cS$cS <- cS_object(Analysis$Settings$Spectrum)
      })
      

      #Collect Plotsettings
      shiny::observe({
        shiny::req(Analysis$Settings$Spectrum,
                   cS$cS)
        
        Plotdata <- list(
          Spectrum = Analysis$Settings$Spectrum,
          Spectrum_Name = Analysis$Settings$Spectrum_Name,
          maxE = Analysis$Settings$general$Emax[[1]],
          Sensitivity = ifelse(input$Sensitivity, Name, NA),
          Sensitivity_Spectrum = Analysis$Settings$general$Ewtd[[6]],
          subtitle = lang$server(53),
          alpha = ifelse(input$Hintergrund, 0.85, 0),
          # font_size = 15,
          Second_plot = input$Testfarben,
          Name = NULL,
          CRI = cS$CRI
          
        )
        
        Analysis[[ns_plot(feed)]]$args <- Plotdata
        Analysis[[ns_plot(feed)]]$fun <- "Plot_Combi"
        
      })
      
      #Plot generation
      output$plot <- shiny::renderPlot({
        shiny::req(Analysis[[ns_plot(feed)]])
        
        do.call(Analysis[[ns_plot(feed)]]$fun, Analysis[[ns_plot(feed)]]$args)

      } ,height = 350,
      width = \() {session$clientData$output_Plotbreite_width}
      )
        shiny::outputOptions(
          output, 
          "plot", 
          suspendWhenHidden =  FALSE
          )
      
      #create an (internal) Table
      shiny::observe({
        shiny::req(Analysis$Settings$Spectrum,
                   cS$cS)
        
        CRI_val <- cS$cS %>% CRI(input$CIE_grenzen)

        Analysis[[ns_table(feed)]]$internal <- tibble::tribble(
          #columns
          ~Groesse, ~Zeichen, ~Formelzeichen,
          ~Wert, ~Einheit,
          #Illuminance
          lang$server(54), "E<sub>v</sub>", "E_v",
          Analysis$Settings$general$Ev[Analysis$Settings$general$Names == Name], 
          "lx",
          #photometric irradiance
          lang$server(55),"E<sub>e,v</sub>", "E_e,v",
          Analysis$Settings$general$E[Analysis$Settings$general$Names == Name]*
            1000, "mW/m\u00b2",
          #photometric to complete visual irradiance
          lang$server(56),"E<sub>e,v</sub>/E<sub>e</sub>", "E_e,v/E_e",
          Analysis$Settings$general$E[Analysis$Settings$general$Names == Name]/
            sum(Analysis$Settings$Spectrum[[2]]),
          "",
          #CCT
          lang$server(57),"CCT", "CCT",
          colorSpec::computeCCT(cS$cS, strict = input$CIE_grenzen),
          "K",
          # CRI
          lang$server(52),lang$server(137), lang$server(138), CRI_val, ""
        )

        #fill in the CRI values from R1 to R14
        if(!is.na(CRI_val)) {
          cS$CRI$CRI <-
            attr(CRI_val, "data")$table4$CRI
        }
        else {cS$CRI$CRI <- 0}
      })

      #create the Settings for the output Table
      shiny::observe({
        shiny::req(Analysis[[ns_table(feed)]]$internal)
        
        Analysis[[ns_table(feed)]]$output <-
          list(
            Table = Analysis[[ns_table(feed)]]$internal,
            Spectrum_Name = Analysis$Settings$Spectrum_Name,
            subtitle = lang$server(63),
            Breite = 100,
            CIE_grenzen = input$CIE_grenzen
          )
        
        Analysis[[ns_table(feed)]]$fun <- "table_phot"
        
      })
      
      # Table (Output for Radiometry)
      output$table <- gt::render_gt({
        shiny::req(Analysis[[ns_table(feed)]]$output)
        
        do.call(Analysis[[ns_table(feed)]]$fun, 
                Analysis[[ns_table(feed)]]$output)
        })
      shiny::outputOptions(output, "table", suspendWhenHidden = FALSE)

    })
  }

# App ---------------------------------------------------------------------
