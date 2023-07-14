
# UI ----------------------------------------------------------------------

analysis_alphaUI <- function(
    id
    ) {

  ns <- shiny::NS(id)
  
  alpha_list <- list(Name = Specs$Alpha$names,
                     id = ns(Specs$Alpha$names))
  alpha_list <- alpha_list %>% purrr::pmap(analysis_alpha2UI)
  alpha_list[["id"]] <- ns("alpha")
  
  htmltools::tagList(
    shiny::fluidRow(
      shinydashboard::box( width = 12,
    #Inputs
    shiny::checkboxInput(
      ns("Sensitivity"), 
      label = lang$ui(95), 
      width = "100%",
      value = TRUE),
    shiny::checkboxInput(ns("Vergleich"), label = lang$ui(100), width = "100%"),
    shiny::checkboxInput(
      ns("Hintergrund"),
      label = lang$ui(168),
      value = TRUE,
      width = "100%"
    ),
    #Tabset with individual sets
    do.call(shiny::tabsetPanel, alpha_list)
    )
    )
  )
}

analysis_alpha2UI <- function(
    id, 
    Name) {
  
  ns <- shiny::NS(id)
  #Outputs
  shiny::tabPanel(
    title = Name,
    shiny::plotOutput(ns("plot"), height = "350px"),
    gt::gt_output(ns("table"))
  )
  
}

# Server ------------------------------------------------------------------

analysis_alphaServer <- 
  function(id,
           Analysis,
           Tabactive
  ) {
    
    shiny::moduleServer(id, function(input, output, session) {
      
      #checking the sensitivity box, when export demands it
      shiny::observe({
        shiny::updateCheckboxInput(
          session, "Sensitivity", value = Analysis$action_spectra)
      }) %>% shiny::bindEvent(Analysis$action_spectra, ignoreInit = TRUE)
      
      #create the module servers
      purrr::map(1:5, \(i) {
      analysis_alpha2Server(id = Specs$Alpha$names[[i]],
                            Analysis = Analysis,
                            feed = Specs$Alpha$names[[i]],
                            Name = Specs$Alpha$names[[i]],
                            index = i,
                            Sensitivity = shiny::reactive(input$Sensitivity),
                            Vergleich = shiny::reactive(input$Vergleich),
                            Hintergrund = shiny::reactive(input$Hintergrund),
                            Tabactive)
      })
      
      #create an alpha table
      shiny::observe({
        shiny::req(Analysis$Settings$Spectrum)
        Analysis[[ns_table(lang$server(126))]]$internal <- table_alpha(Analysis)
        
      })
      
      
    })
  }

analysis_alpha2Server <- 
  function(id, 
           Analysis,
           feed,
           Name,
           index,
           Sensitivity,
           Vergleich,
           Hintergrund,
           Tabactive
  ) {
    
    shiny::moduleServer(id, function(input, output, session) {
      
# Collect Plotsettings
      shiny::observe({
        shiny::req(Analysis$Settings$Spectrum)
        
        Plotdata <- list(
          Spectrum = Analysis$Settings$Spectrum,
          Spectrum_Name = Analysis$Settings$Spectrum_Name,
          maxE = Analysis$Settings$general$Emax[[1]],
          Sensitivity = ifelse(Sensitivity(), Name, NA),
          Sensitivity_Spectrum = Analysis$Settings$general$Ewtd[[index]],
          Sensitivity_Overview = Analysis$Settings$general,
          subtitle = Specs$Alpha$descriptions[[the$language]][[index]],
          alpha = ifelse(Hintergrund(), 0.85, 0),
          Second_plot = Vergleich(),
          Name = Name
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
      # shiny::observe({
        shiny::outputOptions(
          output, 
          "plot", 
          suspendWhenHidden = FALSE
          # suspendWhenHidden = if(Tabactive() == "analysis") FALSE else TRUE
        )
      # })

      #create an (internal) Table
      shiny::observe({
        shiny::req(Analysis$Settings$Spectrum)
        
        adjective <- Specs$Alpha$adjectives[[the$language]][[index]]
        Werte <- Analysis$Settings$general %>% dplyr::filter(Names %in% Name)

        Analysis[[ns_table(feed)]]$internal <- tibble::tribble(
          #columns
          ~Groesse, ~Zeichen, ~Formelzeichen,
          ~Wert, ~Einheit,
          #alphaopic illuminance
          paste0(adjective, lang$server(64),Specs$Alpha$abb[[index]] ,"EDI)"),
          paste0("E<sub>v,", Werte$Abbr,",D65</sub>"),
          paste0("E_v,",Werte$Abbr, "D65"), Werte$Ev[1], "lux",
          #alphaopic irradiance
          paste0(adjective, lang$server(65)),
          paste0("E<sub>e,", Werte$Abbr, "</sub>"),
          paste0("E_e,",Werte$Abbr), Werte$E[1]*1000, "mW/m\u00b2",
          # #alphaopic action factor
          paste0(adjective, lang$server(66)),
          paste0("a<sub>", Werte$Abbr,",v</sub>"),
          paste0("a_",Werte$Abbr), 
          Werte$E[1]/Analysis$Settings$general$E[[6]], "",
          # #alphaopic Daylight efficacy ratio
          paste0(adjective, lang$server(67)),
          paste0("&gamma;<sub>",Werte$Abbr ,",v,D65</sub>"),
          paste0("y_",Werte$Abbr, ",v,D65"),
          Werte$Ev[1]/Analysis$Settings$general$Ev[[6]], ""
        )

      })

      #create the Settings for the output Table
      shiny::observe({
        shiny::req(Analysis[[ns_table(feed)]]$internal)
        
        Analysis[[ns_table(feed)]]$output <-
          list(Table = Analysis[[ns_table(feed)]]$internal,
               Spectrum_Name = Analysis$Settings$Spectrum_Name,
               subtitle = Specs$Alpha$descriptions[[the$language]][[index]],
               Breite = 100,
               index = index)
        
        Analysis[[ns_table(feed)]]$fun <- "table_alph"
      })
      
      # Table (Output for Radiometry)
      output$table <- gt::render_gt({
        shiny::req(Analysis[[ns_table(feed)]])
        
        do.call(Analysis[[ns_table(feed)]]$fun, 
                Analysis[[ns_table(feed)]]$output)
        
      })
      shiny::outputOptions(output, "table", suspendWhenHidden = FALSE)
      
    })
  }

# App ---------------------------------------------------------------------
