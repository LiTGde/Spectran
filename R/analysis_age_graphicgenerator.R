
# UI ----------------------------------------------------------------------

# Server ------------------------------------------------------------------

analysis_age_graphicgeneratorServer <- 
  function(id, 
           Analysis,
           Alter_mel,
           Hintergrund,
           plot_multiplier,
           Alter_inset,
           Alter_rel,
           Alter
           ) 
    {
    
    shiny::moduleServer(id, function(input, output, session) {
      
      #creates a Table for the transmission axis
       shiny::observe({
        shiny::req(Analysis$Settings$general)
         age_scale <-
           tibble::tibble(
             x = 380,
             y = Analysis$Settings$general$Emax[[1]] *
               c(0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2),
             label = 
               c("25%", "50%", "75%", "100%", "125%", "150%", "175%", "200%")
           )
         
         Analysis$plot_Age$age_scale <- age_scale
      })
      
      # Collect Plotsettings
      
      # Pupil
      shiny::observe({
        shiny::req(Analysis$Settings$Spectrum)

        Plotdata <- list(
          Age = Alter())
        Analysis[[ns_plot(lang$server(127))]]$args <- Plotdata
        Analysis[[ns_plot(lang$server(127))]]$fun <- "Plot_age_pup"
      })
      
      # Transmission
      shiny::observe({
        shiny::req(Analysis$Settings$Spectrum)

        Plotdata <- list(
          Spectrum = Analysis$Settings$Spectrum,
          Spectrum_Name = Analysis$Settings$Spectrum_Name,
          maxE = Analysis$Settings$general$Emax[[1]],
          plot_multiplier = plot_multiplier(),
          subtitle = paste0(lang$server(95), Alter(), lang$server(96)),
          Age = Alter(),
          Spectrum_mel_wtd = Analysis$Settings$general$Ewtd[[1]],
          Alter_mel = Alter_mel(),
          age_scale = Analysis$plot_Age$age_scale,
          Alter_inset = Alter_inset(),
          Alter_rel = Alter_rel(),
          alpha = ifelse(Hintergrund(), 0.85, 0)
        )
        
        Analysis[[ns_plot(lang$server(128))]]$args <- Plotdata
        Analysis[[ns_plot(lang$server(128))]]$fun <- "Plot_age_trans"
      })
      
      # Summary
      shiny::observe({
        shiny::req(Analysis$Settings$Spectrum)

        Plotdata <- list(
          Spectrum = Analysis$Settings$Spectrum,
          Spectrum_Name = Analysis$Settings$Spectrum_Name,
          maxE = Analysis$Settings$general$Emax[[1]],
          plot_multiplier = plot_multiplier(),
          subtitle = paste0(lang$server(95), Alter(), lang$server(96)),
          Age = Alter(),
          Spectrum_mel_wtd = Analysis$Settings$general$Ewtd[[1]],
          Alter_mel = Alter_mel(),
          alpha = ifelse(Hintergrund(), 0.85, 0)
        )
        Analysis[[ns_plot(lang$server(125))]]$args <- Plotdata
        Analysis[[ns_plot(lang$server(125))]]$fun <- "Plot_age_tot"
      })
      
      #Create a table for all the pupil output
      shiny::observe({
        shiny::req(Analysis$Settings$general)
        
        #calculate the correction factors
        Basis <- Analysis$Settings$general[1,]
        k_pup <- k_pup_fun(Alter())
        k_trans <- k_trans_fun(Alter(),Basis[["Ewtd"]][[1]])
        k_mel <- k_pup * k_trans
        
        #create the table
        Table <-
          tibble::tibble(
            Groesse = c(
              lang$server(75),
              lang$server(76),
              lang$server(77),
              lang$server(78),
              paste0(lang$server(79), Alter() ,lang$server(80))
            ),
            Zeichen = c(
              paste0(
                "E", htmltools::tags$sub("v,mel,D65"),"(32)"),
              paste0("k", htmltools::tags$sub("pupil"), "(", Alter(), ")"),
              paste0("k<sub>mel,trans</sub>(", Alter(), ")"),
              paste0("k<sub>mel</sub>(", Alter(), ")"),
              paste0("E<sub>v,mel,D65</sub>(", Alter(), ")")
            ),
            Formelzeichen = c(
              "E_v,mel,D65_(32)",
              paste0("k_pupil_(", Alter(), ")"),
              paste0("k_mel,trans_(", Alter(), ")"),
              paste0("k_mel_(", Alter(), ")"),
              paste0("E_v,mel,D65_(", Alter(), ")")
            ),
            Wert = c(Basis[["Ev"]],
                     k_pup,
                     k_trans,
                     k_mel,
                     Basis[["Ev"]]*k_mel),
            Einheit = c("lux", "", "", "", "lux")
          )
        
        Analysis$table_Age$internal <- Table
        
      })
      
      #create the Settings for the output Table pupil
      shiny::observe({
        shiny::req(Analysis$table_Age$internal)

        Analysis[[ns_table(lang$server(127))]]$output <-
          list(Table = Analysis$table_Age$internal,
               Spectrum_Name = Analysis$Settings$Spectrum_Name,
               subtitle = lang$server(90),
               Breite = 100,
               slice = 2)

        Analysis[[ns_table(lang$server(127))]]$fun <- "table_age"
      })
      
      # create the Settings for the output Table Transmission
      shiny::observe({
        shiny::req(Analysis$table_Age$internal)

        Analysis[[ns_table(lang$server(128))]]$output <-
          list(Table = Analysis$table_Age$internal,
               Spectrum_Name = Analysis$Settings$Spectrum_Name,
               subtitle = lang$server(91),
               Breite = 100,
               slice = 3)

        Analysis[[ns_table(lang$server(128))]]$fun <- "table_age"
      })
      
      #create the Settings for the output Table Summary
      shiny::observe({
        shiny::req(Analysis$table_Age$internal)

        Analysis[[ns_table(lang$server(125))]]$output <-
          list(Table = Analysis$table_Age$internal,
               Spectrum_Name = Analysis$Settings$Spectrum_Name,
               subtitle = lang$server(98),
               Breite = 100,
               slice = 1:5)

        Analysis[[ns_table(lang$server(125))]]$fun <- "table_age"
      })
      
    })
  }

# App ---------------------------------------------------------------------
