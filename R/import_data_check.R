
# UI ----------------------------------------------------------------------

# Server ------------------------------------------------------------------

import_data_checkServer <-
  function(id,
           dat,
           dat0,
           importfile,
           csv_settings) {
    stopifnot(shiny::is.reactive(dat))
    stopifnot(shiny::is.reactive(dat0))
    stopifnot(shiny::is.reactive(importfile))
    stopifnot(shiny::is.reactive(csv_settings))
    
    shiny::moduleServer(id, function(input, output, session) {
      
      # Notifications for the right data import structure:
      # Data frame requires at least 2 columns
      shiny::observe({
        if(csv_settings()$x_y2 < 1) shiny::removeNotification("belowz")
        
        import_structure_notifier(
          requirement = dat0(),
          test = dim(dat())[2] >= 2,
          text_if = lang$server(7),
          text_else = lang$server(8),
          id = "is_sufficient",
          remove_id = c("is_numeric", "is_integer", "belowz")
        )

      # Data frame requires the wavelength column to be of type integer
        import_structure_notifier(
          requirement = dat0(),
          test = is.integer(dat()[[as.numeric(csv_settings()$x_y)]]) &
            csv_settings()$x_y >= 1,
          text_if = htmltools::p(lang$server(9), lang$server(11)),
          text_else =
            htmltools::p(
              lang$server(9),htmltools::strong(lang$server(10)), lang$server(11)
              ),
          id = "is_integer"
        )

      # Data Frame requires the irradiance column to be of type numeric
        import_structure_notifier(
          requirement = dat0(),
          test = is.numeric(dat()[[csv_settings()$x_y2]]) &
            csv_settings()$x_y2 >= 1,
          text_if = htmltools::p(lang$server(12), lang$server(13)),
          text_else =
            htmltools::p(
              lang$server(12), 
              htmltools::strong(lang$server(10)), 
              lang$server(13)
              ),
          id = "is_numeric"
        )

      # Message about how many numbers are below zero and that those will be
      # replaced with zero.
      
        shiny::req(importfile()
        )
        neg <- sum(try(dat0()[[as.numeric(csv_settings()$x_y2)]]) < 0)
        if (
          shiny::isTruthy(
            try(
              neg > 0 & is.numeric(dat0()[[csv_settings()$x_y2]])
              )
            )
          ) {
          shiny::showNotification(htmltools::HTML(
            paste0(
              htmltools::strong(neg),
              " ",
              lang$server(14),
              htmltools::strong(lang$server(15)),
              lang$server(16)
            )
          ),
          type = "warning",
          duration = NULL,
          id = "belowz")
        }
        else shiny::removeNotification("belowz")
      }) %>% shiny::bindEvent(dat())
      
      #Create a flag on whether the data is ready for analysis
      Daten_da <- shiny::reactiveValues(x = FALSE)
      
      #setting the flag
      shiny::observe({
        shiny::req(dat())
        if(
          all(
            shiny::isTruthy(
              csv_settings()$row_nr >= 0
              ), #Req:sensible number of rows to skip
            dplyr::between(
              csv_settings()$x_y2, 1, ncol(dat())), #Req: sensible Col2
            dplyr::between(
              csv_settings()$x_y, 1, ncol(dat())), #Req: sensible Col2
            dim(dat())[2] >= 2, #Req: Data needs more than one column
            is.integer(dat()[[csv_settings()$x_y]]), #Req: Col 1 is integer
            is.numeric(dat()[[csv_settings()$x_y2]]), #Req: Col 2 is numeric
            csv_settings()$multiplikator > 0, #Req: Multiplicator is > 0
            !is.na(csv_settings()$multiplikator) #Req: Multiplicator is not NA
          ) %>% try() %>% shiny::isTruthy()
        ) {
          Daten_da$x <- TRUE
          shiny::showNotification(
            htmltools::strong(lang$server(131)),
            type = "message",
            duration = NULL,
            id = "success")
        }
        else {
          Daten_da$x <- FALSE
          shiny::showNotification(
            htmltools::strong(lang$server(132)),
            type = "error",
            duration = NULL,
            id = "success")
        }
      })
      
      #Return value
      Daten_da
      
      
    })
  }

# App ---------------------------------------------------------------------
