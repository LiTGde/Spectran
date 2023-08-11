
# UI ----------------------------------------------------------------------

import_visual_checkUI <-
  function(id) {
    htmltools::tagList(
      htmltools::tags$head(
        htmltools::tags$style(
          #Change the Validation message for the plot:
          htmltools::HTML(
            "
                .shiny-output-error-validation {
                color: #ff0000;
              font-weight: bold;
              }
              "
          )
        )
      ),
      shiny::uiOutput(shiny::NS(id,"inputuebersicht"))
    )
  }

# Server ------------------------------------------------------------------

import_visual_checkServer <-
  function(id,
           dat,
           csv_settings,
           importfile,
           dat0
           ) {
    stopifnot(shiny::is.reactive(dat))
    stopifnot(shiny::is.reactive(csv_settings))
    
    shiny::moduleServer(id, function(input, output, session) {
      
      #Plotoverview of the data
      output$Uebersichtsplot <- shiny::renderPlot({
        shiny::validate(
          shiny::need(
            dplyr::between(
              csv_settings()$x_y2, 1, ncol(dat())), lang$server(17)
          ),
          shiny::need(
            dplyr::between(
              csv_settings()$x_y2, 1, ncol(dat())), lang$server(17)
            ),
          shiny::need(
            dplyr::between(
              csv_settings()$x_y, 1, ncol(dat())
              ), 
            lang$server(18)
            )
        )
        ggplot2::ggplot(
          data = dat(),
          ggplot2::aes(
            .data[[names(dat())[csv_settings()$x_y]]],
            .data[[names(dat())[csv_settings()$x_y2]]]
          )) +
          ggplot2::geom_area(fill = "grey")+
          ggplot2::geom_point(size = 0.5)+
          # geom_path(size = 0.1)+
          cowplot::theme_cowplot()
      })

      #Table overview of the data
      output$Spektraldaten <- gt::render_gt({

        dat() %>% 
        # head(n=8) %>%
          dplyr::mutate(rows = 1:nrow(.)) %>%
          gt::gt(rowname_col = "rows") %>% 
          # tab_header(subtitle = lang$server(20)) %>% 
          gt::opt_interactive(page_size_default = 8)
      })
      
      # UI generation for the overview
      output$inputuebersicht <- shiny::renderUI({
        shiny::req(importfile())
        shiny::validate(
          shiny::need(dat0(), lang$server(19))
        )
        ns <- session$ns
        shiny::fluidPage(
          shiny::column(width = 6,
                 shiny::plotOutput(ns("Uebersichtsplot"))
          ),
          shiny::column(width = 6,
                 gt::gt_output(ns("Spektraldaten"))
          )
        )
        })
    })
    }

# App ---------------------------------------------------------------------
