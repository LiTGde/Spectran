
# UI ----------------------------------------------------------------------

import_visual_checkUI <-
  function(id, lang_setting = get("lang_setting", envir = caller_env(n = 1))) {
    tagList(
      uiOutput(NS(id,"inputuebersicht"))
    )
  }

# Server ------------------------------------------------------------------

import_visual_checkServer <-
  function(id,
           lang_setting = get("lang_setting", envir = caller_env(n = 1)),
           dat,
           csv_settings
           ) {
    stopifnot(is.reactive(dat))
    stopifnot(is.reactive(csv_settings))
    
    moduleServer(id, function(input, output, session) {
      
      #Plotoverview of the data
      output$Uebersichtsplot <- renderPlot({
        validate(
          need(between(csv_settings()$x_y2, 1, ncol(dat())), lang$server(17)),
          need(between(csv_settings()$x_y, 1, ncol(dat())), lang$server(18))
        )
        ggplot(
          data = dat(),
          aes(
            .data[[names(dat())[csv_settings()$x_y]]],
            .data[[names(dat())[csv_settings()$x_y2]]]
          )) +
          geom_area(fill = "grey")+
          geom_point(size = 0.5)+
          # geom_path(size = 0.1)+
          theme_cowplot()
      })

      #Table overview of the data
      output$Spektraldaten <- render_gt({
        validate(
          need(dat(), lang$server(19))
        )

        dat() %>% 
        # head(n=8) %>%
          dplyr::mutate(rows = 1:nrow(.)) %>%
          gt(rowname_col = "rows") %>% 
          # tab_header(subtitle = lang$server(20)) %>% 
          opt_interactive(page_size_default = 8)
      })
      
      # UI generation for the overview
      output$inputuebersicht <- renderUI({
        req(dat())
        ns <- session$ns
        fluidPage(
          tags$head(
            tags$style(
              #Change the Validation message for the plot:
              HTML(
                "
                .shiny-output-error-validation {
                color: #ff0000;
              font-weight: bold;
              }
              "
              )
            )
          ),
          column(width = 6,
                 plotOutput(ns("Uebersichtsplot"))
          ),
          column(width = 6,
                 gt_output(ns("Spektraldaten"))
          )
        )
        })
    })
    }

# App ---------------------------------------------------------------------

# import_visual_checkApp <- function(lang_setting = "Deutsch") {
#   ui <- fluidPage(
#     import_data_checkUI("import")
#   )
#   server <- function(input, output, session) {
#     import_data_checkServer("import")
#   }
#   shinyApp(ui, server)
# }