
# UI ----------------------------------------------------------------------

import_csv_settingsUI <-
  function(id,
           lang_setting = get("lang_setting", envir = caller_env(n = 1))) {
    tagList(
      h4(lang$ui(36)),
      splitLayout(
        cellWidths = c("30%", "35%", "35%"),
        cellArgs = list(style = "padding: 6px"),
        #Rownumber:
        numericInput(
          NS(id, "row_nr"),
          label = lang$ui(37),
          value = 0,
          min = 0,
          step = 1
        ),
        #Wavelength Column:
        numericInput(
          NS(id, "x_y"),
          label = lang$ui(38),
          min = 1,
          step = 1,
          value = 1
        ),
        #Irradiance Column:
        numericInput(
          NS(id, "x_y2"),
          label = lang$ui(39),
          min = 1,
          step = 1,
          value = 2
        ),
      ),
      splitLayout(
        cellWidths = c("33%", "33%", "33%"),
        cellArgs = list(style = "padding: 6px"),
        #Separator between cells:
        radioButtons(
          NS(id, "separator"),
          label = lang$ui(40),
          choiceNames =  c(",", ";", lang$ui(41)),
          choiceValues = c(",", ";", ""),
          selected = ","
        ),
        #Separator between integers and decimal places:
        radioButtons(
          NS(id, "decimal"),
          label = lang$ui(42),
          choices = c(".", ",", ";"),
          selected = "."
        ),
        column(
          width = 12,
          #Whether there is a header:
          checkboxInput(
            NS(id, "header"),
            label = strong(lang$ui(43)),
            value = TRUE
          ),
          #Possible Multiplicator for Irradiance
          numericInput(
            NS(id, "multiplikator"),
            label = lang$ui(44),
            value = 1,
            min = 0
          )
        )
      ),
      #Set back to defaults
      actionButton(
        NS(id, "Standardeinstellungen"),
        label = lang$ui(45),
        icon("fast-backward", lib = "glyphicon")
        )
      )
  }

# Server ------------------------------------------------------------------

import_csv_settingsServer <- 
  function(id, lang_setting = get("lang_setting", envir = caller_env(n=1)),
           dat0) {
    stopifnot(is.reactive(dat0))
    
  moduleServer(id, function(input, output, session) {

    #Restore defaults
    observe({
      updateNumericInput(session, "row_nr", value = 0)
      updateRadioButtons(session, "separator", selected = ",")
      updateRadioButtons(session, "decimal", selected = ".")
      updateNumericInput(session, "x_y", value = 1)
      updateNumericInput(session, "x_y2", value = 2)
      updateNumericInput(session, "multiplikator", value = 1)
      updateCheckboxInput(session, "header", value = TRUE)
    }) %>% bindEvent(input$Standardeinstellungen)
    
    #Warning messages, if column selections are not ok
    observeEvent(input$x_y, {
      shinyFeedback::feedbackDanger("x_y",
                                    !(input$x_y %>% between(1, ncol(dat0())) &
                                        !is.na(input$x_y)),
                                    paste0(lang$server(5), ncol(dat0())))
    })
    
    observeEvent(input$x_y2, {
      shinyFeedback::feedbackDanger("x_y2",
                                    !(input$x_y2 %>% between(1, ncol(dat0())) &
                                        !is.na(input$x_y2)),
                                    paste0(lang$server(5), ncol(dat0())))
    })
    
    #Warning messages, if multiplier not ok
    observeEvent(input$multiplikator, {
      shinyFeedback::feedbackDanger("multiplikator",
                                    !(input$multiplikator > 0 &
                                        !is.na(input$multiplikator)),
                                    lang$server(6))
    })
    
    #Warning messages, if Row Nr selection is bad or NA
    observeEvent(input$row_nr, {
      shinyFeedback::feedbackDanger("row_nr",
                                    is.na(input$row_nr),
                                    lang$server(6))
    })
    
    reactive({
      list(
      row_nr = (input$row_nr),
      separator = (input$separator),
      decimal = (input$decimal),
      x_y = (input$x_y),
      x_y2 = ifelse(is.na(input$x_y2), 0, input$x_y2),
      multiplikator = (input$multiplikator),
      header = (input$header)
    )
    })
    
  })
}

# App ---------------------------------------------------------------------

# import_csv_settingsApp <- function(lang_setting = "Deutsch") {
#   ui <- fluidPage(
#     import_csv_settingsUI("import")
#   )
#   server <- function(input, output, session) {
#     import_csv_settingsServer("import")
#   }
#   shinyApp(ui, server)
# }