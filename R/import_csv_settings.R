
# UI ----------------------------------------------------------------------

import_csv_settingsUI <-
  function(id) {
    htmltools::tagList(
      htmltools::h4(lang$ui(36)),
      shiny::splitLayout(
        cellWidths = c("30%", "35%", "35%"),
        cellArgs = list(style = "padding: 6px"),
        #Rownumber:
        shiny::numericInput(
          shiny::NS(id, "row_nr"),
          label = lang$ui(37),
          value = 0,
          min = 0,
          step = 1
        ),
        #Wavelength Column:
        shiny::numericInput(
          shiny::NS(id, "x_y"),
          label = lang$ui(38),
          min = 1,
          step = 1,
          value = 1
        ),
        #Irradiance Column:
        shiny::numericInput(
          shiny::NS(id, "x_y2"),
          label = lang$ui(39),
          min = 1,
          step = 1,
          value = 2
        ),
      ),
      shiny::splitLayout(
        cellWidths = c("33%", "33%", "33%"),
        cellArgs = list(style = "padding: 6px"),
        #Separator between cells:
        shiny::radioButtons(
          shiny::NS(id, "separator"),
          label = lang$ui(40),
          choiceNames =  c(",", ";", lang$ui(41)),
          choiceValues = c(",", ";", ""),
          selected = ","
        ),
        #Separator between integers and decimal places:
        shiny::radioButtons(
          shiny::NS(id, "decimal"),
          label = lang$ui(42),
          choices = c(".", ",", ";"),
          selected = "."
        ),
        shiny::column(
          width = 12,
          #Whether there is a header:
          shiny::checkboxInput(
            shiny::NS(id, "header"),
            label = htmltools::strong(lang$ui(43)),
            value = TRUE
          ),
          #Scaling of the data
          shiny::selectInput(
            shiny::NS(id, "scaling"),
            label = lang$ui(178),
            choices = c("(W/m\u00b2*nm)" = "a", 
                        "(mW/m\u00b2*nm)" = "b",
                        lang$server(139)),
            selected = "none"
          ),
          #Possible Multiplicator for Irradiance
          shiny::numericInput(
            shiny::NS(id, "multiplikator"),
            label = lang$ui(44),
            value = 1,
            min = 0
          )
        )
      ),
      #Set back to defaults
      shiny::actionButton(
        shiny::NS(id, "Standardeinstellungen"),
        label = lang$ui(45),
        shiny::icon("fast-backward", lib = "glyphicon")
        )
      )
  }

# Server ------------------------------------------------------------------

import_csv_settingsServer <- 
  function(id,
           dat0) {
    stopifnot(shiny::is.reactive(dat0))
    
    shiny::moduleServer(id, function(input, output, session) {

    #Restore defaults
      shiny::observe({
        shiny::updateNumericInput(session, "row_nr", value = 0)
        shiny::updateRadioButtons(session, "separator", selected = ",")
        shiny::updateRadioButtons(session, "decimal", selected = ".")
        shiny::updateNumericInput(session, "x_y", value = 1)
        shiny::updateNumericInput(session, "x_y2", value = 2)
        shiny::updateNumericInput(session, "multiplikator", value = 1)
        shiny::updateCheckboxInput(session, "header", value = TRUE)
    }) %>% shiny::bindEvent(input$Standardeinstellungen)
    
    #Change the Multiplicator based on setting
      shiny::observeEvent(input$scaling, {
        if(input$scaling == "a") {
          shiny::updateNumericInput(session, "multiplikator", value = 1)
        } else if(input$scaling == "b") {
          shiny::updateNumericInput(session, "multiplikator", value = 1000)
        }
      })
      
    #Change the scaling based on the multiplicator
      shiny::observeEvent(input$multiplikator, {
        if(input$multiplikator == 1) {
          shiny::updateSelectInput(session, "scaling", selected = "a")
        } else if(input$multiplikator == 1000) {
          shiny::updateSelectInput(session, "scaling", selected = "b")
        } else {
          shiny::updateSelectInput(session, "scaling", 
                                   selected = lang$server(139))
        }
      })
      
    #Warning messages, if column selections are not ok
      shiny::observeEvent(input$x_y, {
      shinyFeedback::feedbackDanger("x_y",
                                    !(input$x_y %>% 
                                        dplyr::between(1, ncol(dat0())) &
                                        !is.na(input$x_y)),
                                    paste0(lang$server(5), ncol(dat0())))
    })
    
      shiny::observeEvent(input$x_y2, {
      shinyFeedback::feedbackDanger("x_y2",
                                    !(input$x_y2 %>% 
                                        dplyr::between(1, ncol(dat0())) &
                                        !is.na(input$x_y2)),
                                    paste0(lang$server(5), ncol(dat0())))
    })
    
    
      
    #Warning messages, if multiplier not ok
      shiny::observeEvent(input$multiplikator, {
      shinyFeedback::feedbackDanger("multiplikator",
                                    !(input$multiplikator > 0 &
                                        !is.na(input$multiplikator)),
                                    lang$server(6))
    })
    
    #Warning messages, if Row Nr selection is bad or NA
      shiny::observeEvent(input$row_nr, {
      shinyFeedback::feedbackDanger("row_nr",
                                    is.na(input$row_nr),
                                    lang$server(6))
    })
    
      shiny::reactive({
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
