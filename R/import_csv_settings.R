# UI ----------------------------------------------------------------------

import_csv_settingsUI <- function(id) {
  ns <- shiny::NS(id)
  labels <- list(
    heading = lang$ui(36),
    skip = lang$ui(37),
    wavelength = lang$ui(38),
    value = lang$ui(39),
    separator = lang$ui(40),
    whitespace = lang$ui(41),
    decimal = lang$ui(42),
    header = lang$ui(43),
    reset = lang$ui(45),
    invalid_columns = lang$server(5)
  )

  scaling_controls <- htmltools::tagList(
    shiny::selectInput(
      ns("scaling"),
      label = lang$ui(178),
      choices = c(
        "(W/m\u00b2*nm)" = "a",
        "(mW/m\u00b2*nm)" = "b",
        lang$server(139)
      ),
      selected = "none"
    ),
    shiny::numericInput(
      ns("multiplikator"),
      label = lang$ui(44),
      value = 1,
      min = 0
    )
  )

  spectral_csv_settingsUI(
    ns("core"),
    labels = labels,
    extra_controls = scaling_controls
  )
}

# Server ------------------------------------------------------------------

import_csv_settingsServer <- function(id, dat0) {
  stopifnot(shiny::is.reactive(dat0))

  shiny::moduleServer(id, function(input, output, session) {
    labels <- list(
      heading = lang$ui(36),
      skip = lang$ui(37),
      wavelength = lang$ui(38),
      value = lang$ui(39),
      separator = lang$ui(40),
      whitespace = lang$ui(41),
      decimal = lang$ui(42),
      header = lang$ui(43),
      reset = lang$ui(45),
      invalid_columns = lang$server(5)
    )

    core_settings <- spectral_csv_settingsServer(
      "core",
      data = dat0,
      labels = labels
    )

    shiny::observeEvent(
      core_settings()$reset,
      {
        shiny::updateNumericInput(session, "multiplikator", value = 1)
        shiny::updateSelectInput(session, "scaling", selected = "a")
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(input$scaling, {
      shiny::req(input$scaling)
      if (input$scaling == "a") {
        shiny::updateNumericInput(session, "multiplikator", value = 1)
      } else if (input$scaling == "b") {
        shiny::updateNumericInput(session, "multiplikator", value = 1000)
      }
    })

    shiny::observeEvent(input$multiplikator, {
      shiny::req(input$multiplikator)
      if (input$multiplikator == 1) {
        shiny::updateSelectInput(session, "scaling", selected = "a")
      } else if (input$multiplikator == 1000) {
        shiny::updateSelectInput(session, "scaling", selected = "b")
      } else {
        shiny::updateSelectInput(
          session,
          "scaling",
          selected = lang$server(139)
        )
      }
    })

    shiny::observeEvent(input$multiplikator, {
      shinyFeedback::feedbackDanger(
        "multiplikator",
        !(input$multiplikator > 0 && !is.na(input$multiplikator)),
        lang$server(6)
      )
    })

    shiny::reactive({
      core <- core_settings()
      list(
        row_nr = core$skip,
        separator = core$delimiter,
        decimal = core$decimal_mark,
        x_y = core$wavelength_column,
        x_y2 = core$value_column,
        multiplikator = input$multiplikator,
        header = core$header
      )
    })
  })
}

# App ---------------------------------------------------------------------
