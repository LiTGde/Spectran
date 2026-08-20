# Shared CSV import helpers -------------------------------------------------

#' Labels for the shared spectral CSV settings module
#'
#' @param value_label Label for the spectral value column.
#'
#' @return A named list of labels used by `spectral_csv_settingsUI()`.
#' @noRd
spectral_csv_settings_labels <- function(value_label = "Value column") {
  list(
    heading = "CSV settings",
    skip = "Rows to skip",
    wavelength = "Wavelength column",
    value = value_label,
    separator = "Column separator",
    whitespace = "Whitespace",
    decimal = "Decimal mark",
    decimal_choices = NULL,
    header = "First row contains column names",
    reset = "Reset settings",
    invalid_columns = "Select two different columns that exist in the file."
  )
}

#' Shared spectral CSV settings controls
#'
#' @param id Shiny module identifier.
#' @param labels Named label list from `spectral_csv_settings_labels()`.
#' @param extra_controls Optional controls rendered below the header setting.
#'
#' @return Shiny UI tags.
#' @noRd
spectral_csv_settingsUI <- function(
  id,
  labels = spectral_csv_settings_labels(),
  extra_controls = NULL
) {
  ns <- shiny::NS(id)
  decimal_choices <- labels$decimal_choices
  if (is.null(decimal_choices)) {
    decimal_choices <- c("." = ".", "," = ",", ";" = ";")
  }

  htmltools::tagList(
    htmltools::h4(labels$heading),
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::numericInput(
          ns("skip"),
          label = labels$skip,
          value = 0,
          min = 0,
          step = 1,
          width = "100%"
        )
      ),
      shiny::column(
        width = 4,
        shiny::numericInput(
          ns("wavelength_column"),
          label = labels$wavelength,
          value = 1,
          min = 1,
          step = 1,
          width = "100%"
        )
      ),
      shiny::column(
        width = 4,
        shiny::numericInput(
          ns("value_column"),
          label = labels$value,
          value = 2,
          min = 1,
          step = 1,
          width = "100%"
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::radioButtons(
          ns("delimiter"),
          label = labels$separator,
          choiceNames = c(",", ";", labels$whitespace),
          choiceValues = c(",", ";", ""),
          selected = ","
        )
      ),
      shiny::column(
        width = 4,
        shiny::radioButtons(
          ns("decimal_mark"),
          label = labels$decimal,
          choices = decimal_choices,
          selected = "."
        )
      ),
      shiny::column(
        width = 4,
        shiny::checkboxInput(
          ns("header"),
          label = htmltools::strong(labels$header),
          value = TRUE
        ),
        extra_controls
      )
    ),
    shiny::uiOutput(ns("validation")),
    shiny::actionButton(
      ns("reset"),
      label = labels$reset,
      icon = shiny::icon("backward-fast")
    )
  )
}

#' Server for shared spectral CSV settings
#'
#' @param id Shiny module identifier.
#' @param data Reactive containing the parsed CSV data.
#' @param labels Named label list from `spectral_csv_settings_labels()`.
#'
#' @return A reactive settings list.
#' @noRd
spectral_csv_settingsServer <- function(
  id,
  data,
  labels = spectral_csv_settings_labels()
) {
  stopifnot(shiny::is.reactive(data))

  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$reset, {
      shiny::updateNumericInput(session, "skip", value = 0)
      shiny::updateNumericInput(session, "wavelength_column", value = 1)
      shiny::updateNumericInput(session, "value_column", value = 2)
      shiny::updateRadioButtons(session, "delimiter", selected = ",")
      shiny::updateRadioButtons(session, "decimal_mark", selected = ".")
      shiny::updateCheckboxInput(session, "header", value = TRUE)
    })

    settings <- shiny::reactive({
      list(
        skip = input$skip,
        delimiter = input$delimiter,
        decimal_mark = input$decimal_mark,
        wavelength_column = input$wavelength_column,
        value_column = input$value_column,
        header = input$header,
        reset = if (is.null(input$reset)) 0L else as.integer(input$reset)
      )
    })

    columns_are_valid <- shiny::reactive({
      parsed <- data()
      current <- settings()

      if (is.null(parsed) || inherits(parsed, "try-error")) {
        return(TRUE)
      }

      all(
        is.data.frame(parsed),
        length(current$wavelength_column) == 1L,
        length(current$value_column) == 1L,
        is.finite(current$wavelength_column),
        is.finite(current$value_column),
        current$wavelength_column >= 1,
        current$value_column >= 1,
        current$wavelength_column <= ncol(parsed),
        current$value_column <= ncol(parsed),
        current$wavelength_column != current$value_column
      )
    })

    output$validation <- shiny::renderUI({
      if (columns_are_valid()) {
        return(NULL)
      }

      htmltools::tags$p(
        class = "text-danger",
        role = "alert",
        labels$invalid_columns
      )
    })

    settings
  })
}

#' Coerce current and legacy CSV settings to one internal contract
#'
#' @param settings Named settings list.
#'
#' @return A normalized settings list.
#' @noRd
coerce_spectral_csv_settings <- function(settings) {
  if (!is.list(settings)) {
    stop("`settings` must be a named list.", call. = FALSE)
  }

  value_or <- function(primary, legacy, default = NULL) {
    value <- settings[[primary]]
    if (is.null(value)) {
      value <- settings[[legacy]]
    }
    if (is.null(value)) default else value
  }

  normalized <- list(
    skip = value_or("skip", "row_nr"),
    delimiter = value_or("delimiter", "separator"),
    decimal_mark = value_or("decimal_mark", "decimal"),
    wavelength_column = value_or("wavelength_column", "x_y"),
    value_column = value_or("value_column", "x_y2"),
    header = value_or("header", "header")
  )

  validate_spectral_csv_settings(normalized)
  normalized
}

#' Validate shared spectral CSV settings
#'
#' @param settings Normalized settings list.
#'
#' @return `TRUE`, invisibly.
#' @noRd
validate_spectral_csv_settings <- function(settings) {
  required <- c(
    "skip",
    "delimiter",
    "decimal_mark",
    "wavelength_column",
    "value_column",
    "header"
  )
  missing_settings <- setdiff(required, names(settings))
  if (length(missing_settings) > 0L) {
    stop(
      "Missing CSV settings: ",
      paste(missing_settings, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  whole_number <- function(x) {
    length(x) == 1L && is.numeric(x) && is.finite(x) && x == floor(x)
  }

  if (!whole_number(settings$skip) || settings$skip < 0) {
    stop("`skip` must be one non-negative whole number.", call. = FALSE)
  }
  if (
    !whole_number(settings$wavelength_column) ||
      settings$wavelength_column < 1
  ) {
    stop(
      "`wavelength_column` must be one positive whole number.",
      call. = FALSE
    )
  }
  if (!whole_number(settings$value_column) || settings$value_column < 1) {
    stop("`value_column` must be one positive whole number.", call. = FALSE)
  }
  if (settings$wavelength_column == settings$value_column) {
    stop("Wavelength and value columns must be different.", call. = FALSE)
  }
  if (
    !is.character(settings$delimiter) ||
      length(settings$delimiter) != 1L ||
      !settings$delimiter %in% c(",", ";", "")
  ) {
    stop("`delimiter` must be `,`, `;`, or an empty string.", call. = FALSE)
  }
  if (
    !is.character(settings$decimal_mark) ||
      length(settings$decimal_mark) != 1L ||
      nchar(settings$decimal_mark) != 1L
  ) {
    stop("`decimal_mark` must be exactly one character.", call. = FALSE)
  }
  if (
    !is.logical(settings$header) ||
      length(settings$header) != 1L ||
      is.na(settings$header)
  ) {
    stop("`header` must be `TRUE` or `FALSE`.", call. = FALSE)
  }

  invisible(TRUE)
}

#' Read a spectral CSV file using explicit settings
#'
#' @param path Path to a local delimited text file.
#' @param settings Current or legacy spectral CSV settings.
#'
#' @return A data frame containing the unchanged parsed columns.
#' @noRd
read_spectral_csv <- function(path, settings) {
  if (
    !is.character(path) ||
      length(path) != 1L ||
      is.na(path) ||
      !file.exists(path)
  ) {
    stop("`path` must identify one existing local file.", call. = FALSE)
  }

  settings <- coerce_spectral_csv_settings(settings)

  tryCatch(
    utils::read.csv(
      file = path,
      sep = settings$delimiter,
      dec = settings$decimal_mark,
      skip = settings$skip,
      header = settings$header
    ),
    error = function(error) {
      stop(
        "The spectral CSV could not be parsed with the selected settings: ",
        conditionMessage(error),
        call. = FALSE
      )
    }
  )
}

#' Extract the selected wavelength and value columns for preview
#'
#' @param data Parsed spectral CSV data.
#' @param settings Current or legacy spectral CSV settings.
#'
#' @return A tibble with source row, wavelength, and value columns.
#' @noRd
spectral_csv_preview_data <- function(data, settings) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  settings <- coerce_spectral_csv_settings(settings)
  selected <- c(settings$wavelength_column, settings$value_column)
  if (any(selected > ncol(data))) {
    detected <- ncol(data)
    available <- if (detected == 0L) {
      "no columns"
    } else if (detected == 1L) {
      "column 1"
    } else {
      paste0("columns 1\u2013", detected)
    }
    stop(
      paste0(
        "The file was parsed into ",
        detected,
        if (detected == 1L) " column" else " columns",
        ", but wavelength column ",
        settings$wavelength_column,
        " and transmission column ",
        settings$value_column,
        " were selected. Check the separator, decimal mark, header setting, ",
        "and rows to skip, or select from ",
        available,
        "."
      ),
      call. = FALSE
    )
  }

  tibble::tibble(
    source_row = seq_len(nrow(data)),
    wavelength_nm = data[[settings$wavelength_column]],
    value = data[[settings$value_column]]
  )
}
