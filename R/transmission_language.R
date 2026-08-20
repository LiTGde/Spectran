# Transmission language helpers ------------------------------------------

#' Resolve the active language for transmission-module text
#'
#' @param language_direct Optional explicit language.
#'
#' @return `"English"` or `"Deutsch"`.
#' @noRd
transmission_language_setting <- function(language_direct = NULL) {
  setting <- language_direct
  if (is.null(setting) && exists("language", envir = the, inherits = FALSE)) {
    setting <- the$language
  }
  if (
    is.null(setting) ||
      length(setting) != 1L ||
      is.na(setting) ||
      !setting %in% c("English", "Deutsch")
  ) {
    setting <- "English"
  }
  setting
}

#' Read one semantic transmission string from Spectran's language workbook
#'
#' @param key Semantic key without the `transmission.` prefix.
#' @param ... Optional values passed to [base::sprintf()].
#' @param language_direct Optional explicit language.
#'
#' @return One translated character string.
#' @noRd
transmission_text <- function(key, ..., language_direct = NULL) {
  if (!is.character(key) || length(key) != 1L || is.na(key)) {
    stop("`key` must be one text value.", call. = FALSE)
  }
  semantic_name <- paste0("transmission.", key)
  row <- match(semantic_name, language$ui$Name)
  if (is.na(row)) {
    stop(
      paste0("Transmission language key `", semantic_name, "` is missing."),
      call. = FALSE
    )
  }
  setting <- transmission_language_setting(language_direct)
  value <- language$ui[[setting]][[row]]
  if (
    is.null(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(value)
  ) {
    value <- language$ui$English[[row]]
  }
  arguments <- list(...)
  if (length(arguments) > 0L) {
    return(do.call(sprintf, c(list(fmt = value), arguments)))
  }
  value
}

#' Translate metric labels without changing their machine identifiers
#'
#' @param metric_id Stable metric identifiers.
#' @param fallback Existing labels used if a semantic entry is unavailable.
#' @param language_direct Optional explicit language.
#'
#' @return Translated labels in the same order.
#' @noRd
transmission_metric_labels <- function(
  metric_id,
  fallback,
  language_direct = NULL
) {
  stopifnot(length(metric_id) == length(fallback))
  vapply(
    seq_along(metric_id),
    function(index) {
      key <- paste0("metric_", metric_id[[index]])
      semantic_name <- paste0("transmission.", key)
      if (!semantic_name %in% language$ui$Name) {
        return(fallback[[index]])
      }
      transmission_text(key, language_direct = language_direct)
    },
    character(1)
  )
}

#' Localize diagnostic messages emitted by the pure preparation core
#'
#' Scientific helpers retain stable English audit messages. This adapter only
#' changes their visible UI presentation.
#'
#' @param messages Character vector of diagnostics.
#'
#' @return Localized messages in the same order.
#' @noRd
transmission_localize_diagnostics <- function(messages) {
  if (
    length(messages) == 0L ||
      !identical(transmission_language_setting(), "Deutsch")
  ) {
    return(messages)
  }

  vapply(
    messages,
    function(message) {
      localize_details <- function(value) {
        value <- gsub("at source rows", "in Quellzeilen", value, fixed = TRUE)
        value <- gsub("source row", "Quellzeile", value, fixed = TRUE)
        value <- gsub("input", "Eingabe", value, fixed = TRUE)
        value <- gsub(
          "scaled fraction",
          "skalierter Anteil",
          value,
          fixed = TRUE
        )
        value <- gsub(
          "; and ([0-9]+) more",
          "; und \\1 weitere",
          value
        )
        value
      }

      if (identical(message, "Supply at least two spectral samples.")) {
        return(transmission_text("diagnostic_two_samples"))
      }

      if (
        startsWith(
          message,
          "The selected wavelength column is not numeric."
        )
      ) {
        detail <- if (grepl(" Problem values: ", message, fixed = TRUE)) {
          values <- sub(
            paste0(
              "^The selected wavelength column is not numeric\\. ",
              "Problem values: (.*)\\. Check.*$"
            ),
            "\\1",
            message
          )
          transmission_text(
            "diagnostic_problem_values",
            localize_details(values)
          )
        } else {
          transmission_text("diagnostic_column_text")
        }
        return(transmission_text(
          "diagnostic_wavelength_not_numeric",
          detail
        ))
      }

      if (
        startsWith(
          message,
          "The selected transmission column is not numeric."
        )
      ) {
        detail <- if (grepl(" Problem values: ", message, fixed = TRUE)) {
          values <- sub(
            paste0(
              "^The selected transmission column is not numeric\\. ",
              "Problem values: (.*)\\. Check.*$"
            ),
            "\\1",
            message
          )
          transmission_text(
            "diagnostic_problem_values",
            localize_details(values)
          )
        } else {
          transmission_text("diagnostic_column_text")
        }
        return(transmission_text(
          "diagnostic_transmission_not_numeric",
          detail
        ))
      }

      if (
        identical(
          message,
          "Choose whether transmission is a fraction or percent."
        )
      ) {
        return(transmission_text("diagnostic_choose_scale"))
      }

      wavelength_missing_pattern <- paste0(
        "^Wavelengths contain missing or non-finite values at (.*)\\. ",
        "Check.*$"
      )
      if (grepl(wavelength_missing_pattern, message)) {
        details <- sub(wavelength_missing_pattern, "\\1", message)
        return(transmission_text(
          "diagnostic_wavelength_missing",
          localize_details(details)
        ))
      }

      transmission_missing_pattern <- paste0(
        "^Transmission contains missing or non-finite values at (.*)\\. ",
        "Check.*$"
      )
      if (grepl(transmission_missing_pattern, message)) {
        details <- sub(transmission_missing_pattern, "\\1", message)
        return(transmission_text(
          "diagnostic_transmission_missing",
          localize_details(details)
        ))
      }

      duplicate_pattern <- "^Duplicate wavelengths are not allowed: (.*)\\.$"
      if (grepl(duplicate_pattern, message)) {
        details <- sub(duplicate_pattern, "\\1", message)
        return(transmission_text(
          "diagnostic_duplicates",
          localize_details(details)
        ))
      }

      range_pattern <- paste0(
        "^Transmission must be between 0 and 1 after scaling\\. Affected ",
        "(.*)\\. Check whether Fraction or Percent is selected correctly\\.$"
      )
      if (grepl(range_pattern, message)) {
        details <- sub(range_pattern, "\\1", message)
        return(transmission_text(
          "diagnostic_transmission_range",
          localize_details(details)
        ))
      }

      if (
        identical(
          message,
          "The supplied spectrum does not overlap or bracket 380\u2013780 nm."
        )
      ) {
        return(transmission_text("diagnostic_no_overlap"))
      }

      parsed_columns_pattern <- paste0(
        "^The file was parsed into ([0-9]+) columns?, but wavelength column ",
        "([0-9]+) and transmission column ([0-9]+) were selected\\..*$"
      )
      if (grepl(parsed_columns_pattern, message)) {
        return(transmission_text(
          "diagnostic_parsed_columns",
          as.integer(sub(parsed_columns_pattern, "\\1", message)),
          as.integer(sub(parsed_columns_pattern, "\\2", message)),
          as.integer(sub(parsed_columns_pattern, "\\3", message))
        ))
      }

      if (
        startsWith(
          message,
          "The spectral CSV could not be parsed with the selected settings:"
        )
      ) {
        return(transmission_text("diagnostic_csv_parse"))
      }

      if (
        identical(
          message,
          transmission_text(
            "diagnostic_sorted",
            language_direct = "English"
          )
        )
      ) {
        return(transmission_text("diagnostic_sorted"))
      }

      outside_pattern <- paste0(
        "^([0-9]+) supplied sample\\(s\\) lie outside 380\u2013780 nm\\. ",
        "They are retained for audit and boundary bracketing but excluded ",
        "from calculations\\.$"
      )
      if (grepl(outside_pattern, message)) {
        count <- as.integer(sub(outside_pattern, "\\1", message))
        return(transmission_text("diagnostic_outside", count))
      }

      external_interval_pattern <- paste0(
        "^Outside-domain interval\\(s\\) (.*) contain no completed ",
        "380\u2013780 nm grid samples\\. They are shown for audit and boundary ",
        "context but require no interpolation consent\\.$"
      )
      if (grepl(external_interval_pattern, message)) {
        intervals <- sub(external_interval_pattern, "\\1", message)
        return(transmission_text(
          "diagnostic_external_intervals",
          intervals
        ))
      }

      lower_pattern <- paste0(
        "^Choose how to complete the lower tail \\((.*)\\): ",
        "opaque at 0%, transparent at 100%, or carry the first supplied ",
        "value backward\\.$"
      )
      if (grepl(lower_pattern, message)) {
        affected <- sub(lower_pattern, "\\1", message)
        return(transmission_text("require_lower_tail", affected))
      }

      upper_pattern <- paste0(
        "^Choose how to complete the upper tail \\((.*)\\): ",
        "opaque at 0%, transparent at 100%, or carry the last supplied ",
        "value forward\\.$"
      )
      if (grepl(upper_pattern, message)) {
        affected <- sub(upper_pattern, "\\1", message)
        return(transmission_text("require_upper_tail", affected))
      }

      gap_pattern <- paste0(
        "^Acknowledge linear interpolation across ([0-9]+) internal ",
        "gap\\(s\\) larger than ([0-9.]+) nm\\.$"
      )
      if (grepl(gap_pattern, message)) {
        count <- as.integer(sub(gap_pattern, "\\1", message))
        maximum <- sub(gap_pattern, "\\2", message)
        return(transmission_text("require_gap", count, maximum))
      }

      message
    },
    character(1),
    USE.NAMES = FALSE
  )
}
