# Pure transmission-spectrum preparation ----------------------------------

#' Prepare a transmission curve for calculations on Spectran's visible grid
#'
#' @param data A data frame with `source_row`, `wavelength_nm`, and `value`.
#' @param scale Either `"fraction"` or `"percent"`.
#' @param lower_tail How missing wavelengths below the supplied range are
#'   completed: `"zero"`, `"one"`, `"carry"`, or `NULL` until the user
#'   decides. `"carry"` extends the nearest supplied value to the boundary.
#' @param upper_tail How missing wavelengths above the supplied range are
#'   completed: `"zero"`, `"one"`, `"carry"`, or `NULL` until the user
#'   decides. `"carry"` extends the nearest supplied value to the boundary.
#' @param acknowledge_large_gaps Whether interpolation across gaps larger than
#'   `max_auto_gap_nm` has been explicitly accepted.
#' @param max_auto_gap_nm Largest gap that can be interpolated automatically.
#' @param grid_nm Calculation wavelengths in nanometres.
#'
#' @return A `transmission_preparation` list containing original, normalized,
#'   completed, and diagnostic records.
#' @noRd
prepare_transmission_curve <- function(
  data,
  scale,
  lower_tail = NULL,
  upper_tail = NULL,
  acknowledge_large_gaps = FALSE,
  max_auto_gap_nm = 5,
  grid_nm = 380:780
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (
    !is.numeric(grid_nm) ||
      length(grid_nm) < 2L ||
      any(!is.finite(grid_nm)) ||
      is.unsorted(grid_nm, strictly = TRUE)
  ) {
    stop(
      "`grid_nm` must be a strictly increasing finite numeric vector.",
      call. = FALSE
    )
  }
  if (
    !is.numeric(max_auto_gap_nm) ||
      length(max_auto_gap_nm) != 1L ||
      !is.finite(max_auto_gap_nm) ||
      max_auto_gap_nm <= 0
  ) {
    stop("`max_auto_gap_nm` must be one positive finite number.", call. = FALSE)
  }

  required <- c("wavelength_nm", "value")
  missing_columns <- setdiff(required, names(data))
  if (length(missing_columns) > 0L) {
    stop(
      "`data` is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  source_row <- if ("source_row" %in% names(data)) data$source_row else NULL
  if (is.null(source_row)) {
    source_row <- seq_len(nrow(data))
  }
  original <- tibble::tibble(
    source_row = source_row,
    wavelength_nm = data$wavelength_nm,
    transmission_input = data$value
  )

  errors <- character()
  requirements <- character()
  warnings <- character()

  if (nrow(original) < 2L) {
    errors <- c(errors, "Supply at least two spectral samples.")
  }
  if (!is.numeric(original$wavelength_nm)) {
    converted <- suppressWarnings(as.numeric(as.character(
      original$wavelength_nm
    )))
    affected <- which(is.na(converted) & !is.na(original$wavelength_nm))
    detail <- if (length(affected) > 0L) {
      paste0(
        " Problem values: ",
        format_transmission_source_values(
          original$source_row[affected],
          original$wavelength_nm[affected]
        ),
        "."
      )
    } else {
      " The column was read as text."
    }
    errors <- c(
      errors,
      paste0(
        "The selected wavelength column is not numeric.",
        detail,
        " Check the decimal mark, header setting, rows to skip, and column ",
        "selections."
      )
    )
  }
  if (!is.numeric(original$transmission_input)) {
    converted <- suppressWarnings(as.numeric(as.character(
      original$transmission_input
    )))
    affected <- which(is.na(converted) & !is.na(original$transmission_input))
    detail <- if (length(affected) > 0L) {
      paste0(
        " Problem values: ",
        format_transmission_source_values(
          original$source_row[affected],
          original$transmission_input[affected]
        ),
        "."
      )
    } else {
      " The column was read as text."
    }
    errors <- c(
      errors,
      paste0(
        "The selected transmission column is not numeric.",
        detail,
        " Check the decimal mark, header setting, rows to skip, and column ",
        "selections."
      )
    )
  }
  if (
    is.null(scale) ||
      length(scale) != 1L ||
      is.na(scale) ||
      !scale %in% c("fraction", "percent")
  ) {
    errors <- c(errors, "Choose whether transmission is a fraction or percent.")
  }

  numeric_columns <- is.numeric(original$wavelength_nm) &&
    is.numeric(original$transmission_input)
  if (numeric_columns) {
    invalid_wavelength <- which(!is.finite(original$wavelength_nm))
    invalid_transmission_input <- which(
      !is.finite(
        original$transmission_input
      )
    )
    if (length(invalid_wavelength) > 0L) {
      errors <- c(
        errors,
        paste0(
          "Wavelengths contain missing or non-finite values at ",
          format_transmission_source_values(
            original$source_row[invalid_wavelength],
            original$wavelength_nm[invalid_wavelength]
          ),
          ". Check the decimal mark, header setting, rows to skip, and ",
          "wavelength column."
        )
      )
    }
    if (length(invalid_transmission_input) > 0L) {
      errors <- c(
        errors,
        paste0(
          "Transmission contains missing or non-finite values at ",
          format_transmission_source_values(
            original$source_row[invalid_transmission_input],
            original$transmission_input[invalid_transmission_input]
          ),
          ". Check the decimal mark, header setting, rows to skip, and ",
          "transmission column."
        )
      )
    }
  }

  if (length(errors) > 0L) {
    return(new_transmission_preparation(
      original = original,
      errors = unique(errors),
      requirements = requirements,
      warnings = warnings,
      max_auto_gap_nm = max_auto_gap_nm
    ))
  }

  transmittance <- original$transmission_input
  if (scale == "percent") {
    transmittance <- transmittance / 100
  }

  normalized <- tibble::tibble(
    source_row = original$source_row,
    wavelength_nm = as.numeric(original$wavelength_nm),
    transmittance = as.numeric(transmittance)
  )

  duplicate_wavelengths <- sort(unique(
    normalized$wavelength_nm[duplicated(normalized$wavelength_nm)]
  ))
  if (length(duplicate_wavelengths) > 0L) {
    duplicate_details <- vapply(
      duplicate_wavelengths,
      function(wavelength) {
        rows <- normalized$source_row[
          normalized$wavelength_nm == wavelength
        ]
        paste0(
          format_transmission_number(wavelength),
          " nm at source rows ",
          paste(rows, collapse = ", ")
        )
      },
      character(1)
    )
    errors <- c(
      errors,
      paste0(
        "Duplicate wavelengths are not allowed: ",
        paste(duplicate_details, collapse = "; "),
        "."
      )
    )
  }

  invalid_transmission <- normalized$transmittance < 0 |
    normalized$transmittance > 1
  if (any(invalid_transmission)) {
    affected <- which(invalid_transmission)
    details <- paste0(
      "source row ",
      normalized$source_row[affected],
      " (input ",
      vapply(
        original$transmission_input[
          match(normalized$source_row[affected], original$source_row)
        ],
        format_transmission_number,
        character(1)
      ),
      ", scaled fraction ",
      vapply(
        normalized$transmittance[affected],
        format_transmission_number,
        character(1)
      ),
      ")"
    )
    shown <- utils::head(details, 5L)
    more <- length(details) - length(shown)
    errors <- c(
      errors,
      paste0(
        "Transmission must be between 0 and 1 after scaling. Affected ",
        paste(shown, collapse = "; "),
        if (more > 0L) paste0("; and ", more, " more") else "",
        ". Check whether Fraction or Percent is selected correctly."
      )
    )
  }

  original_order <- normalized$wavelength_nm
  sorted_order <- order(normalized$wavelength_nm, normalized$source_row)
  was_sorted <- !identical(sorted_order, seq_len(nrow(normalized)))
  normalized <- normalized[sorted_order, , drop = FALSE]
  normalized$within_calculation_range <-
    normalized$wavelength_nm >= min(grid_nm) &
    normalized$wavelength_nm <= max(grid_nm)

  if (was_sorted) {
    warnings <- c(
      warnings,
      "Input wavelengths were out of order and have been sorted."
    )
  }

  outside_count <- sum(!normalized$within_calculation_range)
  if (outside_count > 0L) {
    warnings <- c(
      warnings,
      paste0(
        outside_count,
        " supplied sample(s) lie outside 380\u2013780 nm. They are retained for ",
        "audit and boundary bracketing but excluded from calculations."
      )
    )
  }

  if (
    min(normalized$wavelength_nm) > max(grid_nm) ||
      max(normalized$wavelength_nm) < min(grid_nm)
  ) {
    errors <- c(
      errors,
      "The supplied spectrum does not overlap or bracket 380\u2013780 nm."
    )
  }

  if (length(errors) > 0L) {
    return(new_transmission_preparation(
      original = original,
      normalized = normalized,
      errors = unique(errors),
      requirements = requirements,
      warnings = warnings,
      was_sorted = was_sorted,
      outside_count = outside_count,
      duplicate_wavelengths = duplicate_wavelengths,
      max_auto_gap_nm = max_auto_gap_nm
    ))
  }

  wavelength <- normalized$wavelength_nm
  transmission <- normalized$transmittance
  interval_left <- utils::head(wavelength, -1L)
  interval_right <- utils::tail(wavelength, -1L)
  interval_width <- interval_right - interval_left
  relevant_interval <- interval_right > min(grid_nm) &
    interval_left < max(grid_nm)
  large_interval <- relevant_interval & interval_width > max_auto_gap_nm

  large_gaps <- tibble::tibble(
    from_nm = interval_left[large_interval],
    to_nm = interval_right[large_interval],
    width_nm = interval_width[large_interval]
  )

  external_interval <- interval_right <= min(grid_nm) |
    interval_left >= max(grid_nm)
  external_intervals <- tibble::tibble(
    from_nm = interval_left[external_interval],
    to_nm = interval_right[external_interval],
    width_nm = interval_width[external_interval],
    position = as.character(ifelse(
      interval_right[external_interval] <= min(grid_nm),
      "below calculation range",
      "above calculation range"
    ))
  )
  if (nrow(external_intervals) > 0L) {
    external_labels <- paste0(
      vapply(
        external_intervals$from_nm,
        format_transmission_number,
        character(1)
      ),
      "\u2013",
      vapply(
        external_intervals$to_nm,
        format_transmission_number,
        character(1)
      ),
      " nm"
    )
    warnings <- c(
      warnings,
      paste0(
        "Outside-domain interval(s) ",
        paste(external_labels, collapse = ", "),
        " contain no completed 380\u2013780 nm grid samples. They are shown for ",
        "audit and boundary context but require no interpolation consent."
      )
    )
  }

  lower_missing <- min(wavelength) > min(grid_nm)
  upper_missing <- max(wavelength) < max(grid_nm)
  lower_tail_grid <- grid_nm[grid_nm < min(wavelength)]
  upper_tail_grid <- grid_nm[grid_nm > max(wavelength)]
  lower_tail <- normalize_tail_choice(lower_tail)
  upper_tail <- normalize_tail_choice(upper_tail)

  if (lower_missing && is.null(lower_tail)) {
    requirements <- c(
      requirements,
      paste0(
        "Choose how to complete the lower tail (",
        format_transmission_range(lower_tail_grid),
        "): opaque at 0%, transparent at 100%, or carry the first supplied ",
        "value backward."
      )
    )
  }
  if (upper_missing && is.null(upper_tail)) {
    requirements <- c(
      requirements,
      paste0(
        "Choose how to complete the upper tail (",
        format_transmission_range(upper_tail_grid),
        "): opaque at 0%, transparent at 100%, or carry the last supplied ",
        "value forward."
      )
    )
  }
  if (nrow(large_gaps) > 0L && !isTRUE(acknowledge_large_gaps)) {
    requirements <- c(
      requirements,
      paste0(
        "Acknowledge linear interpolation across ",
        nrow(large_gaps),
        " internal gap(s) larger than ",
        max_auto_gap_nm,
        " nm."
      )
    )
  }

  completed_value <- stats::approx(
    x = wavelength,
    y = transmission,
    xout = grid_nm,
    method = "linear",
    rule = 1,
    ties = "ordered"
  )$y
  status <- rep(NA_character_, length(grid_nm))

  supplied_match <- match(grid_nm, wavelength)
  supplied <- !is.na(supplied_match)
  completed_value[supplied] <- transmission[supplied_match[supplied]]
  status[supplied] <- "supplied"

  interval_index <- findInterval(grid_nm, wavelength)
  between_samples <- !supplied &
    interval_index >= 1L &
    interval_index < length(wavelength)
  if (any(between_samples)) {
    widths <- interval_width[interval_index[between_samples]]
    status[between_samples] <- ifelse(
      widths > max_auto_gap_nm,
      if (isTRUE(acknowledge_large_gaps)) {
        "acknowledged_large_gap"
      } else {
        "large_gap_unacknowledged"
      },
      "interpolated"
    )
  }

  lower_grid <- grid_nm < min(wavelength)
  if (any(lower_grid)) {
    if (is.null(lower_tail)) {
      status[lower_grid] <- "missing_lower_tail"
    } else {
      completed_value[lower_grid] <- switch(
        lower_tail,
        zero = 0,
        one = 1,
        carry = transmission[[1L]]
      )
      status[lower_grid] <- if (identical(lower_tail, "carry")) {
        "carried_lower_tail"
      } else {
        "assumed_lower_tail"
      }
    }
  }

  upper_grid <- grid_nm > max(wavelength)
  if (any(upper_grid)) {
    if (is.null(upper_tail)) {
      status[upper_grid] <- "missing_upper_tail"
    } else {
      completed_value[upper_grid] <- switch(
        upper_tail,
        zero = 0,
        one = 1,
        carry = transmission[[length(transmission)]]
      )
      status[upper_grid] <- if (identical(upper_tail, "carry")) {
        "carried_upper_tail"
      } else {
        "assumed_upper_tail"
      }
    }
  }

  completed <- tibble::tibble(
    wavelength_nm = as.numeric(grid_nm),
    transmittance = completed_value,
    status = status
  )

  ready <- length(errors) == 0L &&
    length(requirements) == 0L &&
    all(is.finite(completed$transmittance)) &&
    all(completed$transmittance >= 0 & completed$transmittance <= 1)

  new_transmission_preparation(
    original = original,
    normalized = normalized,
    completed = completed,
    errors = unique(errors),
    requirements = unique(requirements),
    warnings = unique(warnings),
    ready = ready,
    was_sorted = was_sorted,
    outside_count = outside_count,
    duplicate_wavelengths = duplicate_wavelengths,
    lower_missing = lower_missing,
    upper_missing = upper_missing,
    lower_tail = lower_tail,
    upper_tail = upper_tail,
    lower_tail_grid = lower_tail_grid,
    upper_tail_grid = upper_tail_grid,
    large_gaps = large_gaps,
    external_intervals = external_intervals,
    acknowledge_large_gaps = isTRUE(acknowledge_large_gaps),
    max_auto_gap_nm = max_auto_gap_nm,
    input_order = original_order
  )
}

#' Format a numeric value for validation and audit messages
#'
#' @param value A scalar value.
#'
#' @return A compact character representation.
#' @noRd
format_transmission_number <- function(value) {
  if (length(value) != 1L) {
    stop("`value` must contain exactly one element.", call. = FALSE)
  }
  if (is.na(value)) {
    return(if (is.numeric(value) && is.nan(value)) "NaN" else "NA")
  }
  if (is.numeric(value) && is.infinite(value)) {
    return(if (value > 0) "Inf" else "-Inf")
  }
  if (is.numeric(value)) {
    return(format(value, digits = 10L, scientific = FALSE, trim = TRUE))
  }
  paste0("\"", as.character(value), "\"")
}

#' Format source-row examples for an actionable validation message
#'
#' @param rows Source-row identifiers.
#' @param values Values from those rows.
#' @param max_examples Maximum number of examples to show.
#'
#' @return A concise character string.
#' @noRd
format_transmission_source_values <- function(
  rows,
  values,
  max_examples = 5L
) {
  stopifnot(length(rows) == length(values))
  shown <- seq_len(min(length(rows), max_examples))
  details <- paste0(
    "source row ",
    rows[shown],
    " (",
    vapply(values[shown], format_transmission_number, character(1)),
    ")"
  )
  more <- length(rows) - length(shown)
  paste0(
    paste(details, collapse = "; "),
    if (more > 0L) paste0("; and ", more, " more") else ""
  )
}

#' Format an affected calculation-grid interval
#'
#' @param wavelength_nm One or more affected calculation wavelengths.
#'
#' @return A human-readable wavelength or wavelength range.
#' @noRd
format_transmission_range <- function(wavelength_nm) {
  if (length(wavelength_nm) == 0L) {
    return("no calculation-grid wavelengths")
  }
  limits <- range(wavelength_nm)
  if (limits[[1]] == limits[[2]]) {
    return(paste0(format_transmission_number(limits[[1]]), " nm"))
  }
  paste0(
    format_transmission_number(limits[[1]]),
    "\u2013",
    format_transmission_number(limits[[2]]),
    " nm"
  )
}

#' Normalize a tail completion choice
#'
#' @param choice Tail choice supplied by Shiny or a pure R caller.
#'
#' @return `"zero"`, `"one"`, `"carry"`, or `NULL`.
#' @noRd
normalize_tail_choice <- function(choice) {
  if (is.null(choice) || length(choice) == 0L) {
    return(NULL)
  }
  if (length(choice) == 1L && (is.na(choice) || identical(choice, ""))) {
    return(NULL)
  }
  if (
    !is.character(choice) ||
      length(choice) != 1L ||
      is.na(choice) ||
      !choice %in% c("zero", "one", "carry")
  ) {
    stop(
      "A tail choice must be `zero`, `one`, `carry`, or `NULL`.",
      call. = FALSE
    )
  }
  choice
}

#' Construct a transmission preparation result
#'
#' @param original Original selected input rows.
#' @param normalized Sorted and fraction-scaled supplied data.
#' @param completed Completed calculation grid.
#' @param errors Blocking data errors.
#' @param requirements Unresolved user decisions or acknowledgements.
#' @param warnings Non-blocking disclosures.
#' @param ready Whether the result can proceed to application.
#' @param ... Additional diagnostic fields.
#'
#' @return A `transmission_preparation` object.
#' @noRd
new_transmission_preparation <- function(
  original,
  normalized = NULL,
  completed = NULL,
  errors = character(),
  requirements = character(),
  warnings = character(),
  ready = FALSE,
  ...
) {
  diagnostics <- c(
    list(
      ready = isTRUE(ready),
      errors = errors,
      requirements = requirements,
      warnings = warnings
    ),
    list(...)
  )

  structure(
    list(
      original = original,
      normalized = normalized,
      completed = completed,
      diagnostics = diagnostics
    ),
    class = "transmission_preparation"
  )
}

#' Summarize completion statuses for display and audit
#'
#' @param preparation A result from `prepare_transmission_curve()`.
#'
#' @return A tibble with one row per completion status.
#' @noRd
transmission_status_summary <- function(preparation) {
  if (!inherits(preparation, "transmission_preparation")) {
    stop(
      "`preparation` must be returned by `prepare_transmission_curve()`.",
      call. = FALSE
    )
  }
  if (is.null(preparation$completed)) {
    return(tibble::tibble(status = character(), samples = integer()))
  }

  counts <- table(preparation$completed$status, useNA = "ifany")
  tibble::tibble(
    status = names(counts),
    samples = as.integer(counts)
  )
}

#' Translate completed-curve status identifiers for display
#'
#' @param status Machine-readable status values.
#'
#' @return Human-readable labels in the same order.
#' @noRd
transmission_status_label <- function(status) {
  keys <- c(
    supplied = "status_supplied",
    interpolated = "status_interpolated",
    acknowledged_large_gap = "status_large_ack",
    large_gap_unacknowledged = "status_large_wait",
    assumed_lower_tail = "status_lower_assumed",
    assumed_upper_tail = "status_upper_assumed",
    carried_lower_tail = "status_lower_carried",
    carried_upper_tail = "status_upper_carried",
    missing_lower_tail = "status_lower_wait",
    missing_upper_tail = "status_upper_wait",
    outside_range = "status_outside"
  )
  labels <- vapply(keys, transmission_text, character(1))
  translated <- unname(labels[status])
  translated[is.na(translated)] <- status[is.na(translated)]
  translated
}

#' Create the downloadable neutral-filter template
#'
#' @return A 401-row tibble with fractional transmittance equal to one.
#' @noRd
transmission_template_data <- function() {
  tibble::tibble(
    wavelength_nm = 380:780,
    transmittance_fraction = rep(1, 401L)
  )
}

#' Deterministic fixtures for the isolated transmission showcase
#'
#' @param name Fixture name.
#'
#' @return A tibble accepted by `prepare_transmission_curve()`.
#' @noRd
transmission_fixture <- function(
  name = c("neutral", "selective", "partial", "large_gap", "invalid")
) {
  name <- match.arg(name)

  if (name == "neutral") {
    wavelength <- seq(380, 780, by = 5)
    value <- rep(0.5, length(wavelength))
  } else if (name == "selective") {
    wavelength <- seq(380, 780, by = 5)
    value <- 0.12 + 0.78 / (1 + exp(-(wavelength - 545) / 24))
  } else if (name == "partial") {
    wavelength <- seq(420, 700, by = 5)
    value <- seq(0.15, 0.85, length.out = length(wavelength))
  } else if (name == "large_gap") {
    wavelength <- c(seq(380, 500, by = 5), seq(525, 780, by = 5))
    value <- 0.55 + 0.2 * sin((wavelength - 380) / 80)
  } else {
    wavelength <- c(380, 400, 400, 780)
    value <- c(0.5, 0.6, 1.2, 0.5)
  }

  fixture <- tibble::tibble(
    source_row = seq_along(wavelength),
    wavelength_nm = wavelength,
    value = value
  )
  attr(fixture, "filter_name") <- switch(
    name,
    neutral = "Neutral 50%",
    selective = "Selective short-wavelength filter",
    partial = "Partial-coverage filter",
    large_gap = "Large-gap filter",
    invalid = "Invalid filter fixture"
  )
  fixture
}
