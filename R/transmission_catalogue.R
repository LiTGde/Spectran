# Bundled transmission-filter catalogues ---------------------------------

#' Return the bundled transmission catalogue records
#'
#' @return A tibble with one row per bundled filter.
#' @noRd
transmission_catalogue_records_data <- function() {
  # Keep the locked source package available at runtime so the installed
  # catalogue retains its executable provenance alongside the bundled copy.
  invisible(photobiologyFilters::glass_windows)
  transmission_catalogue_records
}

#' Return one bundled transmission-filter record and curve
#'
#' @param catalogue_id Stable catalogue record identifier.
#'
#' @return A list with `record`, `curve`, and source `provenance`.
#' @noRd
transmission_catalogue_record <- function(catalogue_id) {
  if (
    length(catalogue_id) != 1L ||
      is.na(catalogue_id) ||
      !nzchar(trimws(catalogue_id))
  ) {
    stop("Select one catalogue filter.", call. = FALSE)
  }
  record <- transmission_catalogue_records[
    transmission_catalogue_records$catalogue_id == catalogue_id,
    ,
    drop = FALSE
  ]
  if (nrow(record) != 1L) {
    stop("The selected catalogue filter does not exist.", call. = FALSE)
  }
  curve <- transmission_catalogue_curves[
    transmission_catalogue_curves$catalogue_id == catalogue_id,
    c("wavelength_nm", "transmittance", "source_status"),
    drop = FALSE
  ]
  curve <- curve[order(curve$wavelength_nm), , drop = FALSE]
  rownames(curve) <- NULL
  list(
    record = record,
    curve = tibble::as_tibble(curve),
    provenance = transmission_catalogue_provenance[[record$catalogue[[1L]]]]
  )
}

#' Return one localized catalogue narrative field
#'
#' Bundled records retain their authoritative source field and additionally
#' expose explicit `_en` and `_de` presentation columns. This keeps catalogue
#' localization extensible without embedding record-specific prose in the UI.
#'
#' @param record One-row catalogue record.
#' @param field Base narrative field name.
#' @param language_direct Optional explicit language.
#'
#' @return One localized character value, or `NA_character_`.
#' @noRd
transmission_catalogue_localized_value <- function(
  record,
  field,
  language_direct = NULL
) {
  if (!is.data.frame(record) || nrow(record) != 1L) {
    stop("`record` must contain exactly one catalogue row.", call. = FALSE)
  }
  suffix <- if (
    identical(
      transmission_language_setting(language_direct),
      "Deutsch"
    )
  ) {
    "_de"
  } else {
    "_en"
  }
  localized_field <- paste0(field, suffix)
  candidates <- c(localized_field, field)
  for (candidate in candidates) {
    if (!candidate %in% names(record)) {
      next
    }
    value <- record[[candidate]][[1L]]
    if (
      length(value) == 1L &&
        !is.na(value) &&
        nzchar(trimws(as.character(value)))
    ) {
      return(as.character(value))
    }
  }
  NA_character_
}

#' Filter catalogue records for the catalogue browser
#'
#' @param records Catalogue record table.
#' @param collection One of `featured`, `all`, `facade_windows`, or
#'   `spitschan2019`.
#' @param category Optional category identifier.
#' @param query Optional free-text query.
#'
#' @return Filtered catalogue records sorted for display.
#' @noRd
filter_transmission_catalogue <- function(
  records = transmission_catalogue_records_data(),
  collection = "featured",
  category = "all",
  query = ""
) {
  collection <- match.arg(
    collection,
    c("featured", "all", "facade_windows", "spitschan2019")
  )
  result <- records
  if (identical(collection, "featured")) {
    result <- result[result$featured, , drop = FALSE]
  } else if (!identical(collection, "all")) {
    result <- result[result$catalogue == collection, , drop = FALSE]
  }

  if (
    length(category) == 1L &&
      !is.na(category) &&
      nzchar(category) &&
      !identical(category, "all")
  ) {
    result <- result[result$category_id == category, , drop = FALSE]
  }

  if (length(query) == 1L && !is.na(query) && nzchar(trimws(query))) {
    needle <- tolower(trimws(query))
    search_fields <- intersect(
      c(
        "display_name",
        "manufacturer",
        "product_name",
        "category_en",
        "category_de",
        "source_reference",
        "source_description",
        "source_description_en",
        "source_description_de"
      ),
      names(result)
    )
    searchable <- do.call(
      paste,
      c(unname(result[search_fields]), list(sep = " "))
    )
    result <- result[
      grepl(needle, tolower(searchable), fixed = TRUE),
      ,
      drop = FALSE
    ]
  }

  result <- result[
    order(
      !result$featured,
      result$category_en,
      result$display_name,
      result$catalogue_id
    ),
    ,
    drop = FALSE
  ]
  rownames(result) <- NULL
  tibble::as_tibble(result)
}

#' Format catalogue choices with unique, human-readable labels
#'
#' @param records Catalogue record table.
#' @param language `English` or `Deutsch`.
#'
#' @return Named character choices suitable for a Shiny select input.
#' @noRd
transmission_catalogue_choices <- function(records, language = "English") {
  if (nrow(records) == 0L) {
    return(character())
  }
  category <- if (identical(language, "Deutsch")) {
    records$category_de
  } else {
    records$category_en
  }
  labels <- paste0(records$display_name, " (", category, ")")
  duplicates <- duplicated(labels) | duplicated(labels, fromLast = TRUE)
  labels[duplicates] <- paste0(
    labels[duplicates],
    " \u00b7 ",
    records$source_record[duplicates]
  )
  stats::setNames(records$catalogue_id, labels)
}
