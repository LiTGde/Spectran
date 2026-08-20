# Transmission export helpers --------------------------------------------

#' Freeze the original transmission-input record
#'
#' @param record_type Input-record type.
#' @param record_name Original filename or catalogue/fixture name.
#' @param media_type Input media type.
#' @param raw_bytes Exact uploaded bytes, when applicable.
#' @param parsed_values Explicitly selected wavelength/value rows.
#' @param citation Source citation or user-supplied record label.
#' @param license Rights or licence statement.
#' @param source_url Source URL, when applicable.
#' @param ... Additional pinned provenance fields.
#'
#' @return A `transmission_input_record` list.
#' @noRd
new_transmission_input_record <- function(
  record_type = c("upload", "development_fixture", "catalogue"),
  record_name,
  media_type = "text/csv",
  raw_bytes = raw(),
  parsed_values,
  citation,
  license,
  source_url = "",
  ...
) {
  record_type <- match.arg(record_type)
  if (!is.raw(raw_bytes)) {
    stop("`raw_bytes` must be a raw vector.", call. = FALSE)
  }
  if (!is.data.frame(parsed_values)) {
    stop("`parsed_values` must be a data frame.", call. = FALSE)
  }
  optional_text <- function(value, arg) {
    if (!is.character(value) || length(value) != 1L || is.na(value)) {
      stop(paste0("`", arg, "` must be one text value."), call. = FALSE)
    }
    value
  }
  extra <- list(...)
  structure(
    c(
      list(
        record_type = record_type,
        record_name = transmission_scalar_text(record_name, "record_name"),
        media_type = optional_text(media_type, "media_type"),
        raw_bytes = raw_bytes,
        parsed_values = tibble::as_tibble(parsed_values),
        citation = transmission_scalar_text(citation, "citation"),
        license = transmission_scalar_text(license, "license"),
        source_url = optional_text(source_url, "source_url")
      ),
      extra
    ),
    class = c("transmission_input_record", "list")
  )
}

#' Create a portable filename component
#'
#' @param value User-facing text.
#' @param fallback Value used when no portable characters remain.
#'
#' @return A lowercase ASCII filename component.
#' @noRd
transmission_filename_component <- function(value, fallback = "transmission") {
  value <- transmission_scalar_text(value, "value")
  value <- gsub("\u00d7", "x", value, fixed = TRUE)
  value <- gsub("\u00b0", "deg", value, fixed = TRUE)
  ascii <- iconv(value, from = "UTF-8", to = "ASCII//TRANSLIT")
  if (is.na(ascii)) {
    ascii <- value
  }
  component <- tolower(gsub("[^A-Za-z0-9]+", "-", ascii))
  component <- gsub("(^-+|-+$)", "", component)
  if (!nzchar(component)) fallback else component
}

#' Export a completed transmission curve
#'
#' @param snapshot Applied transmission snapshot.
#'
#' @return The completed-filter contract as a tibble.
#' @noRd
transmission_completed_filter_export <- function(snapshot) {
  validate_transmission_applied_snapshot(snapshot)
  as_completed_filter(snapshot$filter)
}

#' Export incident and transmitted spectra on one grid
#'
#' @param snapshot Applied transmission snapshot.
#'
#' @return A 401-row comparison tibble.
#' @noRd
transmission_spectral_comparison_export <- function(snapshot) {
  validate_transmission_applied_snapshot(snapshot)
  tibble::tibble(
    wavelength_nm = snapshot$incident_spectrum$Wellenlaenge,
    incident_spectral_irradiance_w_m2_nm = snapshot$incident_spectrum$Bestrahlungsstaerke,
    transmittance = snapshot$filter$transmittance,
    transmitted_spectral_irradiance_w_m2_nm = snapshot$transmitted_spectrum$Bestrahlungsstaerke,
    completion_status = snapshot$filter$status
  )
}

#' Export D65-referenced filter properties
#'
#' @param snapshot Applied transmission snapshot.
#'
#' @return Long-form D65 property records.
#' @noRd
transmission_d65_export <- function(snapshot) {
  validate_transmission_applied_snapshot(snapshot)
  tibble::as_tibble(snapshot$d65_properties)
}

#' Export active-source applied metrics
#'
#' @param snapshot Applied transmission snapshot.
#'
#' @return Long-form active-source metric records.
#' @noRd
transmission_applied_metrics_export <- function(snapshot) {
  validate_transmission_applied_snapshot(snapshot)
  tibble::as_tibble(snapshot$active_metrics)
}

#' Export active-source retained light metrics
#'
#' @param snapshot Applied transmission snapshot.
#'
#' @return Long-form retained metric records.
#' @noRd
transmission_light_metrics_export <- function(snapshot) {
  metrics <- transmission_applied_metrics_export(snapshot)
  metrics[metrics$comparison_type == "retained", , drop = FALSE]
}

#' Export action factors and daylight efficacy ratios
#'
#' @param snapshot Applied transmission snapshot.
#'
#' @return Long-form change metric records.
#' @noRd
transmission_balance_metrics_export <- function(snapshot) {
  metrics <- transmission_applied_metrics_export(snapshot)
  metrics[metrics$comparison_type == "change", , drop = FALSE]
}

#' Machine identifiers for the configurable user export bundle
#'
#' @return Character vector of supported bundle contents.
#' @noRd
transmission_export_content_ids <- function() {
  c(
    "result_plot",
    "filter_plot",
    "plot_table",
    "d65_png",
    "light_png",
    "balance_png",
    "completed_csv",
    "comparison_csv",
    "d65_csv",
    "light_csv",
    "balance_csv",
    "history_csv",
    "audit_zip"
  )
}

#' Match one metric group to its styled table
#'
#' @param snapshot Applied transmission snapshot.
#' @param metric_group One of `"d65"`, `"light"`, or `"balance"`.
#'
#' @return A `gt_tbl`.
#' @noRd
transmission_metric_gt <- function(
  snapshot,
  metric_group = c("d65", "light", "balance")
) {
  validate_transmission_applied_snapshot(snapshot)
  metric_group <- match.arg(metric_group)
  switch(
    metric_group,
    d65 = transmission_d65_gt(snapshot),
    light = transmission_absolute_gt(snapshot),
    balance = transmission_balance_gt(snapshot)
  )
}

#' Validate an immutable applied snapshot
#'
#' @param snapshot Candidate applied snapshot.
#'
#' @return The snapshot, invisibly.
#' @noRd
validate_transmission_applied_snapshot <- function(snapshot) {
  if (!inherits(snapshot, "transmission_applied_snapshot")) {
    stop(
      "`snapshot` must be a transmission applied snapshot.",
      call. = FALSE
    )
  }
  as_visible_spectrum(snapshot$incident_spectrum, "snapshot$incident_spectrum")
  as_visible_spectrum(
    snapshot$transmitted_spectrum,
    "snapshot$transmitted_spectrum"
  )
  as_completed_filter(snapshot$filter, "snapshot$filter")
  invisible(snapshot)
}

#' Flatten nested metadata for a two-column audit table
#'
#' @param value A nested list.
#' @param prefix Current path prefix.
#'
#' @return A tibble containing `field` and `value`.
#' @noRd
transmission_flatten_metadata <- function(value, prefix = "") {
  if (!is.list(value) || is.data.frame(value)) {
    stop("`value` must be a metadata list.", call. = FALSE)
  }
  rows <- list()
  append_value <- function(path, item) {
    display <- if (is.null(item)) {
      ""
    } else if (is.raw(item)) {
      paste0("<raw bytes: ", length(item), ">")
    } else if (is.data.frame(item)) {
      paste0("<data frame: ", nrow(item), " rows x ", ncol(item), " columns>")
    } else if (length(item) == 0L) {
      ""
    } else {
      paste(as.character(item), collapse = " | ")
    }
    rows[[length(rows) + 1L]] <<- tibble::tibble(
      field = path,
      value = display
    )
  }
  walk <- function(item, path) {
    if (is.list(item) && !is.data.frame(item)) {
      if (length(item) == 0L) {
        append_value(path, NULL)
        return(invisible(NULL))
      }
      item_names <- names(item)
      if (is.null(item_names)) {
        append_value(path, item)
        return(invisible(NULL))
      }
      for (index in seq_along(item)) {
        child_name <- item_names[[index]]
        if (!nzchar(child_name)) {
          child_name <- as.character(index)
        }
        child_path <- if (nzchar(path)) {
          paste(path, child_name, sep = ".")
        } else {
          child_name
        }
        walk(item[[index]], child_path)
      }
      return(invisible(NULL))
    }
    append_value(path, item)
    invisible(NULL)
  }
  walk(value, prefix)
  if (length(rows) == 0L) {
    return(tibble::tibble(field = character(), value = character()))
  }
  do.call(rbind, rows)
}

#' Create decision and warning records for an audit archive
#'
#' @param snapshot Applied transmission snapshot.
#'
#' @return A long-form audit tibble.
#' @noRd
transmission_decisions_warnings_export <- function(snapshot) {
  validate_transmission_applied_snapshot(snapshot)
  decisions <- snapshot$metadata$normalization_decisions
  decision_rows <- if (is.null(decisions)) {
    tibble::tibble(kind = character(), item = character(), value = character())
  } else {
    flattened <- transmission_flatten_metadata(decisions)
    tibble::tibble(
      kind = "decision",
      item = flattened$field,
      value = flattened$value
    )
  }
  warnings <- unique(c(
    snapshot$metadata$normalization_warnings,
    snapshot$warnings
  ))
  warnings <- warnings[!is.na(warnings) & nzchar(warnings)]
  warning_rows <- tibble::tibble(
    kind = rep("warning", length(warnings)),
    item = if (length(warnings) == 0L) {
      character()
    } else {
      paste0("warning_", seq_along(warnings))
    },
    value = warnings
  )
  rbind(decision_rows, warning_rows)
}

#' Citation and licence records for an audit archive
#'
#' @param snapshot Applied transmission snapshot.
#'
#' @return A citation table.
#' @noRd
transmission_citations_licenses_export <- function(snapshot) {
  validate_transmission_applied_snapshot(snapshot)
  input_record <- snapshot$metadata$input_record
  input_citation <- if (is.list(input_record)) {
    input_record$citation
  } else {
    NULL
  }
  input_license <- if (is.list(input_record)) {
    input_record$license
  } else {
    NULL
  }
  if (is.null(input_citation) || !nzchar(input_citation)) {
    input_citation <- "User-supplied transmission spectrum"
  }
  if (is.null(input_license) || !nzchar(input_license)) {
    input_license <- "Rights and reuse terms remain with the user or source"
  }

  tibble::tibble(
    component = c(
      "Spectran software",
      "CIE alpha-opic action spectra and efficacy constants",
      "CIE standard illuminant D65",
      "Transmission input record"
    ),
    citation = c(
      paste(
        "Zauner J. Spectran: Visual and Non-Visual Spectral Analysis of Light.",
        "R package version 1.0.6."
      ),
      "CIE S 026/E:2018, CIE System for Metrology of Optical Radiation for ipRGC-Influenced Responses to Light.",
      "ISO/CIE 23539:2023, Photometry: The CIE system of physical photometry.",
      input_citation
    ),
    source_url = c(
      "https://github.com/LiTGde/Spectran",
      "https://doi.org/10.25039/S026.2018",
      "https://doi.org/10.25039/IS0.CIE.23539.2023",
      if (is.list(input_record) && !is.null(input_record$source_url)) {
        input_record$source_url
      } else {
        ""
      }
    ),
    license_or_rights = c(
      "MIT",
      "Reference standard; no CIE standard text is bundled in this archive",
      "Reference standard; no CIE standard text is bundled in this archive",
      input_license
    )
  )
}

#' Bilingual audit README
#'
#' @param snapshot Applied transmission snapshot.
#' @param active_state Active-spectrum adapter at download time.
#' @param generated_at Generation time.
#'
#' @return UTF-8 text lines.
#' @noRd
transmission_audit_readme <- function(snapshot, active_state, generated_at) {
  validate_transmission_applied_snapshot(snapshot)
  active_state <- as_transmission_active_spectrum(active_state)
  generated <- format(
    as.POSIXct(generated_at, tz = "UTC"),
    "%Y-%m-%d %H:%M:%S UTC",
    tz = "UTC"
  )
  c(
    "SPECTRAN TRANSMISSION AUDIT ARCHIVE",
    "",
    "English",
    paste0("Generated: ", generated),
    paste0("Applied source: ", snapshot$incident_name),
    paste0("Filter: ", snapshot$metadata$filter_name),
    paste0("Active spectrum at download: ", active_state$name),
    paste0("Active history node: ", active_state$node_id),
    paste0("Active change type: ", active_state$change_type),
    "All scientific calculations use the 380 to 780 nm, 1 nm grid.",
    paste(
      "The archive preserves the input record, parsed and completed curves,",
      "incident and transmitted spectra, metrics, history tree, decisions,",
      "warnings, citations, licences, and checksums."
    ),
    paste(
      "A blank or undefined value is not zero. Consult warning fields and",
      "decisions-warnings.csv before interpreting such values."
    ),
    "manifest.csv lists payload files, byte sizes, and MD5 checksums.",
    "",
    "Deutsch",
    paste0("Erstellt: ", generated),
    paste0("Angewandte Quelle: ", snapshot$incident_name),
    paste0("Filter: ", snapshot$metadata$filter_name),
    paste0("Aktives Spektrum beim Download: ", active_state$name),
    paste0("Aktiver Verlaufsknoten: ", active_state$node_id),
    paste0("Art der Aktivierung: ", active_state$change_type),
    "Alle wissenschaftlichen Berechnungen verwenden 380 bis 780 nm in 1-nm-Schritten.",
    paste(
      "Das Archiv enth\u00e4lt den Eingabedatensatz, die eingelesene und",
      "vervollst\u00e4ndigte Kurve, einfallende und transmittierte Spektren,"
    ),
    paste(
      "Kennwerte, den Verlaufsbaum, Entscheidungen, Warnungen, Quellen,",
      "Lizenzen und Pr\u00fcfsummen."
    ),
    paste(
      "Ein leerer oder nicht definierter Wert ist nicht gleich null.",
      paste0(
        "Vor der Interpretation bitte Warnungen und decisions-warnings.csv ",
        "pr\u00fcfen."
      )
    ),
    paste0(
      "manifest.csv enth\u00e4lt Dateinamen, Dateigr\u00f6\u00dfen und ",
      "MD5-Pr\u00fcfsummen."
    )
  )
}

#' Write a UTF-8 CSV consistently
#'
#' @param data Data frame to write.
#' @param path Output path.
#'
#' @return The path, invisibly.
#' @noRd
write_transmission_csv <- function(data, path) {
  utils::write.csv(
    data,
    file = path,
    row.names = FALSE,
    na = "",
    fileEncoding = "UTF-8"
  )
  invisible(path)
}

#' Write a complete transmission audit ZIP
#'
#' @param file Output ZIP path.
#' @param snapshot Current immutable applied snapshot.
#' @param history Session-local history tree.
#' @param active_state Current active-spectrum adapter.
#' @param generated_at Archive generation time.
#'
#' @return The output path, invisibly.
#' @noRd
write_transmission_audit_zip <- function(
  file,
  snapshot,
  history,
  active_state,
  generated_at = Sys.time()
) {
  validate_transmission_applied_snapshot(snapshot)
  validate_transmission_history(history)
  active_state <- as_transmission_active_spectrum(active_state)
  if (!requireNamespace("zip", quietly = TRUE)) {
    stop("The `zip` package is required to create an audit archive.")
  }

  audit_dir <- tempfile("spectran-transmission-audit-")
  dir.create(audit_dir, recursive = TRUE)
  on.exit(unlink(audit_dir, recursive = TRUE, force = TRUE), add = TRUE)
  subdirectories <- c(
    "input",
    "filter",
    "spectra",
    "metrics",
    "history",
    "metadata"
  )
  vapply(
    file.path(audit_dir, subdirectories),
    dir.create,
    logical(1),
    recursive = TRUE
  )

  readme_path <- file.path(audit_dir, "README.txt")
  writeLines(
    transmission_audit_readme(snapshot, active_state, generated_at),
    readme_path,
    useBytes = TRUE
  )

  input_record <- snapshot$metadata$input_record
  if (!is.list(input_record)) {
    input_record <- list(
      record_type = "unspecified",
      record_name = "transmission-input",
      media_type = "",
      raw_bytes = raw(),
      parsed_values = snapshot$metadata$parsed_values,
      citation = "User-supplied transmission spectrum",
      license = "Rights and reuse terms remain with the user or source"
    )
  }
  raw_bytes <- input_record$raw_bytes
  if (is.raw(raw_bytes) && length(raw_bytes) > 0L) {
    original_name <- input_record$record_name
    if (is.null(original_name) || !nzchar(original_name)) {
      original_name <- "transmission-upload.dat"
    }
    original_name <- basename(original_name)
    original_name <- gsub("[^A-Za-z0-9._-]+", "-", original_name)
    writeBin(raw_bytes, file.path(audit_dir, "input", original_name))
  } else if (is.data.frame(input_record$parsed_values)) {
    write_transmission_csv(
      input_record$parsed_values,
      file.path(audit_dir, "input", "development-fixture-record.csv")
    )
  }

  input_metadata <- input_record
  input_metadata$raw_bytes <- NULL
  input_metadata$parsed_values <- NULL
  write_transmission_csv(
    transmission_flatten_metadata(input_metadata),
    file.path(audit_dir, "input", "input-record-metadata.csv")
  )
  parsed_values <- input_record$parsed_values
  if (!is.data.frame(parsed_values)) {
    parsed_values <- snapshot$metadata$parsed_values
  }
  if (!is.data.frame(parsed_values)) {
    parsed_values <- tibble::tibble()
  }
  write_transmission_csv(
    parsed_values,
    file.path(audit_dir, "input", "parsed-curve.csv")
  )

  write_transmission_csv(
    transmission_completed_filter_export(snapshot),
    file.path(audit_dir, "filter", "completed-filter.csv")
  )
  write_transmission_csv(
    transmission_status_summary_from_filter(snapshot$filter),
    file.path(audit_dir, "filter", "completion-status.csv")
  )
  write_transmission_csv(
    tibble::tibble(
      wavelength_nm = snapshot$incident_spectrum$Wellenlaenge,
      spectral_irradiance_w_m2_nm = snapshot$incident_spectrum$Bestrahlungsstaerke
    ),
    file.path(audit_dir, "spectra", "incident-spectrum.csv")
  )
  write_transmission_csv(
    tibble::tibble(
      wavelength_nm = snapshot$transmitted_spectrum$Wellenlaenge,
      spectral_irradiance_w_m2_nm = snapshot$transmitted_spectrum$Bestrahlungsstaerke
    ),
    file.path(audit_dir, "spectra", "transmitted-spectrum.csv")
  )
  write_transmission_csv(
    transmission_spectral_comparison_export(snapshot),
    file.path(audit_dir, "spectra", "spectral-comparison.csv")
  )

  write_transmission_csv(
    transmission_d65_export(snapshot),
    file.path(audit_dir, "metrics", "d65-properties.csv")
  )
  write_transmission_csv(
    transmission_applied_metrics_export(snapshot),
    file.path(audit_dir, "metrics", "active-metrics.csv")
  )
  write_transmission_csv(
    snapshot$metrics,
    file.path(audit_dir, "metrics", "all-metrics.csv")
  )
  write_transmission_csv(
    transmission_history_table(history),
    file.path(audit_dir, "history", "history-tree.csv")
  )
  write_transmission_csv(
    transmission_history_spectra(history),
    file.path(audit_dir, "history", "node-spectra.csv")
  )

  metadata_without_record <- snapshot$metadata
  metadata_without_record$input_record <- NULL
  write_transmission_csv(
    transmission_flatten_metadata(metadata_without_record),
    file.path(audit_dir, "metadata", "applied-metadata.csv")
  )
  write_transmission_csv(
    transmission_decisions_warnings_export(snapshot),
    file.path(audit_dir, "metadata", "decisions-warnings.csv")
  )
  write_transmission_csv(
    transmission_citations_licenses_export(snapshot),
    file.path(audit_dir, "citations-licenses.csv")
  )

  payload_paths <- list.files(
    audit_dir,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  payload_paths <- payload_paths[!dir.exists(payload_paths)]
  relative_paths <- substring(payload_paths, nchar(audit_dir) + 2L)
  manifest <- tibble::tibble(
    file = relative_paths,
    bytes = as.numeric(file.info(payload_paths)$size),
    md5 = unname(tools::md5sum(payload_paths))
  )
  manifest <- manifest[order(manifest$file), , drop = FALSE]
  write_transmission_csv(manifest, file.path(audit_dir, "manifest.csv"))

  archive_paths <- c(relative_paths, "manifest.csv")
  zip::zipr(
    zipfile = file,
    files = archive_paths,
    root = audit_dir,
    include_directories = FALSE,
    mode = "mirror"
  )
  invisible(file)
}

#' Write an all-in-one user export bundle
#'
#' @param file Destination ZIP path.
#' @param snapshot Applied transmission snapshot.
#' @param history Session-local history tree.
#' @param active_state Current active-spectrum adapter.
#' @param show_transmittance_panel Include panel B in the result figure.
#' @param incident_fill Show the lighter incident fill.
#' @param response_curves Individually selected action-spectrum overlays.
#' @param contents Machine identifiers returned by
#'   `transmission_export_content_ids()`.
#' @param plot_width Figure width in inches.
#' @param plot_height Figure height in inches.
#' @param font_size Base figure font size.
#' @param max_irradiance Optional upper scale limit in mW/m2/nm.
#' @param plot_table_metric Metric group used in the combined figure/table PNG.
#'
#' @return The output path, invisibly.
#' @noRd
write_transmission_export_bundle <- function(
  file,
  snapshot,
  history,
  active_state,
  show_transmittance_panel = FALSE,
  incident_fill = TRUE,
  response_curves = character(),
  contents = transmission_export_content_ids(),
  plot_width = if (isTRUE(show_transmittance_panel)) 12 else 9,
  plot_height = 5.8,
  font_size = 15,
  max_irradiance = NULL,
  plot_table_metric = c("d65", "light", "balance")
) {
  validate_transmission_applied_snapshot(snapshot)
  validate_transmission_history(history)
  active_state <- as_transmission_active_spectrum(active_state)
  if (!requireNamespace("zip", quietly = TRUE)) {
    stop("The `zip` package is required to create an export bundle.")
  }
  plot_table_metric <- match.arg(plot_table_metric)
  contents <- intersect(unique(as.character(contents)),
    transmission_export_content_ids())
  if (length(contents) == 0L) {
    stop("Select at least one file for the export bundle.", call. = FALSE)
  }

  export_dir <- tempfile("spectran-transmission-export-")
  dir.create(export_dir, recursive = TRUE)
  on.exit(unlink(export_dir, recursive = TRUE, force = TRUE), add = TRUE)
  base <- transmission_filename_component(snapshot$metadata$filter_name)
  path <- function(suffix, extension) {
    file.path(export_dir, paste0(base, "-", suffix, ".", extension))
  }

  if ("result_plot" %in% contents) {
    write_transmission_result_plot(
      snapshot = snapshot,
      file = path("result-plot", "png"),
      show_transmittance_panel = show_transmittance_panel,
      incident_fill = incident_fill,
      response_curves = response_curves,
      width = plot_width,
      height = plot_height,
      font_size = font_size,
      max_irradiance = max_irradiance
    )
  }
  if ("filter_plot" %in% contents) {
    write_transmission_filter_plot(
      snapshot = snapshot,
      file = path("transmittance-spectrum", "png"),
      width = plot_width,
      height = plot_height,
      font_size = font_size
    )
  }
  if ("plot_table" %in% contents) {
    write_transmission_plot_table_png(
      snapshot = snapshot,
      file = path(paste0("result-with-", plot_table_metric, "-table"), "png"),
      metric_group = plot_table_metric,
      show_transmittance_panel = show_transmittance_panel,
      incident_fill = incident_fill,
      response_curves = response_curves,
      width = plot_width,
      height = plot_height,
      font_size = font_size,
      max_irradiance = max_irradiance
    )
  }
  if ("d65_png" %in% contents) {
    write_transmission_gt_png(
      transmission_d65_gt(snapshot),
      path("d65-properties-table", "png")
    )
  }
  if ("light_png" %in% contents) {
    write_transmission_gt_png(
      transmission_absolute_gt(snapshot),
      path("light-values-table", "png")
    )
  }
  if ("balance_png" %in% contents) {
    write_transmission_gt_png(
      transmission_balance_gt(snapshot),
      path("spectral-balance-table", "png")
    )
  }
  if ("completed_csv" %in% contents) {
    write_transmission_csv(
      transmission_completed_filter_export(snapshot),
      path("completed-filter", "csv")
    )
  }
  if ("comparison_csv" %in% contents) {
    write_transmission_csv(
      transmission_spectral_comparison_export(snapshot),
      path("spectral-comparison", "csv")
    )
  }
  if ("d65_csv" %in% contents) {
    write_transmission_csv(
      transmission_d65_export(snapshot),
      path("d65-properties", "csv")
    )
  }
  if ("light_csv" %in% contents) {
    write_transmission_csv(
      transmission_light_metrics_export(snapshot),
      path("light-values", "csv")
    )
  }
  if ("balance_csv" %in% contents) {
    write_transmission_csv(
      transmission_balance_metrics_export(snapshot),
      path("spectral-balance", "csv")
    )
  }
  if ("history_csv" %in% contents) {
    write_transmission_csv(
      transmission_history_table(history),
      path("history", "csv")
    )
  }
  if ("audit_zip" %in% contents) {
    write_transmission_audit_zip(
      file = path("audit", "zip"),
      snapshot = snapshot,
      history = history,
      active_state = active_state
    )
  }

  files <- list.files(export_dir, full.names = FALSE)
  zip::zipr(
    zipfile = file,
    files = files,
    root = export_dir,
    include_directories = FALSE,
    mode = "mirror"
  )
  invisible(file)
}

#' Count completed-filter statuses without a preparation object
#'
#' @param filter Completed filter.
#'
#' @return A status-count tibble.
#' @noRd
transmission_status_summary_from_filter <- function(filter) {
  filter <- as_completed_filter(filter)
  counts <- table(filter$status, useNA = "ifany")
  tibble::tibble(
    status = names(counts),
    status_label = transmission_status_label(names(counts)),
    samples = as.integer(counts)
  )
}
