# Compare the same material under the applied source and standard daylight.
# Keep percentage points distinct from a relative percentage change.
material_coefficient_comparison <- function(snapshot) {
  validate_transmission_applied_snapshot(snapshot)
  properties <- snapshot$d65_properties
  active <- snapshot$active_metrics
  wanted <- ifelse(
    properties$response_id == "photopic",
    "photopic_illuminance",
    paste0(properties$response_id, "_irradiance")
  )
  active <- active[match(wanted, active$metric_id), , drop = FALSE]
  incident <- ifelse(
    active$comparison_defined,
    active$comparison_value,
    NA_real_
  )
  d65 <- ifelse(properties$defined, properties$transmitted_value, NA_real_)
  tibble::tibble(
    material_mode = material_mode(snapshot),
    response_id = properties$response_id,
    metric_id = sub("_d65$", "", properties$metric_id),
    metric_label = properties$metric_label,
    symbol = sub(",D65$", "", properties$symbol),
    incident_spectrum_name = snapshot$incident_name,
    incident_percent = 100 * incident,
    d65_percent = 100 * d65,
    difference_pp = 100 * (incident - d65),
    incident_defined = active$comparison_defined,
    d65_defined = properties$defined
  )
}

material_cumulative_has_rescaling <- function(cumulative) {
  nrow(cumulative$steps) > 0L && any(cumulative$steps$rescaling_factor != 1)
}

material_cumulative_gt <- function(metrics, title) {
  metrics <- metrics[
    !grepl("_action_factor$", metrics$metric_id),
    ,
    drop = FALSE
  ]
  data <- tibble::tibble(
    metric = transmission_metric_labels(
      metrics$metric_id,
      metrics$metric_label
    ),
    original = metrics$incident_value,
    outcome = metrics$transmitted_value,
    unit = metrics$unit,
    absolute_change = metrics$transmitted_value - metrics$incident_value,
    relative_change = metrics$relative_change
  )
  data |>
    gt::gt(rowname_col = "metric") |>
    gt::tab_header(title = title) |>
    gt::cols_label(
      original = transmission_text("table_incident"),
      outcome = if (transmission_language_setting() == "Deutsch")
        "Ergebnis" else "Outcome",
      unit = transmission_text("table_unit"),
      absolute_change = transmission_text("table_absolute_change"),
      relative_change = transmission_text("table_relative_change")
    ) |>
    transmission_gt_format_numbers(
      columns = c("original", "outcome", "absolute_change")
    ) |>
    gt::fmt_percent(
      columns = "relative_change",
      decimals = 1,
      locale = transmission_gt_locale()
    ) |>
    gt::sub_missing(missing_text = transmission_text("undefined")) |>
    gt::tab_source_note(
      source_note = material_text("effective_der_reference")
    ) |>
    transmission_gt_theme(variant = "analysis")
}

material_cumulative_export <- function(cumulative) {
  if (material_cumulative_has_rescaling(cumulative)) {
    return(tibble::tibble(
      status = "not_available_after_illuminance_rescaling",
      root_id = cumulative$root_id,
      node_id = cumulative$node_id,
      path = paste(cumulative$path, collapse = " > "),
      scaled_nodes = paste(
        cumulative$steps$node_id[cumulative$steps$rescaling_factor != 1],
        collapse = "; "
      ),
      note = "Cumulative metrics are shown only for branches without illuminance rescaling. Individual snapshots and spectra remain in the audit archive."
    ))
  }
  result <- cumulative$material_metrics |>
    dplyr::mutate(comparison = "material_only_f1") |>
    dplyr::filter(!grepl("_action_factor$", .data[["metric_id"]])) |>
    dplyr::mutate(
      root_id = cumulative$root_id,
      node_id = cumulative$node_id,
      path = paste(cumulative$path, collapse = " > ")
    )
  names(result)[names(result) == "transmitted_value"] <- "outcome_value"
  result
}

material_metric_export <- function(metrics, mode) {
  result <- tibble::as_tibble(metrics)
  if (mode == "reflection")
    names(result)[names(result) == "transmitted_value"] <- "outcome_value"
  result
}

#' Select the shared metric rows for displayed tables and their exports
#'
#' @param snapshot Immutable applied material snapshot.
#' @param group Either `"light"` or `"balance"`.
#'
#' @return Metric records in presentation order, without changing their values.
#' @noRd
material_result_metric_rows <- function(
  snapshot,
  group = c("light", "balance")
) {
  validate_transmission_applied_snapshot(snapshot)
  group <- match.arg(group)
  metrics <- snapshot$active_metrics
  ids <- metrics$metric_id
  rows <- if (group == "light") {
    c(which(ids == "photopic_illuminance"), which(grepl("_irradiance$", ids)))
  } else {
    c(which(grepl("_edi$", ids)), which(grepl("_der($|_effective$)", ids)))
  }
  metrics[rows, , drop = FALSE]
}
