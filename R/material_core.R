# Shared passive transmission and reflection model ------------------------

# Native reflection is exitance. The irradiance-shaped result retained by
# the existing analysis adapters is explicitly the receiver scenario F = 1.
material_mode <- function(snapshot = NULL, mode = NULL) {
  if (is.null(mode)) mode <- snapshot$material_mode
  if (is.null(mode)) mode <- snapshot$metadata$material_mode
  if (is.null(mode)) mode <- "transmission"
  match.arg(mode, c("transmission", "reflection"))
}

material_assumption <- function(mode) {
  if (material_mode(mode = mode) == "reflection") {
    paste(
      "Native output is reflected spectral radiant exitance M_lambda = E_lambda * rho_lambda.",
      "Receiver irradiance assumes F = 1: a uniformly illuminated Lambertian surface",
      "fills the receiver hemisphere. No room geometry, BRDF or automatic interreflection is calculated."
    )
  } else {
    paste(
      "Receiver irradiance assumes F = 1: all transmitted light represented by",
      "the measured coefficient reaches the receiver. Scattering and measurement",
      "geometry may require a different receiver scenario."
    )
  }
}

material_photopic_lux <- function(spectrum) {
  responses <- calculate_visible_spectrum_metrics(spectrum)$responses
  unname(responses$equivalent_illuminance_lx[
    responses$response_id == "photopic"
  ])
}

material_effective_der <- function(reference, outcome) {
  reference_metrics <- calculate_visible_spectrum_metrics(reference)
  outcome_metrics <- calculate_visible_spectrum_metrics(outcome)
  reference_lux <- material_photopic_lux(reference)
  medi <- function(x) {
    unname(x$responses$equivalent_illuminance_lx[
      x$responses$response_id == "melanopic"
    ])
  }
  before <- guarded_spectral_ratio(
    medi(reference_metrics),
    reference_lux,
    metric_label = "Reference MDER",
    denominator_label = "reference photopic illuminance"
  )
  after <- guarded_spectral_ratio(
    medi(outcome_metrics),
    reference_lux,
    metric_label = "Effective MDER",
    denominator_label = "reference photopic illuminance"
  )
  active_change_metric_row(
    metric_id = "melanopic_der_effective",
    metric_label = "Effective MDER (MEDI / reference Ev)",
    symbol = "MDEReff",
    response_id = "melanopic",
    incident_value = before$value,
    transmitted_value = after$value,
    incident_defined = before$defined,
    transmitted_defined = after$defined,
    incident_warning = before$warning,
    transmitted_warning = after$warning
  )
}

calculate_material_result <- function(incident, filter, mode = "transmission") {
  mode <- material_mode(mode = mode)
  result <- calculate_transmission_result(incident, filter)
  result$material_mode <- mode
  result$receiver_factor <- 1
  result$receiver_assumption <- material_assumption(mode)
  result$output_quantity <- if (mode == "reflection")
    "spectral_radiant_exitance" else "spectral_irradiance"
  result$native_output <- tibble::tibble(
    wavelength_nm = result$transmitted_spectrum$Wellenlaenge,
    value_w_m2_nm = result$transmitted_spectrum$Bestrahlungsstaerke,
    quantity = result$output_quantity
  )
  if (mode == "reflection") {
    result$d65_properties$metric_id <- sub(
      "^tau_",
      "rho_",
      result$d65_properties$metric_id
    )
    result$d65_properties$symbol <- gsub(
      "\u03c4",
      "\u03c1",
      result$d65_properties$symbol,
      fixed = TRUE
    )
    result$d65_properties$metric_label <- gsub(
      "transmittance",
      "reflectance",
      result$d65_properties$metric_label,
      fixed = TRUE
    )
  }
  result$active_metrics <- dplyr::bind_rows(
    result$active_metrics,
    material_effective_der(
      result$incident_spectrum,
      result$transmitted_spectrum
    )
  )
  result$metrics <- dplyr::bind_rows(
    result$d65_properties,
    result$active_metrics
  )
  result$warnings <- unique(result$metrics$warning[nzchar(
    result$metrics$warning
  )])
  result
}

# Keep the applied material snapshot immutable when a new receiver scenario
# is promoted. A zero target is valid; zero photopic output cannot be scaled
# to a positive target. Factors above one represent an explicit new scenario.
material_promotion <- function(snapshot, target_lux = NULL) {
  validate_transmission_applied_snapshot(snapshot)
  spectrum <- snapshot$transmitted_spectrum
  default_lux <- material_photopic_lux(spectrum)
  if (is.null(target_lux)) target_lux <- default_lux
  if (
    !is.numeric(target_lux) ||
      length(target_lux) != 1L ||
      !is.finite(target_lux) ||
      target_lux < 0
  ) {
    stop(
      "Target illuminance must be one finite, nonnegative number.",
      call. = FALSE
    )
  }
  if (
    !is.finite(default_lux) ||
      default_lux < 0 ||
      any(spectrum$Bestrahlungsstaerke < 0)
  ) {
    stop("Promotion requires a nonnegative irradiance spectrum.", call. = FALSE)
  }
  if (default_lux <= 0 && target_lux > 0) {
    stop(
      "A spectrum with zero photopic illuminance cannot be scaled to a positive target.",
      call. = FALSE
    )
  }
  same_target <- target_lux == default_lux ||
    (default_lux > 0 && abs(target_lux / default_lux - 1) < 1e-12)
  factor <- if (same_target) {
    1
  } else if (target_lux == 0) {
    0
  } else {
    target_lux / default_lux
  }
  spectrum$Bestrahlungsstaerke <- spectrum$Bestrahlungsstaerke * factor
  if (any(!is.finite(spectrum$Bestrahlungsstaerke))) {
    stop("The requested target produces nonfinite irradiance.", call. = FALSE)
  }
  list(
    spectrum = spectrum,
    provenance = list(
      material_mode = material_mode(snapshot),
      receiver_factor_default = 1,
      default_illuminance_lx = default_lux,
      target_illuminance_lx = target_lux,
      rescaling_factor = factor,
      explicitly_rescaled = !isTRUE(all.equal(factor, 1, tolerance = 0)),
      receiver_assumption = material_assumption(material_mode(snapshot))
    )
  )
}

# Follow parents, never chronological neighbours: restoring a branch must not
# include coefficients or overrides from any sibling branch.
material_history_path <- function(history, node_id = history$active_node_id) {
  validate_transmission_history(history)
  path <- character()
  while (
    !is.null(node_id) &&
      length(node_id) == 1L &&
      !is.na(node_id) &&
      nzchar(node_id)
  ) {
    if (node_id %in% path)
      stop("History contains a parent cycle.", call. = FALSE)
    node <- transmission_history_node(history, node_id)
    path <- c(node_id, path)
    node_id <- node$parent_id
  }
  path
}

calculate_material_cumulative <- function(
  history,
  node_id = history$active_node_id
) {
  path <- material_history_path(history, node_id)
  nodes <- history$nodes[path]
  original <- nodes[[1L]]$spectrum
  coefficient <- rep(1, nrow(original))
  steps <- purrr::map(nodes[-1L], function(node) {
    snapshot <- node$applied_snapshot
    validate_transmission_applied_snapshot(snapshot)
    tibble::tibble(
      node_id = node$node_id,
      parent_id = node$parent_id,
      mode = material_mode(snapshot),
      material = snapshot$metadata$filter_name,
      default_illuminance_lx = material_photopic_lux(
        snapshot$transmitted_spectrum
      ),
      target_illuminance_lx = material_photopic_lux(node$spectrum),
      rescaling_factor = if (is.null(node$provenance$rescaling_factor)) 1 else
        node$provenance$rescaling_factor
    )
  }) |>
    purrr::list_rbind()
  for (node in nodes[-1L])
    coefficient <- coefficient *
      as_completed_filter(node$applied_snapshot$filter)$transmittance
  material <- original
  material$Bestrahlungsstaerke <- original$Bestrahlungsstaerke * coefficient
  actual <- nodes[[length(nodes)]]$spectrum
  compare <- function(spectrum) {
    dplyr::bind_rows(
      compare_spectrum_metrics(original, spectrum)$metrics,
      material_effective_der(original, spectrum)
    )
  }
  list(
    root_id = path[[1L]],
    node_id = node_id,
    path = path,
    steps = steps,
    material_metrics = compare(material),
    actual_metrics = compare(actual),
    spectra = tibble::tibble(
      wavelength_nm = original$Wellenlaenge,
      original_irradiance_w_m2_nm = original$Bestrahlungsstaerke,
      cumulative_material_coefficient = coefficient,
      material_receiver_irradiance_f1_w_m2_nm = material$Bestrahlungsstaerke,
      actual_receiver_irradiance_w_m2_nm = actual$Bestrahlungsstaerke
    )
  )
}
