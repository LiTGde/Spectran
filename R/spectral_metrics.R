# Shared visible-spectrum calculations ------------------------------------

#' Describe Spectran's photoreceptor and photopic response functions
#'
#' @return A 6-row tibble containing stable response identifiers, labels,
#'   action-spectrum columns, and efficacy constants.
#' @noRd
spectral_response_definitions <- function() {
  action_columns <- c(
    "Melanopsin",
    "L-cone-opsin",
    "M-cone-opsin",
    "S-cone-opsin",
    "Rhodopsin",
    "V(lambda)"
  )
  efficacy_names <- c(
    "melanopic",
    "L-cone-opic",
    "M-cone-opic",
    "S-cone-opic",
    "rhodopic",
    "photopic"
  )

  missing_actions <- setdiff(action_columns, names(Specs$AS_wide))
  missing_efficacies <- setdiff(efficacy_names, names(Specs$Efficacy))
  if (length(missing_actions) > 0L || length(missing_efficacies) > 0L) {
    stop(
      "Spectran's action spectra or efficacy constants are incomplete.",
      call. = FALSE
    )
  }

  tibble::tibble(
    response_id = c(
      "melanopic",
      "l_cone_opic",
      "m_cone_opic",
      "s_cone_opic",
      "rhodopic",
      "photopic"
    ),
    response_label = c(
      "Melanopic",
      "L-cone-opic",
      "M-cone-opic",
      "S-cone-opic",
      "Rhodopic",
      "Photopic"
    ),
    action_spectrum = action_columns,
    efficacy_name = efficacy_names,
    efficacy_lm_per_w = as.numeric(unlist(Specs$Efficacy[efficacy_names])),
    is_alpha_opic = c(rep(TRUE, 5L), FALSE)
  )
}

#' Validate and normalize Spectran's active visible-spectrum contract
#'
#' @param spectrum A data frame containing `Wellenlaenge` and
#'   `Bestrahlungsstaerke`.
#' @param arg Argument label used in errors.
#'
#' @return A 401-row tibble on the 380 to 780 nm grid.
#' @noRd
as_visible_spectrum <- function(spectrum, arg = "spectrum") {
  if (!is.data.frame(spectrum)) {
    stop("`", arg, "` must be a data frame.", call. = FALSE)
  }
  required <- c("Wellenlaenge", "Bestrahlungsstaerke")
  missing_columns <- setdiff(required, names(spectrum))
  if (length(missing_columns) > 0L) {
    stop(
      "`",
      arg,
      "` is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if (
    !is.numeric(spectrum$Wellenlaenge) ||
      !is.numeric(spectrum$Bestrahlungsstaerke)
  ) {
    stop(
      "`",
      arg,
      "` wavelength and irradiance columns must be numeric.",
      call. = FALSE
    )
  }
  if (
    nrow(spectrum) != 401L ||
      !identical(as.numeric(spectrum$Wellenlaenge), as.numeric(380:780))
  ) {
    stop(
      "`",
      arg,
      "` must contain exactly one row for every wavelength from 380 to 780 nm.",
      call. = FALSE
    )
  }
  if (any(!is.finite(spectrum$Bestrahlungsstaerke))) {
    affected <- which(!is.finite(spectrum$Bestrahlungsstaerke))
    stop(
      "`",
      arg,
      "` contains missing or non-finite irradiance at row(s) ",
      paste(utils::head(affected, 5L), collapse = ", "),
      if (length(affected) > 5L) {
        paste0(", and ", length(affected) - 5L, " more")
      } else {
        ""
      },
      ".",
      call. = FALSE
    )
  }

  tibble::tibble(
    Wellenlaenge = as.numeric(spectrum$Wellenlaenge),
    Bestrahlungsstaerke = as.numeric(spectrum$Bestrahlungsstaerke)
  )
}

#' Validate the completed transmission-filter contract
#'
#' @param filter A data frame containing `wavelength_nm`, `transmittance`, and
#'   `status`.
#' @param arg Argument label used in errors.
#'
#' @return A validated 401-row tibble.
#' @noRd
as_completed_filter <- function(filter, arg = "filter") {
  if (!is.data.frame(filter)) {
    stop("`", arg, "` must be a data frame.", call. = FALSE)
  }
  required <- c("wavelength_nm", "transmittance", "status")
  missing_columns <- setdiff(required, names(filter))
  if (length(missing_columns) > 0L) {
    stop(
      "`",
      arg,
      "` is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if (!is.numeric(filter$wavelength_nm) || !is.numeric(filter$transmittance)) {
    stop(
      "`",
      arg,
      "` wavelength and transmittance columns must be numeric.",
      call. = FALSE
    )
  }
  if (
    nrow(filter) != 401L ||
      !identical(as.numeric(filter$wavelength_nm), as.numeric(380:780))
  ) {
    stop(
      "`",
      arg,
      "` must contain exactly one row for every wavelength from 380 to 780 nm.",
      call. = FALSE
    )
  }
  invalid <- !is.finite(filter$transmittance) |
    filter$transmittance < 0 |
    filter$transmittance > 1
  if (any(invalid)) {
    affected <- which(invalid)
    stop(
      "`",
      arg,
      "` transmittance must be finite and between 0 and 1. Invalid row(s): ",
      paste(utils::head(affected, 5L), collapse = ", "),
      if (length(affected) > 5L) {
        paste0(", and ", length(affected) - 5L, " more")
      } else {
        ""
      },
      ".",
      call. = FALSE
    )
  }
  if (any(is.na(filter$status)) || any(!nzchar(as.character(filter$status)))) {
    stop("`", arg, "` status values must be non-missing text.", call. = FALSE)
  }

  tibble::tibble(
    wavelength_nm = as.numeric(filter$wavelength_nm),
    transmittance = as.numeric(filter$transmittance),
    status = as.character(filter$status)
  )
}

#' Return one of Spectran's authoritative action spectra
#'
#' @param sensitivity Column name in `Specs$AS_wide`.
#'
#' @return A numeric vector with 401 response values.
#' @noRd
spectral_action_weighting <- function(sensitivity) {
  if (
    !is.character(sensitivity) ||
      length(sensitivity) != 1L ||
      is.na(sensitivity) ||
      !sensitivity %in% names(Specs$AS_wide)
  ) {
    stop(
      "`sensitivity` must name one action spectrum in `Specs$AS_wide`.",
      call. = FALSE
    )
  }
  weighting <- Specs$AS_wide[[sensitivity]]
  if (
    !is.numeric(weighting) ||
      length(weighting) != 401L ||
      any(!is.finite(weighting))
  ) {
    stop("The selected action spectrum is invalid.", call. = FALSE)
  }
  as.numeric(weighting)
}

#' Weight a spectrum with one Spectran response function
#'
#' @param spectrum Spectran active-spectrum data.
#' @param sensitivity Column name in `Specs$AS_wide`.
#'
#' @return A 401-element numeric vector.
#' @noRd
spectral_weighted_values <- function(spectrum, sensitivity) {
  spectrum <- as_visible_spectrum(spectrum)
  spectrum$Bestrahlungsstaerke * spectral_action_weighting(sensitivity)
}

#' Safely divide spectral quantities using only a machine-precision guard
#'
#' @param numerator Numeric numerator.
#' @param denominator Numeric denominator.
#' @param metric_label Human-readable metric label used in warnings.
#' @param denominator_label Human-readable denominator description.
#'
#' @return A one-row tibble with value, defined status, and warning.
#' @noRd
guarded_spectral_ratio <- function(
  numerator,
  denominator,
  metric_label,
  denominator_label = "denominator"
) {
  if (
    !is.numeric(numerator) ||
      length(numerator) != 1L ||
      !is.numeric(denominator) ||
      length(denominator) != 1L
  ) {
    stop("`numerator` and `denominator` must be single numbers.", call. = FALSE)
  }

  warning <- ""
  defined <- TRUE
  value <- NA_real_

  if (!is.finite(denominator)) {
    defined <- FALSE
    warning <- paste0(
      metric_label,
      " is undefined because its ",
      denominator_label,
      " is non-finite."
    )
  } else if (abs(denominator) <= .Machine$double.eps) {
    defined <- FALSE
    warning <- paste0(
      metric_label,
      " is undefined because its ",
      denominator_label,
      " is zero or within machine precision of zero."
    )
  } else if (!is.finite(numerator)) {
    defined <- FALSE
    warning <- paste0(
      metric_label,
      " is undefined because its numerator is non-finite."
    )
  } else {
    value <- numerator / denominator
    if (!is.finite(value)) {
      defined <- FALSE
      value <- NA_real_
      warning <- paste0(
        metric_label,
        " is undefined because the ratio is non-finite."
      )
    }
  }

  tibble::tibble(
    value = value,
    defined = defined,
    warning = warning,
    numerator = as.numeric(numerator),
    denominator = as.numeric(denominator)
  )
}

#' Calculate weighted transmittance
#'
#' @param source Incident spectral values.
#' @param transmission Filter transmittance fractions.
#' @param weighting Spectral weighting values.
#' @param metric_label Human-readable metric label used in warnings.
#'
#' @return A one-row tibble with the weighted transmittance and audit terms.
#' @noRd
weighted_transmittance <- function(
  source,
  transmission,
  weighting = rep(1, length(source)),
  metric_label = "Weighted transmittance"
) {
  inputs <- list(
    source = source,
    transmission = transmission,
    weighting = weighting
  )
  invalid_type <- vapply(
    inputs,
    function(value) !is.numeric(value),
    logical(1)
  )
  if (any(invalid_type)) {
    stop(
      "`source`, `transmission`, and `weighting` must be numeric vectors.",
      call. = FALSE
    )
  }
  lengths <- vapply(inputs, length, integer(1))
  if (length(unique(lengths)) != 1L || lengths[[1]] == 0L) {
    stop(
      "`source`, `transmission`, and `weighting` must have the same positive length.",
      call. = FALSE
    )
  }
  if (any(!is.finite(source)) || any(!is.finite(transmission))) {
    stop(
      "`source` and `transmission` must contain finite values.",
      call. = FALSE
    )
  }
  if (any(transmission < 0 | transmission > 1)) {
    stop("`transmission` must be between 0 and 1.", call. = FALSE)
  }

  denominator <- sum(source * weighting)
  numerator <- sum(source * transmission * weighting)
  guarded_spectral_ratio(
    numerator = numerator,
    denominator = denominator,
    metric_label = metric_label,
    denominator_label = "incident weighted-response denominator"
  )
}

#' Return bundled CIE D65 on Spectran's 1 nm calculation grid
#'
#' @return A 401-row Spectran spectrum tibble.
#' @noRd
d65_visible_spectrum <- function() {
  required <- c("Wellenlaenge", "D65")
  if (!all(required %in% names(examplespectra$CIE))) {
    stop(
      "The bundled CIE D65 reference spectrum is unavailable.",
      call. = FALSE
    )
  }
  raw <- examplespectra$CIE[, required, drop = FALSE]
  interpolated <- stats::approx(
    x = raw$Wellenlaenge,
    y = raw$D65,
    xout = 380:780,
    method = "linear",
    ties = "ordered",
    rule = 1
  )$y
  if (length(interpolated) != 401L || any(!is.finite(interpolated))) {
    stop(
      "The bundled CIE D65 reference spectrum cannot cover 380 to 780 nm.",
      call. = FALSE
    )
  }
  tibble::tibble(
    Wellenlaenge = 380:780,
    Bestrahlungsstaerke = as.numeric(interpolated)
  )
}

#' Calculate response-weighted values for one visible spectrum
#'
#' @param spectrum A Spectran active spectrum.
#'
#' @return A list containing total irradiance, response values, alpha-opic
#'   action factors and DERs, and warnings.
#' @noRd
calculate_visible_spectrum_metrics <- function(spectrum) {
  spectrum <- as_visible_spectrum(spectrum)
  definitions <- spectral_response_definitions()
  irradiance <- spectrum$Bestrahlungsstaerke

  response_w_m2 <- vapply(
    definitions$action_spectrum,
    function(sensitivity) {
      sum(irradiance * spectral_action_weighting(sensitivity))
    },
    numeric(1)
  )
  response_lx <- response_w_m2 * definitions$efficacy_lm_per_w
  responses <- definitions
  responses$response_irradiance_w_m2 <- response_w_m2
  responses$response_irradiance_mw_m2 <- response_w_m2 * 1000
  responses$equivalent_illuminance_lx <- response_lx

  photopic_index <- match("photopic", responses$response_id)
  alpha_indices <- which(responses$is_alpha_opic)
  balance <- lapply(alpha_indices, function(index) {
    action_ratio <- guarded_spectral_ratio(
      numerator = responses$response_irradiance_w_m2[[index]],
      denominator = responses$response_irradiance_w_m2[[photopic_index]],
      metric_label = paste(responses$response_label[[index]], "action factor"),
      denominator_label = "photopic response denominator"
    )
    der_ratio <- guarded_spectral_ratio(
      numerator = responses$equivalent_illuminance_lx[[index]],
      denominator = responses$equivalent_illuminance_lx[[photopic_index]],
      metric_label = paste(responses$response_label[[index]], "DER"),
      denominator_label = "photopic illuminance denominator"
    )
    tibble::tibble(
      response_id = responses$response_id[[index]],
      response_label = responses$response_label[[index]],
      action_factor = action_ratio$value,
      action_factor_defined = action_ratio$defined,
      action_factor_warning = action_ratio$warning,
      der = der_ratio$value,
      der_defined = der_ratio$defined,
      der_warning = der_ratio$warning
    )
  })
  balance <- do.call(rbind, balance)
  balance <- tibble::as_tibble(balance)

  warnings <- unique(c(
    balance$action_factor_warning,
    balance$der_warning
  ))
  warnings <- warnings[nzchar(warnings)]

  list(
    total_irradiance_w_m2 = sum(irradiance),
    total_irradiance_mw_m2 = sum(irradiance) * 1000,
    responses = responses,
    alpha_balance = balance,
    warnings = warnings
  )
}

#' Calculate D65-referenced transmission properties
#'
#' @param filter A completed transmission filter.
#'
#' @return A 6-row long-form metric tibble.
#' @noRd
calculate_d65_filter_properties <- function(filter) {
  filter <- as_completed_filter(filter)
  d65 <- d65_visible_spectrum()
  definitions <- spectral_response_definitions()
  order <- c(6L, 1:5)
  definitions <- definitions[order, , drop = FALSE]

  metric_ids <- c(
    photopic = "tau_v_d65",
    melanopic = "tau_mel_d65",
    l_cone_opic = "tau_l_cone_d65",
    m_cone_opic = "tau_m_cone_d65",
    s_cone_opic = "tau_s_cone_d65",
    rhodopic = "tau_rh_d65"
  )
  symbols <- c(
    photopic = "\u03c4v,D65",
    melanopic = "\u03c4mel,D65",
    l_cone_opic = "\u03c4L,D65",
    m_cone_opic = "\u03c4M,D65",
    s_cone_opic = "\u03c4S,D65",
    rhodopic = "\u03c4rh,D65"
  )
  labels <- c(
    photopic = "Luminous transmittance",
    melanopic = "Melanopic transmittance",
    l_cone_opic = "L-cone-opic transmittance",
    m_cone_opic = "M-cone-opic transmittance",
    s_cone_opic = "S-cone-opic transmittance",
    rhodopic = "Rhodopic transmittance"
  )

  rows <- lapply(seq_len(nrow(definitions)), function(index) {
    response_id <- definitions$response_id[[index]]
    ratio <- weighted_transmittance(
      source = d65$Bestrahlungsstaerke,
      transmission = filter$transmittance,
      weighting = spectral_action_weighting(
        definitions$action_spectrum[[index]]
      ),
      metric_label = paste0(labels[[response_id]], " referenced to D65")
    )
    tibble::tibble(
      scope = "d65_filter",
      metric_id = unname(metric_ids[[response_id]]),
      metric_label = unname(labels[[response_id]]),
      symbol = unname(symbols[[response_id]]),
      response_id = response_id,
      incident_value = 1,
      transmitted_value = ratio$value,
      unit = "fraction",
      comparison_type = "retained",
      comparison_value = ratio$value,
      absolute_change = if (ratio$defined) ratio$value - 1 else NA_real_,
      relative_change = if (ratio$defined) ratio$value - 1 else NA_real_,
      defined = ratio$defined,
      comparison_defined = ratio$defined,
      relative_change_defined = ratio$defined,
      warning = ratio$warning,
      numerator = ratio$numerator,
      denominator = ratio$denominator
    )
  })
  result <- do.call(rbind, rows)
  tibble::as_tibble(result)
}

#' Create one active-source retained metric row
#'
#' @param metric_id Stable metric identifier.
#' @param metric_label Human-readable metric label.
#' @param symbol Plain-text scientific symbol.
#' @param response_id Response identifier or `"radiant"`.
#' @param incident_value Incident absolute value.
#' @param transmitted_value Transmitted absolute value.
#' @param unit Display unit.
#' @param retained Weighted retained-proportion record.
#'
#' @return A one-row long-form metric tibble.
#' @noRd
active_retained_metric_row <- function(
  metric_id,
  metric_label,
  symbol,
  response_id,
  incident_value,
  transmitted_value,
  unit,
  retained
) {
  values_defined <- all(is.finite(c(incident_value, transmitted_value)))
  value_warning <- if (values_defined) {
    ""
  } else {
    paste0(
      metric_label,
      " is undefined because an absolute value is non-finite."
    )
  }
  warning <- paste(
    c(value_warning, retained$warning)[nzchar(c(
      value_warning,
      retained$warning
    ))],
    collapse = " "
  )

  tibble::tibble(
    scope = "active_source",
    metric_id = metric_id,
    metric_label = metric_label,
    symbol = symbol,
    response_id = response_id,
    incident_value = if (values_defined) incident_value else NA_real_,
    transmitted_value = if (values_defined) transmitted_value else NA_real_,
    unit = unit,
    comparison_type = "retained",
    comparison_value = retained$value,
    absolute_change = if (values_defined) {
      transmitted_value - incident_value
    } else {
      NA_real_
    },
    relative_change = if (retained$defined) retained$value - 1 else NA_real_,
    defined = values_defined,
    comparison_defined = retained$defined,
    relative_change_defined = retained$defined,
    warning = warning,
    numerator = retained$numerator,
    denominator = retained$denominator
  )
}

#' Create one action-factor or DER change metric row
#'
#' @param metric_id Stable metric identifier.
#' @param metric_label Human-readable metric label.
#' @param symbol Plain-text scientific symbol.
#' @param response_id Alpha-opic response identifier.
#' @param incident_value Incident balance metric.
#' @param transmitted_value Transmitted balance metric.
#' @param incident_defined Whether the incident value is defined.
#' @param transmitted_defined Whether the transmitted value is defined.
#' @param incident_warning Warning for the incident value.
#' @param transmitted_warning Warning for the transmitted value.
#'
#' @return A one-row long-form metric tibble.
#' @noRd
active_change_metric_row <- function(
  metric_id,
  metric_label,
  symbol,
  response_id,
  incident_value,
  transmitted_value,
  incident_defined,
  transmitted_defined,
  incident_warning,
  transmitted_warning
) {
  values_defined <- isTRUE(incident_defined) &&
    isTRUE(transmitted_defined) &&
    all(is.finite(c(incident_value, transmitted_value)))
  absolute_change <- if (values_defined) {
    transmitted_value - incident_value
  } else {
    NA_real_
  }
  relative <- if (values_defined) {
    guarded_spectral_ratio(
      numerator = absolute_change,
      denominator = incident_value,
      metric_label = paste(metric_label, "relative change"),
      denominator_label = "incident metric value"
    )
  } else {
    tibble::tibble(
      value = NA_real_,
      defined = FALSE,
      warning = "",
      numerator = NA_real_,
      denominator = incident_value
    )
  }
  warning <- paste(
    unique(c(incident_warning, transmitted_warning, relative$warning))[
      nzchar(unique(c(incident_warning, transmitted_warning, relative$warning)))
    ],
    collapse = " "
  )

  tibble::tibble(
    scope = "active_source",
    metric_id = metric_id,
    metric_label = metric_label,
    symbol = symbol,
    response_id = response_id,
    incident_value = if (isTRUE(incident_defined)) incident_value else NA_real_,
    transmitted_value = if (isTRUE(transmitted_defined)) {
      transmitted_value
    } else {
      NA_real_
    },
    unit = "",
    comparison_type = "change",
    comparison_value = absolute_change,
    absolute_change = absolute_change,
    relative_change = relative$value,
    defined = values_defined,
    comparison_defined = values_defined,
    relative_change_defined = relative$defined,
    warning = warning,
    numerator = NA_real_,
    denominator = incident_value
  )
}

#' Calculate incident and transmitted light metrics
#'
#' @param incident A Spectran active spectrum.
#' @param filter A completed transmission filter.
#'
#' @return A list with transmitted spectrum, long-form active-source metrics,
#'   and warnings.
#' @noRd
calculate_active_transmission_metrics <- function(incident, filter) {
  incident <- as_visible_spectrum(incident, arg = "incident")
  filter <- as_completed_filter(filter)
  transmitted <- tibble::tibble(
    Wellenlaenge = incident$Wellenlaenge,
    Bestrahlungsstaerke = incident$Bestrahlungsstaerke * filter$transmittance
  )
  incident_metrics <- calculate_visible_spectrum_metrics(incident)
  transmitted_metrics <- calculate_visible_spectrum_metrics(transmitted)
  definitions <- spectral_response_definitions()

  energy_retained <- weighted_transmittance(
    source = incident$Bestrahlungsstaerke,
    transmission = filter$transmittance,
    weighting = rep(1, 401L),
    metric_label = "Active-source radiant-energy retained proportion"
  )
  rows <- list(active_retained_metric_row(
    metric_id = "total_irradiance",
    metric_label = "Total irradiance",
    symbol = "Ee",
    response_id = "radiant",
    incident_value = incident_metrics$total_irradiance_mw_m2,
    transmitted_value = transmitted_metrics$total_irradiance_mw_m2,
    unit = "mW/m\u00b2",
    retained = energy_retained
  ))

  photopic_index <- match("photopic", definitions$response_id)
  photopic_retained <- weighted_transmittance(
    source = incident$Bestrahlungsstaerke,
    transmission = filter$transmittance,
    weighting = spectral_action_weighting(
      definitions$action_spectrum[[photopic_index]]
    ),
    metric_label = "Active-source photopic retained proportion"
  )
  rows[[length(rows) + 1L]] <- active_retained_metric_row(
    metric_id = "photopic_illuminance",
    metric_label = "Photopic illuminance",
    symbol = "Ev",
    response_id = "photopic",
    incident_value = incident_metrics$responses$equivalent_illuminance_lx[[
      photopic_index
    ]],
    transmitted_value = transmitted_metrics$responses$equivalent_illuminance_lx[[
      photopic_index
    ]],
    unit = "lx",
    retained = photopic_retained
  )

  alpha_definitions <- definitions[definitions$is_alpha_opic, , drop = FALSE]
  alpha_symbols <- c(
    melanopic = "mel",
    l_cone_opic = "L",
    m_cone_opic = "M",
    s_cone_opic = "S",
    rhodopic = "rh"
  )
  for (index in seq_len(nrow(alpha_definitions))) {
    definition <- alpha_definitions[index, , drop = FALSE]
    response_id <- definition$response_id[[1]]
    incident_index <- match(
      response_id,
      incident_metrics$responses$response_id
    )
    transmitted_index <- match(
      response_id,
      transmitted_metrics$responses$response_id
    )
    retained <- weighted_transmittance(
      source = incident$Bestrahlungsstaerke,
      transmission = filter$transmittance,
      weighting = spectral_action_weighting(definition$action_spectrum[[1]]),
      metric_label = paste0(
        "Active-source ",
        tolower(definition$response_label[[1]]),
        " retained proportion"
      )
    )
    symbol <- unname(alpha_symbols[[response_id]])

    rows[[length(rows) + 1L]] <- active_retained_metric_row(
      metric_id = paste0(response_id, "_irradiance"),
      metric_label = paste(definition$response_label[[1]], "irradiance"),
      symbol = paste0("Ee,", symbol),
      response_id = response_id,
      incident_value = incident_metrics$responses$response_irradiance_mw_m2[[
        incident_index
      ]],
      transmitted_value = transmitted_metrics$responses$response_irradiance_mw_m2[[
        transmitted_index
      ]],
      unit = "mW/m\u00b2",
      retained = retained
    )
    rows[[length(rows) + 1L]] <- active_retained_metric_row(
      metric_id = paste0(response_id, "_edi"),
      metric_label = paste(definition$response_label[[1]], "EDI"),
      symbol = paste0("Ev,", symbol, ",D65"),
      response_id = response_id,
      incident_value = incident_metrics$responses$equivalent_illuminance_lx[[
        incident_index
      ]],
      transmitted_value = transmitted_metrics$responses$equivalent_illuminance_lx[[
        transmitted_index
      ]],
      unit = "lx",
      retained = retained
    )

    incident_balance_index <- match(
      response_id,
      incident_metrics$alpha_balance$response_id
    )
    transmitted_balance_index <- match(
      response_id,
      transmitted_metrics$alpha_balance$response_id
    )
    rows[[length(rows) + 1L]] <- active_change_metric_row(
      metric_id = paste0(response_id, "_action_factor"),
      metric_label = paste(definition$response_label[[1]], "action factor"),
      symbol = paste0("a", symbol, ",v"),
      response_id = response_id,
      incident_value = incident_metrics$alpha_balance$action_factor[[
        incident_balance_index
      ]],
      transmitted_value = transmitted_metrics$alpha_balance$action_factor[[
        transmitted_balance_index
      ]],
      incident_defined = incident_metrics$alpha_balance$action_factor_defined[[
        incident_balance_index
      ]],
      transmitted_defined = transmitted_metrics$alpha_balance$action_factor_defined[[
        transmitted_balance_index
      ]],
      incident_warning = incident_metrics$alpha_balance$action_factor_warning[[
        incident_balance_index
      ]],
      transmitted_warning = transmitted_metrics$alpha_balance$action_factor_warning[[
        transmitted_balance_index
      ]]
    )
    rows[[length(rows) + 1L]] <- active_change_metric_row(
      metric_id = paste0(response_id, "_der"),
      metric_label = paste(definition$response_label[[1]], "DER"),
      symbol = paste0("\u03b3", symbol, ",v,D65"),
      response_id = response_id,
      incident_value = incident_metrics$alpha_balance$der[[
        incident_balance_index
      ]],
      transmitted_value = transmitted_metrics$alpha_balance$der[[
        transmitted_balance_index
      ]],
      incident_defined = incident_metrics$alpha_balance$der_defined[[
        incident_balance_index
      ]],
      transmitted_defined = transmitted_metrics$alpha_balance$der_defined[[
        transmitted_balance_index
      ]],
      incident_warning = incident_metrics$alpha_balance$der_warning[[
        incident_balance_index
      ]],
      transmitted_warning = transmitted_metrics$alpha_balance$der_warning[[
        transmitted_balance_index
      ]]
    )
  }

  metrics <- do.call(rbind, rows)
  metrics <- tibble::as_tibble(metrics)
  warnings <- unique(metrics$warning[nzchar(metrics$warning)])

  list(
    incident_spectrum = incident,
    transmitted_spectrum = transmitted,
    metrics = metrics,
    warnings = warnings,
    incident_metrics = incident_metrics,
    transmitted_metrics = transmitted_metrics
  )
}

#' Calculate a complete transmission result
#'
#' @param incident A Spectran active spectrum.
#' @param filter A completed transmission filter.
#'
#' @return A list with immutable calculation inputs, spectra, D65 properties,
#'   active-source metrics, combined metrics, and warnings.
#' @noRd
calculate_transmission_result <- function(incident, filter) {
  incident <- as_visible_spectrum(incident, arg = "incident")
  filter <- as_completed_filter(filter)
  d65_properties <- calculate_d65_filter_properties(filter)
  active <- calculate_active_transmission_metrics(incident, filter)
  metrics <- rbind(d65_properties, active$metrics)
  metrics <- tibble::as_tibble(metrics)
  warnings <- unique(metrics$warning[nzchar(metrics$warning)])

  list(
    incident_spectrum = incident,
    filter = filter,
    transmitted_spectrum = active$transmitted_spectrum,
    d65_properties = d65_properties,
    active_metrics = active$metrics,
    metrics = metrics,
    warnings = warnings
  )
}

#' Freeze one applied transmission snapshot
#'
#' @param result Output from `calculate_transmission_result()`.
#' @param metadata Transmission metadata copied at Apply time.
#' @param incident_name Name of the active source spectrum.
#' @param draft_revision Draft revision copied at Apply time.
#' @param apply_sequence Session-local Apply event sequence.
#'
#' @return A `transmission_applied_snapshot` list.
#' @noRd
new_transmission_applied_snapshot <- function(
  result,
  metadata,
  incident_name,
  draft_revision,
  apply_sequence
) {
  if (
    !is.list(result) ||
      !all(
        c(
          "incident_spectrum",
          "filter",
          "transmitted_spectrum",
          "d65_properties",
          "active_metrics",
          "metrics",
          "warnings"
        ) %in%
          names(result)
      )
  ) {
    stop("`result` must be a complete transmission result.", call. = FALSE)
  }
  if (!is.list(metadata)) {
    stop("`metadata` must be a list.", call. = FALSE)
  }
  if (
    !is.character(incident_name) ||
      length(incident_name) != 1L ||
      is.na(incident_name) ||
      !nzchar(trimws(incident_name))
  ) {
    stop("`incident_name` must be non-empty text.", call. = FALSE)
  }
  if (
    !is.numeric(draft_revision) ||
      length(draft_revision) != 1L ||
      !is.finite(draft_revision)
  ) {
    stop("`draft_revision` must be one finite number.", call. = FALSE)
  }
  if (
    !is.numeric(apply_sequence) ||
      length(apply_sequence) != 1L ||
      !is.finite(apply_sequence)
  ) {
    stop("`apply_sequence` must be one finite number.", call. = FALSE)
  }

  structure(
    c(
      result,
      list(
        metadata = metadata,
        incident_name = trimws(incident_name),
        draft_revision = as.integer(draft_revision),
        apply_sequence = as.integer(apply_sequence)
      )
    ),
    class = c("transmission_applied_snapshot", "list")
  )
}
