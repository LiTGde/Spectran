completed_filter <- function(transmittance, status = "supplied") {
  if (length(transmittance) == 1L) {
    transmittance <- rep(transmittance, 401L)
  }
  tibble::tibble(
    wavelength_nm = 380:780,
    transmittance = transmittance,
    status = rep(status, 401L)
  )
}

positive_test_spectrum <- function() {
  tibble::tibble(
    Wellenlaenge = 380:780,
    Bestrahlungsstaerke = seq(0.0002, 0.001, length.out = 401L)
  )
}

test_that("shared response definitions preserve existing Analysis constants", {
  definitions <- spectral_response_definitions()

  expect_equal(nrow(definitions), 6L)
  expect_equal(definitions$action_spectrum, unname(Specs$Plot$Names))
  expect_equal(
    definitions$efficacy_lm_per_w,
    as.numeric(unlist(Specs$Efficacy))
  )

  spectrum <- positive_test_spectrum()
  for (sensitivity in definitions$action_spectrum) {
    direct <- sum(
      spectrum$Bestrahlungsstaerke * Specs$AS_wide[[sensitivity]]
    )
    expect_equal(Spec_int(spectrum, sensitivity), direct, tolerance = 0)
    expect_equal(
      Spec_wtd(spectrum, sensitivity),
      spectrum$Bestrahlungsstaerke * Specs$AS_wide[[sensitivity]],
      tolerance = 0
    )
  }
})

test_that("visible-spectrum and completed-filter contracts are strict", {
  spectrum <- positive_test_spectrum()
  filter <- completed_filter(0.5)

  expect_equal(nrow(as_visible_spectrum(spectrum)), 401L)
  expect_equal(nrow(as_completed_filter(filter)), 401L)
  expect_error(
    as_visible_spectrum(spectrum[-1, ]),
    "exactly one row"
  )
  expect_error(
    as_visible_spectrum(transform(spectrum, Bestrahlungsstaerke = NA_real_)),
    "non-finite irradiance"
  )
  expect_error(
    as_completed_filter(transform(filter, transmittance = 1.01)),
    "between 0 and 1"
  )
  expect_error(
    as_completed_filter(filter[, c("wavelength_nm", "transmittance")]),
    "missing required columns"
  )
})

test_that("identity filter preserves every defined metric", {
  spectrum <- positive_test_spectrum()
  result <- calculate_transmission_result(spectrum, completed_filter(1))

  expect_equal(
    result$transmitted_spectrum$Bestrahlungsstaerke,
    spectrum$Bestrahlungsstaerke,
    tolerance = 0
  )
  expect_equal(result$d65_properties$transmitted_value, rep(1, 6L))

  retained <- result$active_metrics$comparison_type == "retained"
  changed <- result$active_metrics$comparison_type == "change"
  expect_true(all(result$active_metrics$comparison_defined[retained]))
  expect_equal(result$active_metrics$comparison_value[retained], rep(1, 12L))
  expect_true(all(result$active_metrics$comparison_defined[changed]))
  expect_equal(result$active_metrics$absolute_change[changed], rep(0, 10L))
  expect_equal(result$active_metrics$relative_change[changed], rep(0, 10L))
  expect_length(result$warnings, 0L)
})

test_that("opaque filter returns zero light and explicit undefined balances", {
  spectrum <- positive_test_spectrum()
  result <- calculate_transmission_result(spectrum, completed_filter(0))

  expect_equal(result$transmitted_spectrum$Bestrahlungsstaerke, rep(0, 401L))
  expect_equal(result$d65_properties$transmitted_value, rep(0, 6L))

  retained <- result$active_metrics$comparison_type == "retained"
  changed <- result$active_metrics$comparison_type == "change"
  expect_equal(result$active_metrics$transmitted_value[retained], rep(0, 12L))
  expect_equal(result$active_metrics$comparison_value[retained], rep(0, 12L))
  expect_false(any(result$active_metrics$defined[changed]))
  expect_true(all(nzchar(result$active_metrics$warning[changed])))
  expect_true(any(grepl("photopic response denominator", result$warnings)))
})

test_that("neutral filter scales absolute values but not spectral balance", {
  spectrum <- positive_test_spectrum()
  result <- calculate_transmission_result(spectrum, completed_filter(0.37))
  retained <- result$active_metrics$comparison_type == "retained"
  changed <- result$active_metrics$comparison_type == "change"

  expect_equal(result$d65_properties$transmitted_value, rep(0.37, 6L))
  expect_equal(result$active_metrics$comparison_value[retained], rep(0.37, 12L))
  expect_equal(
    result$active_metrics$transmitted_value[retained],
    result$active_metrics$incident_value[retained] * 0.37
  )
  expect_equal(result$active_metrics$absolute_change[changed], rep(0, 10L))
  expect_equal(result$active_metrics$relative_change[changed], rep(0, 10L))
})

test_that("selective-filter metrics equal independent weighted sums", {
  spectrum <- positive_test_spectrum()
  transmission <- seq(0.05, 0.95, length.out = 401L)
  filter <- completed_filter(transmission)
  result <- calculate_transmission_result(spectrum, filter)
  d65 <- stats::approx(
    x = examplespectra$CIE$Wellenlaenge,
    y = examplespectra$CIE$D65,
    xout = 380:780,
    method = "linear",
    ties = "ordered"
  )$y

  d65_definitions <- spectral_response_definitions()[c(6L, 1:5), ]
  d65_direct <- vapply(
    d65_definitions$action_spectrum,
    function(sensitivity) {
      weighting <- Specs$AS_wide[[sensitivity]]
      sum(d65 * transmission * weighting) / sum(d65 * weighting)
    },
    numeric(1)
  )
  expect_equal(
    result$d65_properties$transmitted_value,
    unname(d65_direct),
    tolerance = 1e-15
  )

  photopic_weighting <- Specs$AS_wide[["V(lambda)"]]
  photopic_direct <- sum(
    spectrum$Bestrahlungsstaerke * transmission * photopic_weighting
  ) /
    sum(spectrum$Bestrahlungsstaerke * photopic_weighting)
  photopic <- result$active_metrics[
    result$active_metrics$metric_id == "photopic_illuminance",
  ]
  expect_equal(photopic$comparison_value, photopic_direct, tolerance = 1e-15)

  melanopic_weighting <- Specs$AS_wide[["Melanopsin"]]
  melanopic_incident_w_m2 <- sum(
    spectrum$Bestrahlungsstaerke * melanopic_weighting
  )
  melanopic_transmitted_w_m2 <- sum(
    spectrum$Bestrahlungsstaerke * transmission * melanopic_weighting
  )
  melanopic_irradiance <- result$active_metrics[
    result$active_metrics$metric_id == "melanopic_irradiance",
  ]
  melanopic_edi <- result$active_metrics[
    result$active_metrics$metric_id == "melanopic_edi",
  ]
  expect_equal(
    melanopic_irradiance$incident_value,
    melanopic_incident_w_m2 * 1000,
    tolerance = 1e-15
  )
  expect_equal(
    melanopic_irradiance$transmitted_value,
    melanopic_transmitted_w_m2 * 1000,
    tolerance = 1e-15
  )
  expect_equal(
    melanopic_edi$incident_value,
    melanopic_incident_w_m2 * Specs$Efficacy$melanopic,
    tolerance = 1e-15
  )

  photopic_incident_w_m2 <- sum(
    spectrum$Bestrahlungsstaerke * photopic_weighting
  )
  photopic_transmitted_w_m2 <- sum(
    spectrum$Bestrahlungsstaerke * transmission * photopic_weighting
  )
  action_factor <- result$active_metrics[
    result$active_metrics$metric_id == "melanopic_action_factor",
  ]
  expect_equal(
    action_factor$incident_value,
    melanopic_incident_w_m2 / photopic_incident_w_m2,
    tolerance = 1e-15
  )
  expect_equal(
    action_factor$transmitted_value,
    melanopic_transmitted_w_m2 / photopic_transmitted_w_m2,
    tolerance = 1e-15
  )
  expect_equal(
    action_factor$absolute_change,
    action_factor$transmitted_value - action_factor$incident_value,
    tolerance = 0
  )
  expect_equal(
    action_factor$relative_change,
    action_factor$absolute_change / action_factor$incident_value,
    tolerance = 0
  )
})

test_that("zero source and numerical guard produce visible undefined ratios", {
  zero <- tibble::tibble(
    Wellenlaenge = 380:780,
    Bestrahlungsstaerke = rep(0, 401L)
  )
  result <- calculate_transmission_result(zero, completed_filter(0.5))
  retained <- result$active_metrics$comparison_type == "retained"

  expect_equal(result$active_metrics$incident_value[retained], rep(0, 12L))
  expect_false(any(result$active_metrics$comparison_defined[retained]))
  expect_true(all(nzchar(result$active_metrics$warning[retained])))
  expect_true(any(grepl("machine precision", result$warnings)))

  above_guard <- weighted_transmittance(
    source = .Machine$double.eps * 2,
    transmission = 0.5,
    weighting = 1
  )
  below_guard <- weighted_transmittance(
    source = .Machine$double.eps / 2,
    transmission = 0.5,
    weighting = 1
  )
  non_finite <- weighted_transmittance(
    source = 1,
    transmission = 0.5,
    weighting = Inf
  )
  expect_true(above_guard$defined)
  expect_equal(above_guard$value, 0.5)
  expect_false(below_guard$defined)
  expect_false(non_finite$defined)
  expect_match(non_finite$warning, "non-finite")
})

test_that("combined metrics and applied snapshot satisfy internal contracts", {
  result <- calculate_transmission_result(
    positive_test_spectrum(),
    completed_filter(0.5)
  )
  required_metrics <- c(
    "scope",
    "metric_id",
    "incident_value",
    "transmitted_value",
    "unit",
    "comparison_type",
    "comparison_value",
    "defined",
    "warning"
  )
  expect_equal(nrow(result$d65_properties), 6L)
  expect_equal(nrow(result$active_metrics), 22L)
  expect_equal(nrow(result$metrics), 28L)
  expect_true(all(required_metrics %in% names(result$metrics)))

  snapshot <- new_transmission_applied_snapshot(
    result = result,
    metadata = list(filter_name = "Neutral 50%"),
    incident_name = "Test source",
    draft_revision = 7L,
    apply_sequence = 2L
  )
  expect_s3_class(snapshot, "transmission_applied_snapshot")
  expect_equal(snapshot$draft_revision, 7L)
  expect_equal(snapshot$apply_sequence, 2L)
  expect_equal(snapshot$incident_name, "Test source")
  expect_equal(snapshot$filter$transmittance, rep(0.5, 401L))
})
