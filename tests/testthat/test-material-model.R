material_test_snapshot <- function(
  source,
  mode = "reflection",
  coefficient = 0.5,
  sequence = 1L
) {
  filter <- tibble::tibble(
    wavelength_nm = 380:780,
    transmittance = rep_len(coefficient, 401),
    status = "supplied"
  )
  new_transmission_applied_snapshot(
    calculate_material_result(source, filter, mode),
    metadata = list(filter_name = "Test material", material_mode = mode),
    incident_name = "Source",
    draft_revision = sequence,
    apply_sequence = sequence
  )
}

test_that("reflection separates native exitance and the F=1 receiver scenario", {
  source <- transmission_source_fixture("d65", target_lux = 200)
  reflection <- material_test_snapshot(source)
  transmission <- material_test_snapshot(source, "transmission")
  expect_identical(reflection$output_quantity, "spectral_radiant_exitance")
  expect_equal(
    reflection$native_output$value_w_m2_nm,
    source$Bestrahlungsstaerke * .5
  )
  expect_equal(
    reflection$transmitted_spectrum,
    transmission$transmitted_spectrum
  )
  expect_match(reflection$receiver_assumption, "Lambertian")
  expect_true(all(startsWith(reflection$d65_properties$metric_id, "rho_")))
  effective <- subset(
    reflection$active_metrics,
    metric_id == "melanopic_der_effective"
  )
  der <- subset(reflection$active_metrics, metric_id == "melanopic_der")
  expect_equal(unname(effective$transmitted_value), .5, tolerance = 2e-5)
  expect_equal(unname(der$transmitted_value), 1, tolerance = 2e-5)
  expect_equal(material_photopic_lux(reflection$transmitted_spectrum), 100)
})

test_that("promotion preserves defaults and records explicit receiver rescaling", {
  snapshot <- material_test_snapshot(transmission_source_fixture(
    "d65",
    target_lux = 200
  ))
  frozen <- snapshot
  expect_equal(
    material_promotion(snapshot)$spectrum,
    snapshot$transmitted_spectrum
  )
  promoted <- material_promotion(snapshot, 400)
  expect_equal(material_photopic_lux(promoted$spectrum), 400)
  expect_equal(promoted$provenance$rescaling_factor, 4)
  expect_true(promoted$provenance$explicitly_rescaled)
  expect_identical(snapshot, frozen)
  expect_true(all(
    material_promotion(snapshot, 0)$spectrum$Bestrahlungsstaerke == 0
  ))
  for (target in list(-1, NA_real_, Inf, c(1, 2), "100"))
    expect_error(material_promotion(snapshot, target), "finite, nonnegative")
  zero <- material_test_snapshot(snapshot$incident_spectrum, coefficient = 0)
  expect_equal(material_promotion(zero)$provenance$rescaling_factor, 1)
  expect_error(material_promotion(zero, 1), "zero photopic")
  expect_equal(
    material_promotion(
      material_test_snapshot(snapshot$incident_spectrum, "transmission"),
      50
    )$provenance$target_illuminance_lx,
    50
  )
})

test_that("mixed cumulative branches separate material attenuation from overrides", {
  source <- transmission_source_fixture("d65", target_lux = 200)
  root <- new_transmission_active_spectrum(
    source,
    "Original",
    "Test",
    1L,
    "import",
    "node-1"
  )
  history <- new_transmission_history(root)
  first <- transmission_history_promote(
    history,
    material_test_snapshot(source, "transmission"),
    "Glass",
    target_lux = 300
  )
  second <- transmission_history_promote(
    first$history,
    material_test_snapshot(first$node$spectrum),
    "Wall"
  )
  cumulative <- calculate_material_cumulative(second$history)
  expect_identical(cumulative$path, c("node-1", "node-2", "node-3"))
  expect_equal(
    cumulative$spectra$cumulative_material_coefficient,
    rep(.25, 401)
  )
  expect_equal(
    unname(
      subset(
        cumulative$material_metrics,
        metric_id == "photopic_illuminance"
      )$transmitted_value
    ),
    50
  )
  expect_equal(
    unname(
      subset(
        cumulative$actual_metrics,
        metric_id == "photopic_illuminance"
      )$transmitted_value
    ),
    150
  )
  expect_equal(
    subset(
      cumulative$material_metrics,
      metric_id == "melanopic_der_effective"
    )$transmitted_value,
    .25,
    tolerance = 2e-5
  )
  expect_equal(
    subset(
      cumulative$actual_metrics,
      metric_id == "melanopic_der_effective"
    )$transmitted_value,
    .75,
    tolerance = 2e-5
  )
  restored <- transmission_history_restore(second$history, "node-1")$history
  sibling <- transmission_history_promote(
    restored,
    material_test_snapshot(source, coefficient = .8),
    "Alternative"
  )
  expect_equal(
    calculate_material_cumulative(
      sibling$history
    )$spectra$cumulative_material_coefficient,
    rep(.8, 401)
  )
  expect_equal(
    calculate_material_cumulative(sibling$history, "node-3"),
    cumulative
  )
  repeated <- transmission_history_promote(
    second$history,
    material_test_snapshot(second$node$spectrum),
    "Second wall"
  )
  expect_equal(
    calculate_material_cumulative(
      repeated$history
    )$spectra$cumulative_material_coefficient,
    rep(.125, 401)
  )
  expect_identical(second$node$provenance$origin, "Reflection")
  bad <- repeated$history
  bad$nodes[["node-1"]]$parent_id <- "node-4"
  expect_error(calculate_material_cumulative(bad), "cycle")
})

test_that("all TUB materials preserve original data and qualified identities", {
  expect_equal(nrow(tub_material_records), 55)
  expect_equal(table(tub_material_records$material_mode)[["reflection"]], 28)
  expect_equal(table(tub_material_records$material_mode)[["transmission"]], 27)
  expect_equal(nrow(tub_material_curves), 55 * 81)
  expect_false(anyDuplicated(tub_material_records$catalogue_id) > 0)
  for (id in tub_material_records$catalogue_id) {
    record <- transmission_catalogue_record(id)
    expect_equal(record$curve$wavelength_nm, seq(380L, 780L, 5L))
    expect_true(all(
      record$curve$transmittance >= 0 & record$curve$transmittance <= 1
    ))
    expect_equal(record$record$licence, "CC BY 4.0")
  }
  oak <- transmission_catalogue_record("tub:reflection:WF5")$record
  expect_equal(oak$source_column, 6L)
  expect_equal(oak$source_column_header, "HB4 / WF4")
  expect_match(oak$display_name, "Oak")
  expect_equal(
    nrow(filter_transmission_catalogue(
      material_catalogue_records_data("reflection"),
      "tub67600"
    )),
    28
  )
})

test_that("reflection exports carry quantity names and bilingual result labels", {
  snapshot <- material_test_snapshot(transmission_source_fixture(
    "equal_energy"
  ))
  expect_true(
    "reflectance" %in% names(transmission_completed_filter_export(snapshot))
  )
  data <- transmission_spectral_comparison_export(snapshot)
  expect_true(all(
    c(
      "reflectance",
      "reflected_spectral_exitance_w_m2_nm",
      "receiver_irradiance_f1_w_m2_nm"
    ) %in%
      names(data)
  ))
  expect_false(any(grepl("transmitt", names(data))))
  expect_true(
    "outcome_value" %in% names(transmission_applied_metrics_export(snapshot))
  )
  expect_false(
    "transmitted_value" %in%
      names(transmission_applied_metrics_export(snapshot))
  )
  for (language in c("English", "Deutsch")) {
    expect_match(
      material_labeler("reflection")(
        "table_transmitted",
        language_direct = language
      ),
      "F = 1",
      fixed = TRUE
    )
    expect_match(material_text("target_help", language), "F = 1", fixed = TRUE)
  }
})

test_that("coloured sequences integrate the product rather than average retained fractions", {
  source <- transmission_source_fixture("d65")
  first_coefficient <- seq(.1, .9, length.out = 401)
  second_coefficient <- rev(first_coefficient)
  root <- new_transmission_active_spectrum(
    source,
    "Root",
    "Test",
    1L,
    "import",
    "node-1"
  )
  first <- transmission_history_promote(
    new_transmission_history(root),
    material_test_snapshot(source, "transmission", first_coefficient),
    "First"
  )
  second <- transmission_history_promote(
    first$history,
    material_test_snapshot(
      first$node$spectrum,
      "reflection",
      second_coefficient
    ),
    "Second"
  )
  cumulative <- calculate_material_cumulative(second$history)
  weighting <- spectral_action_weighting("V(lambda)")
  expected <- sum(
    source$Bestrahlungsstaerke *
      first_coefficient *
      second_coefficient *
      weighting
  ) /
    sum(source$Bestrahlungsstaerke * weighting)
  observed <- subset(
    cumulative$material_metrics,
    metric_id == "photopic_illuminance"
  )$comparison_value
  expect_equal(unname(observed), expected)
  expect_error(
    transmission_history_promote(
      second$history,
      material_test_snapshot(source),
      "Stale"
    ),
    "does not match"
  )
})

test_that("reflection audit preserves unscaled quantities and scaled history", {
  source <- transmission_source_fixture("d65", target_lux = 200)
  root <- new_transmission_active_spectrum(
    source,
    "Root",
    "Test",
    1L,
    "import",
    "node-1"
  )
  snapshot <- material_test_snapshot(source)
  promoted <- transmission_history_promote(
    new_transmission_history(root),
    snapshot,
    "Rescaled wall",
    target_lux = 300
  )
  active <- new_transmission_active_spectrum(
    promoted$node$spectrum,
    "Rescaled wall",
    "Reflection",
    2L,
    "promotion",
    "node-2"
  )
  path <- tempfile(fileext = ".zip")
  write_transmission_audit_zip(path, snapshot, promoted$history, active)
  directory <- tempfile()
  utils::unzip(path, exdir = directory)
  expect_true(file.exists(file.path(
    directory,
    "spectra/reflected-exitance.csv"
  )))
  native <- utils::read.csv(file.path(
    directory,
    "spectra/reflected-exitance.csv"
  ))
  expect_equal(native$value_w_m2_nm, source$Bestrahlungsstaerke * .5)
  cumulative <- utils::read.csv(file.path(
    directory,
    "history/node-2-cumulative-metrics.csv"
  ))
  expect_identical(
    cumulative$status,
    "not_available_after_illuminance_rescaling"
  )
  expect_identical(cumulative$scaled_nodes, "node-2")
  expect_false("outcome_value" %in% names(cumulative))
  expect_equal(material_photopic_lux(promoted$node$spectrum), 300)
  expect_equal(material_photopic_lux(snapshot$transmitted_spectrum), 100)
})

test_that("cumulative availability follows branch ancestry and omits action factors", {
  source <- transmission_source_fixture("d65", target_lux = 200)
  root <- new_transmission_active_spectrum(
    source,
    "Root",
    "Test",
    1L,
    "import",
    "node-1"
  )
  history <- new_transmission_history(root)
  expect_false(material_cumulative_has_rescaling(calculate_material_cumulative(
    history
  )))
  first <- transmission_history_promote(
    history,
    material_test_snapshot(source),
    "Wall"
  )
  unscaled <- calculate_material_cumulative(first$history)
  expect_false(material_cumulative_has_rescaling(unscaled))
  records <- material_cumulative_export(unscaled)
  expect_setequal(
    unique(records$comparison),
    "material_only_f1"
  )
  expect_false(any(grepl("_action_factor$", records$metric_id)))
  expect_true("melanopic_der_effective" %in% records$metric_id)
  scaled <- transmission_history_promote(
    first$history,
    material_test_snapshot(first$node$spectrum),
    "Rescaled",
    target_lux = 300
  )
  expect_true(material_cumulative_has_rescaling(calculate_material_cumulative(
    scaled$history
  )))
  earlier <- calculate_material_cumulative(scaled$history, "node-2")
  expect_false(material_cumulative_has_rescaling(earlier))
  restored <- transmission_history_restore(scaled$history, "node-1")$history
  sibling <- transmission_history_promote(
    restored,
    material_test_snapshot(source),
    "Sibling"
  )
  expect_false(material_cumulative_has_rescaling(calculate_material_cumulative(
    sibling$history
  )))
  expect_true(material_cumulative_has_rescaling(calculate_material_cumulative(
    sibling$history,
    "node-3"
  )))
})

test_that("material integration reproduces public TUB integral reference values", {
  reference <- utils::read.csv(system.file(
    "extdata/tub/reference-photopic.csv",
    package = "Spectran"
  ))
  cie <- examplespectra$CIE
  source_a <- tibble::tibble(
    Wellenlaenge = 380:780,
    Bestrahlungsstaerke = stats::approx(
      cie$Wellenlaenge,
      cie$A,
      xout = 380:780
    )$y
  )
  sources <- list(A = source_a, D65 = d65_visible_spectrum())
  for (i in seq_len(nrow(reference))) {
    row <- reference[i, ]
    curve <- transmission_catalogue_record(row$material)$curve
    filter <- prepare_transmission_curve(
      data.frame(
        wavelength_nm = curve$wavelength_nm,
        value = curve$transmittance
      ),
      scale = "fraction"
    )$completed
    result <- calculate_active_transmission_metrics(
      sources[[row$illuminant]],
      filter
    )
    observed <- result$metrics$comparison_value[
      result$metrics$metric_id == "photopic_illuminance"
    ]
    expect_lte(
      abs(observed - row$expected),
      row$tolerance,
      label = paste(row$reference, row$material, row$illuminant)
    )
  }
})
