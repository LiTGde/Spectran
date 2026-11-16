test_that("DIN references preserve coverage and the exact FL11 exception scope", {
  fixture <- validation_references()$din
  reference <- fixture$expected
  expect_equal(nrow(reference), 456L)
  expect_length(unique(reference$din_id), 39L)
  expect_length(fixture$unmatched_material_ids, 32L)
  expect_length(intersect(reference$din_id, fixture$unmatched_material_ids), 0L)
  expect_false(anyDuplicated(reference[c("din_id", "illuminant", "metric")]) > 0L)
  expect_true(all(is.finite(reference$expected)))
  expect_true(all(reference$expected >= 0 & reference$expected <= 1))
  expect_equal(sum(reference$mode == "transmission"), 24L)
  expect_equal(sum(reference$mode == "reflection"), 432L)
  expect_setequal(unique(reference$illuminant),
    c("EE", "A", "D65", "FL11", "LED-B1", "LED-B2", "LED-B3", "LED-B5"))
  expect_setequal(unique(reference$metric),
    c("photopic_illuminance", "melanopic_irradiance"))
  expect_identical(reference$known_issue,
    reference$din_id %in% c(401:413, 416:427) &
      reference$mode == "reflection" & reference$illuminant == "FL11" &
      reference$metric == "photopic_illuminance")
  expect_equal(sum(reference$known_issue), 25L)
  expect_equal(nrow(fixture$fl11_regression), 25L)
  expect_setequal(fixture$fl11_regression$din_id, c(401:413, 416:427))
  expect_false(anyDuplicated(fixture$fl11_regression$din_id) > 0L)
})

test_that("material metrics reproduce DIN coefficients with scoped FL11 checks", {
  fixture <- validation_references()$din
  reference <- fixture$expected
  material_ids <- unique(reference$catalogue_id)
  curves <- stats::setNames(lapply(material_ids, function(id) {
    curve <- transmission_catalogue_record(id)$curve
    prepare_transmission_curve(data.frame(wavelength_nm = curve$wavelength_nm,
      value = curve$transmittance), scale = "fraction")$completed
  }), material_ids)
  illuminants <- unique(reference$illuminant)
  sources <- stats::setNames(lapply(illuminants, function(id) {
    name <- if (id == "EE") "E" else gsub("-", "_", id, fixed = TRUE)
    data.frame(Wellenlaenge = 380:780,
      Bestrahlungsstaerke = stats::approx(examplespectra$CIE$Wellenlaenge,
        examplespectra$CIE[[name]], xout = 380:780)$y)
  }), illuminants)
  pairs <- unique(reference[c("catalogue_id", "illuminant", "mode")])
  for (i in seq_len(nrow(pairs))) {
    pair <- pairs[i, ]
    expected <- reference[reference$catalogue_id == pair$catalogue_id &
      reference$illuminant == pair$illuminant, ]
    result <- calculate_material_result(sources[[pair$illuminant]],
      curves[[pair$catalogue_id]], pair$mode)$active_metrics
    actual <- result$comparison_value[match(expected$metric, result$metric_id)]
    label <- paste("DIN", expected$din_id, pair$catalogue_id,
      pair$illuminant, expected$metric)
    normal <- !expected$known_issue
    expect_reference_values(actual[normal], expected$expected[normal],
      label[normal], absolute = 0.001)

    if (any(expected$known_issue)) {
      issue <- expected$known_issue
      # Require the known signed discrepancy to remain visible. Returning the
      # erroneous DIN number would fail this test instead of looking correct.
      expect_lt(actual[issue] - expected$expected[issue], -0.001,
        label = label[issue])
      anchor <- fixture$fl11_regression
      anchor <- anchor$regression_value[match(expected$din_id[issue], anchor$din_id)]
      expect_reference_values(actual[issue], anchor, label[issue], absolute = 1e-6)
    }
  }
})
