test_that("CIE Toolbox references cover all ten spectra and twelve metrics", {
  fixture <- validation_references()
  reference <- fixture$cie
  expect_identical(fixture$schema_version, 1L)
  expect_length(fixture$spectra, 10L)
  expect_equal(nrow(reference), 120L)
  expect_false(anyDuplicated(reference[c("spectrum", "metric")]) > 0L)
  expect_true(all(is.finite(reference$expected)))
  expect_setequal(unique(reference$spectrum), names(fixture$spectra))
  expect_setequal(unique(reference$metric), c("illuminance", "irradiance",
    paste0(c("s_cone_opic", "m_cone_opic", "l_cone_opic", "rhodopic",
      "melanopic"), rep(c("_irradiance", "_edi"), each = 5))))
})

test_that("visible metrics reproduce the archived CIE Toolbox outputs", {
  fixture <- validation_references()
  for (id in names(fixture$spectra)) {
    reference <- fixture$cie[fixture$cie$spectrum == id, ]
    actual <- reference_spectrum_metrics(fixture$spectra[[id]])
    # EDI includes the rounded, bundled daylight efficacy constants.
    relative <- ifelse(endsWith(reference$metric, "_edi"), 2e-5, 1e-6)
    expect_reference_values(actual[reference$metric], reference$expected,
      paste("CIE", id, reference$metric, reference$unit),
      absolute = 1e-10, relative = relative)
  }
})
