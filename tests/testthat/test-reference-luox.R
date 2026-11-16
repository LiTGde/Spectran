test_that("luox references retain both explicit CRI applicability exceptions", {
  fixture <- validation_references()
  reference <- fixture$luox
  expect_equal(nrow(reference), 130L)
  expect_setequal(unique(reference$spectrum), names(fixture$spectra))
  expect_false(anyDuplicated(reference[c("spectrum", "metric")]) > 0L)
  expect_true(all(is.finite(reference$expected)))
  expect_false(anyNA(reference$applicable))
  exceptions <- reference[!reference$applicable, ]
  expect_equal(nrow(exceptions), 2L)
  expect_identical(exceptions$metric, rep("cri", 2L))
  expect_setequal(exceptions$spectrum, c("EE_Spektrum", "LED_4000K_2"))
  expect_setequal(unique(reference$metric), c("illuminance", "cct", "cri",
    paste0(c("s_cone_opic", "m_cone_opic", "l_cone_opic", "rhodopic",
      "melanopic"), rep(c("_irradiance", "_edi"), each = 5))))
})

test_that("visible and colour metrics reproduce the archived luox outputs", {
  fixture <- validation_references()
  for (id in names(fixture$spectra)) {
    reference <- fixture$luox[fixture$luox$spectrum == id, ]
    actual <- reference_spectrum_metrics(fixture$spectra[[id]], colour = TRUE)
    # Strict CIE applicability is part of the app's documented behaviour.
    if (any(!reference$applicable)) {
      expect_true(all(is.na(actual[reference$metric[!reference$applicable]])),
        info = paste(id, "must withhold CRI outside its applicability limit"))
    }
    reference <- reference[reference$applicable, ]
    # Historical luox inputs include small negative measurements; the app
    # clips these. Colour uses absolute units, not relative percent errors.
    relative <- ifelse(reference$metric %in% c("cct", "cri"), 0, 5e-4)
    absolute <- ifelse(reference$metric == "cct", 0.1,
      ifelse(reference$metric == "cri", 0.5, 1e-10))
    expect_reference_values(actual[reference$metric], reference$expected,
      paste("luox", id, reference$metric, reference$unit),
      absolute = absolute, relative = relative)
  }
})
