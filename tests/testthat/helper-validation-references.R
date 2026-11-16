validation_references <- function() {
  readRDS(testthat::test_path("fixtures", "validation-references.rds"))
}

# Test the same negative-input policy and calculation helpers as the app.
reference_spectrum_metrics <- function(spectrum, colour = FALSE) {
  spectrum <- prepare_spectran_source(spectrum)$spectrum
  result <- calculate_visible_spectrum_metrics(spectrum)
  responses <- result$responses
  alpha <- responses[responses$is_alpha_opic, ]
  values <- c(
    illuminance = unname(responses$equivalent_illuminance_lx[
      responses$response_id == "photopic"]),
    irradiance = result$total_irradiance_mw_m2,
    stats::setNames(alpha$response_irradiance_mw_m2,
      paste0(alpha$response_id, "_irradiance")),
    stats::setNames(alpha$equivalent_illuminance_lx,
      paste0(alpha$response_id, "_edi"))
  )
  if (colour) {
    object <- cS_object(spectrum)
    values <- c(values,
      cct = as.numeric(colorSpec::computeCCT(object, strict = TRUE)),
      cri = as.numeric(CRI(object, tol = TRUE)))
  }
  values
}

# Explicit elementwise bounds avoid expect_equal()'s vector-wide scaling.
# Non-finite values and missing matches must fail, not disappear via na.rm.
expect_reference_values <- function(actual, expected, label,
  absolute = 0, relative = 0) {
  expect_length(actual, length(expected))
  limit <- absolute + relative * abs(expected)
  passed <- is.finite(actual) & is.finite(expected) &
    abs(actual - expected) <= limit
  failures <- which(!passed)
  detail <- sprintf("%s: actual %.10g, reference %.10g, limit %.4g",
    label[failures], actual[failures], expected[failures],
    rep_len(limit, length(expected))[failures])
  expect_true(all(passed), info = paste(detail, collapse = "\n"))
}
