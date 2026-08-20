test_that("complete neutral input produces the 401-row contract", {
  result <- prepare_transmission_curve(
    transmission_fixture("neutral"),
    scale = "fraction"
  )

  expect_s3_class(result, "transmission_preparation")
  expect_true(result$diagnostics$ready)
  expect_equal(nrow(result$completed), 401)
  expect_equal(result$completed$wavelength_nm, 380:780)
  expect_equal(result$completed$transmittance, rep(0.5, 401))
  expect_setequal(
    unique(result$completed$status),
    c("supplied", "interpolated")
  )
})

test_that("percent scale is explicit and out-of-order rows are disclosed", {
  input <- tibble::tibble(
    source_row = 1:4,
    wavelength_nm = c(800, 780, 380, 360),
    value = rep(50, 4)
  )
  result <- prepare_transmission_curve(
    input,
    scale = "percent",
    acknowledge_large_gaps = TRUE
  )

  expect_true(result$diagnostics$ready)
  expect_true(result$diagnostics$was_sorted)
  expect_equal(result$diagnostics$outside_count, 2)
  expect_equal(result$normalized$transmittance, rep(0.5, 4))
  expect_equal(result$completed$transmittance, rep(0.5, 401))
  expect_match(
    paste(result$diagnostics$warnings, collapse = " "),
    "sorted"
  )
})

test_that("lower and upper tails require independent choices", {
  unresolved <- prepare_transmission_curve(
    transmission_fixture("partial"),
    scale = "fraction"
  )

  expect_false(unresolved$diagnostics$ready)
  expect_true(unresolved$diagnostics$lower_missing)
  expect_true(unresolved$diagnostics$upper_missing)
  expect_equal(
    unresolved$completed$status[[1]],
    "missing_lower_tail"
  )
  expect_equal(
    unresolved$completed$status[[401]],
    "missing_upper_tail"
  )

  resolved <- prepare_transmission_curve(
    transmission_fixture("partial"),
    scale = "fraction",
    lower_tail = "zero",
    upper_tail = "one"
  )

  expect_true(resolved$diagnostics$ready)
  expect_equal(resolved$completed$transmittance[[1]], 0)
  expect_equal(resolved$completed$transmittance[[40]], 0)
  expect_equal(resolved$completed$transmittance[[41]], 0.15)
  expect_equal(resolved$completed$transmittance[[401]], 1)
  expect_equal(resolved$completed$status[[1]], "assumed_lower_tail")
  expect_equal(resolved$completed$status[[401]], "assumed_upper_tail")
})

test_that("tail completion can carry the nearest measured value", {
  resolved <- prepare_transmission_curve(
    transmission_fixture("partial"),
    scale = "fraction",
    lower_tail = "carry",
    upper_tail = "carry"
  )

  expect_true(resolved$diagnostics$ready)
  expect_equal(
    resolved$completed$transmittance[[1L]],
    resolved$completed$transmittance[[41L]]
  )
  expect_equal(
    resolved$completed$transmittance[[401L]],
    resolved$completed$transmittance[[381L]]
  )
  expect_identical(resolved$completed$status[[1L]], "carried_lower_tail")
  expect_identical(resolved$completed$status[[401L]], "carried_upper_tail")
  expect_identical(
    transmission_status_label("carried_lower_tail"),
    "First supplied value carried backward to 380 nm"
  )
  expect_identical(
    transmission_status_label("carried_upper_tail"),
    "Last supplied value carried forward to 780 nm"
  )
})

test_that("large internal gaps require explicit acknowledgement", {
  unresolved <- prepare_transmission_curve(
    transmission_fixture("large_gap"),
    scale = "fraction"
  )
  expect_false(unresolved$diagnostics$ready)
  expect_equal(nrow(unresolved$diagnostics$large_gaps), 1)
  expect_equal(
    unresolved$diagnostics$large_gaps$width_nm,
    25
  )
  expect_true(any(
    unresolved$completed$status == "large_gap_unacknowledged"
  ))

  resolved <- prepare_transmission_curve(
    transmission_fixture("large_gap"),
    scale = "fraction",
    acknowledge_large_gaps = TRUE
  )
  expect_true(resolved$diagnostics$ready)
  expect_true(any(
    resolved$completed$status == "acknowledged_large_gap"
  ))
})

test_that("invalid transmission records are rejected without clipping", {
  duplicate_and_range <- prepare_transmission_curve(
    transmission_fixture("invalid"),
    scale = "fraction"
  )
  messages <- paste(duplicate_and_range$diagnostics$errors, collapse = " ")
  expect_false(duplicate_and_range$diagnostics$ready)
  expect_match(messages, "Duplicate wavelengths")
  expect_match(messages, "between 0 and 1")
  expect_match(messages, "source row 3")
  expect_match(messages, "scaled fraction 1.2")
  expect_null(duplicate_and_range$completed)

  non_finite <- tibble::tibble(
    wavelength_nm = c(380, 500, 780),
    value = c(0.5, NA_real_, Inf)
  )
  result <- prepare_transmission_curve(non_finite, scale = "fraction")
  expect_false(result$diagnostics$ready)
  expect_match(
    paste(result$diagnostics$errors, collapse = " "),
    "source row 2 \\(NA\\).*source row 3 \\(Inf\\)"
  )
})

test_that("outside-domain intervals do not inflate internal-gap consent", {
  input <- tibble::tibble(
    source_row = 1:4,
    wavelength_nm = c(360, 380, 780, 800),
    value = rep(50, 4)
  )
  unresolved <- prepare_transmission_curve(input, scale = "percent")

  expect_equal(nrow(unresolved$diagnostics$large_gaps), 1L)
  expect_equal(
    unresolved$diagnostics$large_gaps[, c("from_nm", "to_nm")],
    tibble::tibble(from_nm = 380, to_nm = 780)
  )
  expect_equal(nrow(unresolved$diagnostics$external_intervals), 2L)
  expect_equal(
    unresolved$diagnostics$external_intervals[, c("from_nm", "to_nm")],
    tibble::tibble(
      from_nm = c(360, 780),
      to_nm = c(380, 800)
    )
  )
  expect_match(
    paste(unresolved$diagnostics$requirements, collapse = " "),
    "1 internal gap"
  )
  expect_match(
    paste(unresolved$diagnostics$warnings, collapse = " "),
    "require no interpolation consent"
  )
})

test_that("validation messages identify source rows and likely corrections", {
  input <- tibble::tibble(
    source_row = 11:13,
    wavelength_nm = c("380", "not-a-wavelength", "780"),
    value = c("0.5", "not-a-value", "0.5")
  )
  result <- prepare_transmission_curve(input, scale = "fraction")
  messages <- paste(result$diagnostics$errors, collapse = " ")

  expect_match(messages, 'source row 12 \\(\"not-a-wavelength\"\\)')
  expect_match(messages, 'source row 12 \\(\"not-a-value\"\\)')
  expect_match(messages, "decimal mark, header setting, rows to skip")
})

test_that("display format preserves consequential scaled precision", {
  expect_identical(format_transmission_number(0.007), "0.007")
  expect_identical(format_transmission_number(1), "1")
  expect_identical(
    transmission_status_label("acknowledged_large_gap"),
    "Large-gap interpolation (acknowledged)"
  )
  expect_identical(format_transmission_range(380:419), "380\u2013419 nm")
})

test_that("outside-range samples are retained and can bracket boundaries", {
  input <- tibble::tibble(
    source_row = 1:3,
    wavelength_nm = c(360, 580, 800),
    value = c(0.2, 0.6, 1)
  )
  result <- prepare_transmission_curve(
    input,
    scale = "fraction",
    acknowledge_large_gaps = TRUE
  )

  expect_true(result$diagnostics$ready)
  expect_equal(nrow(result$normalized), 3)
  expect_equal(result$diagnostics$outside_count, 2)
  expect_equal(nrow(result$completed), 401)
  expect_false(any(grepl("tail", result$completed$status)))
})

test_that("neutral-filter template is valid and calculation-ready", {
  template <- transmission_template_data()
  input <- tibble::tibble(
    source_row = seq_len(nrow(template)),
    wavelength_nm = template$wavelength_nm,
    value = template$transmittance_fraction
  )
  result <- prepare_transmission_curve(input, scale = "fraction")

  expect_equal(nrow(template), 401)
  expect_true(result$diagnostics$ready)
  expect_equal(result$completed$transmittance, rep(1, 401))
  expect_equal(unique(result$completed$status), "supplied")
})
