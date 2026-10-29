test_that("shared CSV reader preserves the existing source-import parse", {
  path <- tempfile(fileext = ".csv")
  writeLines(
    c("wavelength,irradiance", "380,0.10", "381,0.20"),
    con = path
  )
  settings <- list(
    row_nr = 0,
    separator = ",",
    decimal = ".",
    x_y = 1,
    x_y2 = 2,
    header = TRUE
  )

  expected <- utils::read.csv(
    path,
    sep = settings$separator,
    dec = settings$decimal,
    skip = settings$row_nr,
    header = settings$header
  )
  actual <- read_spectral_csv(path, settings)

  expect_identical(actual, expected)
  preview <- spectral_csv_preview_data(actual, settings)
  expect_s3_class(preview, "tbl_df")
  expect_named(preview, c("source_row", "wavelength_nm", "value"))
  expect_identical(preview$wavelength_nm, expected[[1]])
  expect_identical(preview$value, expected[[2]])
})

test_that("shared CSV reader supports decimal commas and skipped rows", {
  path <- tempfile(fileext = ".csv")
  writeLines(
    c("instrument metadata", "nm;T", "380;0,25", "381;0,50"),
    con = path
  )
  settings <- list(
    skip = 1,
    delimiter = ";",
    decimal_mark = ",",
    wavelength_column = 1,
    value_column = 2,
    header = TRUE
  )

  parsed <- read_spectral_csv(path, settings)
  preview <- spectral_csv_preview_data(parsed, settings)

  expect_equal(preview$wavelength_nm, c(380, 381))
  expect_equal(preview$value, c(0.25, 0.5))
})

test_that("shared CSV settings reject ambiguous or missing columns", {
  same_columns <- list(
    skip = 0,
    delimiter = ",",
    decimal_mark = ".",
    wavelength_column = 1,
    value_column = 1,
    header = TRUE
  )
  expect_error(
    coerce_spectral_csv_settings(same_columns),
    "must be different"
  )

  valid_settings <- same_columns
  valid_settings$value_column <- 3
  expect_error(
    spectral_csv_preview_data(data.frame(x = 1, y = 2), valid_settings),
    "parsed into 2 columns"
  )
  expect_error(
    spectral_csv_preview_data(data.frame(x = 1, y = 2), valid_settings),
    "separator, decimal mark, header setting"
  )
})

test_that("shared settings UI namespaces its controls", {
  rendered <- as.character(spectral_csv_settingsUI("fixture"))

  expect_match(rendered, "fixture-skip", fixed = TRUE)
  expect_match(rendered, "fixture-wavelength_column", fixed = TRUE)
  expect_match(rendered, "fixture-value_column", fixed = TRUE)
})

test_that("transmission CSV labels can expose only documented decimals", {
  labels <- spectral_csv_settings_labels("Transmission column")
  labels$decimal_choices <- c(
    "Point (example: 0.5)" = ".",
    "Comma (example: 0,5)" = ","
  )
  rendered <- as.character(spectral_csv_settingsUI("transmission", labels))

  expect_match(rendered, "Point (example: 0.5)", fixed = TRUE)
  expect_match(rendered, "Comma (example: 0,5)", fixed = TRUE)
  expect_false(grepl('name="transmission-decimal_mark" value=";"', rendered))
})
