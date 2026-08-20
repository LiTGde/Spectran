transmission_export_test_snapshot <- function(raw_bytes = raw()) {
  source <- transmission_source_fixture("d65")
  filter <- tibble::tibble(
    wavelength_nm = 380:780,
    transmittance = rep(0.5, 401L),
    status = rep("supplied", 401L)
  )
  parsed <- tibble::tibble(
    source_row = 1:2,
    wavelength_nm = c(380, 780),
    value = c(0.5, 0.5)
  )
  new_transmission_applied_snapshot(
    result = calculate_transmission_result(source, filter),
    metadata = list(
      filter_name = "Neutral 50%",
      scale = "fraction",
      transmittance_type = "total",
      measurement_angle = "normal incidence",
      input_record = list(
        record_type = if (length(raw_bytes) > 0L) "upload" else "fixture",
        record_name = if (length(raw_bytes) > 0L) {
          "instrument export.csv"
        } else {
          "Neutral 50%"
        },
        media_type = "text/csv",
        raw_bytes = raw_bytes,
        parsed_values = parsed,
        citation = "Synthetic test filter",
        license = "MIT",
        source_url = ""
      ),
      normalization_decisions = list(
        sorted_input = FALSE,
        lower_tail = "not required",
        upper_tail = "not required",
        large_gaps_acknowledged = FALSE
      ),
      normalization_warnings = character()
    ),
    incident_name = "CIE D65 at 250 lx",
    draft_revision = 1L,
    apply_sequence = 1L
  )
}

test_that("quick exports retain the scientific long-form contracts", {
  snapshot <- transmission_export_test_snapshot()

  completed <- transmission_completed_filter_export(snapshot)
  comparison <- transmission_spectral_comparison_export(snapshot)
  d65 <- transmission_d65_export(snapshot)
  active <- transmission_applied_metrics_export(snapshot)

  expect_named(
    completed,
    c("wavelength_nm", "transmittance", "status")
  )
  expect_equal(nrow(completed), 401L)
  expect_equal(nrow(comparison), 401L)
  expect_equal(
    comparison$transmitted_spectral_irradiance_w_m2_nm,
    comparison$incident_spectral_irradiance_w_m2_nm * 0.5
  )
  expect_equal(nrow(d65), 6L)
  expect_true(all(d65$scope == "d65_filter"))
  expect_equal(nrow(active), 22L)
  expect_true(all(active$scope == "active_source"))
})

test_that("configurable export bundle uses stable content identifiers", {
  expect_identical(
    transmission_export_content_ids(),
    c(
      "result_plot",
      "filter_plot",
      "plot_table",
      "d65_png",
      "light_png",
      "balance_png",
      "completed_csv",
      "comparison_csv",
      "d65_csv",
      "light_csv",
      "balance_csv",
      "history_csv",
      "audit_zip"
    )
  )

  snapshot <- transmission_export_test_snapshot()
  active <- new_transmission_active_spectrum(
    snapshot$incident_spectrum,
    snapshot$incident_name,
    "Test source",
    1L,
    "import",
    "node-1"
  )
  history <- new_transmission_history(active)
  archive <- tempfile(fileext = ".zip")
  withr::defer(unlink(archive))

  write_transmission_export_bundle(
    file = archive,
    snapshot = snapshot,
    history = history,
    active_state = active,
    response_curves = "photopic",
    contents = c("result_plot", "completed_csv"),
    plot_width = 8,
    plot_height = 4,
    font_size = 11,
    max_irradiance = 8
  )

  expect_true(file.exists(archive))
  expect_setequal(
    utils::unzip(archive, list = TRUE)$Name,
    c(
      "neutral-50-result-plot.png",
      "neutral-50-completed-filter.csv"
    )
  )
  expect_error(
    write_transmission_export_bundle(
      file = tempfile(fileext = ".zip"),
      snapshot = snapshot,
      history = history,
      active_state = active,
      contents = character()
    ),
    "Select at least one file"
  )
})

test_that("audit ZIP preserves uploads, metrics, history, and provenance", {
  original_text <- paste(
    "Instrument export;do not alter",
    "Wavelength_nm;Transmission_percent",
    "380;50,0",
    "780;50,0",
    sep = "\r\n"
  )
  original_bytes <- charToRaw(original_text)
  snapshot <- transmission_export_test_snapshot(original_bytes)
  root <- new_transmission_active_spectrum(
    spectrum = snapshot$incident_spectrum,
    name = snapshot$incident_name,
    origin = "Development fixture",
    revision = 1L,
    change_type = "import",
    node_id = "node-1"
  )
  promoted <- transmission_history_promote(
    new_transmission_history(root),
    snapshot,
    "CIE D65 at 250 lx × Neutral 50%"
  )
  event <- new_transmission_activation_event(
    action_sequence = 1L,
    change_type = "promotion",
    node_id = promoted$node$node_id,
    parent_id = promoted$node$parent_id,
    spectrum = promoted$node$spectrum,
    name = promoted$node$name,
    provenance = promoted$node$provenance
  )
  active <- transmission_active_spectrum_from_event(event, revision = 2L)

  audit_directory <- withr::local_tempdir()
  archive <- file.path(audit_directory, "audit.zip")
  write_transmission_audit_zip(
    archive,
    snapshot,
    promoted$history,
    active,
    generated_at = as.POSIXct("2026-08-13 10:00:00", tz = "UTC")
  )

  expect_true(file.exists(archive))
  listing <- utils::unzip(archive, list = TRUE)$Name
  expected <- c(
    "README.txt",
    "input/instrument-export.csv",
    "input/input-record-metadata.csv",
    "input/parsed-curve.csv",
    "filter/completed-filter.csv",
    "filter/completion-status.csv",
    "spectra/incident-spectrum.csv",
    "spectra/transmitted-spectrum.csv",
    "spectra/spectral-comparison.csv",
    "metrics/d65-properties.csv",
    "metrics/active-metrics.csv",
    "metrics/all-metrics.csv",
    "history/history-tree.csv",
    "history/node-spectra.csv",
    "metadata/applied-metadata.csv",
    "metadata/decisions-warnings.csv",
    "citations-licenses.csv",
    "manifest.csv"
  )
  expect_setequal(listing, expected)

  extracted <- file.path(audit_directory, "extracted")
  dir.create(extracted)
  utils::unzip(archive, exdir = extracted)
  preserved_path <- file.path(extracted, "input", "instrument-export.csv")
  preserved <- readBin(
    preserved_path,
    what = "raw",
    n = file.info(preserved_path)$size
  )
  expect_identical(preserved, original_bytes)

  history <- utils::read.csv(
    file.path(extracted, "history", "history-tree.csv"),
    stringsAsFactors = FALSE
  )
  expect_equal(nrow(history), 2L)
  expect_identical(history$parent_id[[2L]], "node-1")
  expect_true(history$active[[2L]])

  node_spectra <- utils::read.csv(
    file.path(extracted, "history", "node-spectra.csv")
  )
  expect_equal(nrow(node_spectra), 802L)
  manifest <- utils::read.csv(file.path(extracted, "manifest.csv"))
  expect_false(any(manifest$file == "manifest.csv"))
  expect_true(all(nchar(manifest$md5) == 32L))
  expect_true(all(manifest$bytes > 0L))

  readme <- readLines(file.path(extracted, "README.txt"), encoding = "UTF-8")
  expect_true("English" %in% readme)
  expect_true("Deutsch" %in% readme)
  expect_true(any(grepl("Active history node: node-2", readme, fixed = TRUE)))
})

test_that("fixture audit records are exported without pretending to be uploads", {
  snapshot <- transmission_export_test_snapshot()
  root <- new_transmission_active_spectrum(
    snapshot$incident_spectrum,
    snapshot$incident_name,
    "Development fixture",
    1L,
    "import",
    "node-1"
  )
  history <- new_transmission_history(root)
  audit_directory <- withr::local_tempdir()
  archive <- file.path(audit_directory, "fixture-audit.zip")

  write_transmission_audit_zip(
    archive,
    snapshot,
    history,
    root,
    generated_at = as.POSIXct("2026-08-13", tz = "UTC")
  )
  listing <- utils::unzip(archive, list = TRUE)$Name

  expect_true("input/development-fixture-record.csv" %in% listing)
  expect_false(any(grepl("original-upload", listing, fixed = TRUE)))
})

test_that("filename components are portable and deterministic", {
  expect_identical(
    transmission_filename_component("CIE D65 × Neutral 50%"),
    "cie-d65-x-neutral-50"
  )
  expect_identical(
    transmission_filename_component("  Glazing / 8°  "),
    "glazing-8deg"
  )
})
