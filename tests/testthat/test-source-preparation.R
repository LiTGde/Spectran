test_that("source preparation preserves originals and does not hide nonfinite data", {
  source <- tibble::tibble(Wellenlaenge = c(380, 385, 390, 395, 400),
    Bestrahlungsstaerke = c(-1e-6, 0, .1, NA, Inf))
  prepared <- prepare_spectran_source(source)
  expect_equal(prepared$spectrum$Bestrahlungsstaerke, c(0, 0, .1, NA, Inf))
  expect_equal(source$Bestrahlungsstaerke[[1]], -1e-6)
  expect_equal(prepared$provenance$source_preprocessing$count, 1)
  expect_equal(prepare_spectran_source(prepared$spectrum, prepared$provenance), prepared)
  expect_null(prepare_spectran_source(prepared$spectrum)$provenance$source_preprocessing)
  expect_error(as_visible_spectrum(source))
})

test_that("negative measurements can be saved, chained and audited through three materials", {
  source <- transmission_source_fixture("d65", target_lux = 200)
  source$Bestrahlungsstaerke[c(1, 101)] <- c(-1e-6, -2e-6)
  root <- new_transmission_active_spectrum(source, "Noisy measurement", "Test", 1L, "import", "node-1")
  history <- new_transmission_history(root)
  active <- root
  for (step in 1:3) {
    filter <- tibble::tibble(wavelength_nm = 380:780, transmittance = .5, status = "supplied")
    snapshot <- new_transmission_applied_snapshot(
      calculate_material_result(active$spectrum, filter, c("transmission", "reflection", "transmission")[[step]]),
      metadata = list(filter_name = "Neutral 50%", source_preprocessing = active$provenance$source_preprocessing,
        measurement_instrument = "Fixture instrument; 2024", relative_measurement_error = "±2% (fixture)"),
      incident_name = active$name, draft_revision = step, apply_sequence = step)
    saved <- transmission_history_promote(history, snapshot, paste("Step", step))
    history <- saved$history
    active <- new_transmission_active_spectrum(saved$node$spectrum, saved$node$name,
      "Test", step + 1L, "promotion", saved$node$node_id, saved$node$provenance)
    expect_true(all(active$spectrum$Bestrahlungsstaerke >= 0))
    expect_equal(active$provenance$source_preprocessing, root$provenance$source_preprocessing)
  }
  expect_equal(active$spectrum$Bestrahlungsstaerke, pmax(source$Bestrahlungsstaerke, 0) / 8)
  expect_length(history$nodes, 4)
  cumulative <- calculate_material_cumulative(history)
  expect_equal(cumulative$spectra$cumulative_material_coefficient, rep(.125, 401))
  directory <- withr::local_tempdir()
  archive <- file.path(directory, "audit.zip")
  write_transmission_audit_zip(archive, snapshot, history, active)
  utils::unzip(archive, exdir = directory)
  original <- utils::read.csv(file.path(directory, "spectra/source-negative-values.csv"))
  expect_equal(original$wavelength_nm, c(380, 480))
  expect_equal(original$measured_irradiance_w_m2_nm, c(-1e-6, -2e-6))
  expect_true(all(original$used_irradiance_w_m2_nm == 0))
  decisions <- transmission_decisions_warnings_export(snapshot)
  expect_true(any(grepl("negative source measurements", decisions$value)))
  expect_match(paste(readLines(file.path(directory, "metadata/applied-metadata.csv")), collapse = " "), "Fixture instrument")
})

test_that("file imports retain the correction before units and interpolation obscure it", {
  old_language <- the$language
  withr::defer(the$language <- old_language)
  the$language <- "English"
  state <- shiny::reactiveValues()
  initialize_spectran_spectrum_state(state)
  state$Origin <- "File"
  raw <- shiny::reactiveVal(data.frame(nm = c(380, 385, 390, 780), mw = c(-1, 2, 3, 1)))
  settings <- shiny::reactive(list(x_y = 1L, x_y2 = 2L, multiplikator = .001))
  processed <- shiny::reactive(data.frame(nm = raw()$nm, mw = pmax(raw()$mw, 0) * .001))
  shiny::testServer(import_data_verifierServer, args = list(Data_ok = shiny::reactive(TRUE),
    dat = processed, csv_settings = settings, Spectrum = state, Name = shiny::reactive("Noise"), raw_data = raw), {
    session$flushReact()
    session$setInputs(import = 1L)
    correction <- attr(state$Spectrum_raw, "source_preprocessing")
    expect_equal(correction$samples$measured_irradiance_w_m2_nm, -.001)
    expect_equal(correction$stage, "before_interpolation")
    raw(data.frame(nm = c(380, 385, 390, 780), mw = c(1, 2, 3, 1)))
    session$setInputs(import = 2L)
    expect_null(attr(state$Spectrum_raw, "source_preprocessing"))
  })
  captured <- NULL
  state$import_guard <- function(request) captured <<- request
  shiny::testServer(import_verifierServer, args = list(Spectrum = state), {
    session$flushReact()
    state$Spectrum_raw <- data.frame(nm = c(380, 385, 390, 780), irradiance = c(-.001, .002, .003, .001))
    expect_warning({
      signal_spectran_import_attempt(state)
      session$flushReact()
    }, "collapsing to unique")
    expect_equal(captured$spectrum$Bestrahlungsstaerke[1:3], c(0, .0004, .0008))
    expect_equal(captured$provenance$source_preprocessing$count, 1)
    expect_equal(captured$notification$type, "warning")
  })
})

test_that("noisy example scaling reaches the requested level after correction", {
  raw <- data.frame(nm = seq(380, 780, by = 5), irradiance = .001)
  raw$irradiance[20] <- -1e-5
  scaled <- material_scale_source_data(raw, 100, "melanopic")
  spectrum <- tibble::tibble(Wellenlaenge = 380:780,
    Bestrahlungsstaerke = stats::approx(scaled[[1]], scaled[[2]], xout = 380:780)$y)
  expect_equal(material_light_level(spectrum, "melanopic"), 100)
  expect_true(all(spectrum$Bestrahlungsstaerke >= 0))
  expect_equal(attr(scaled, "source_preprocessing")$samples$measured_irradiance_w_m2_nm, -1e-5)
})
