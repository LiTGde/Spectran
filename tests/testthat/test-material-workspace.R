test_that("the material browser preserves catalogue identities and localized discovery", {
  records <- material_browser_records("transmission")
  expect_setequal(records$catalogue_id, material_catalogue_records_data("transmission")$catalogue_id)
  ids <- c(records$catalogue_id, material_browser_records("reflection")$catalogue_id)
  expect_false(anyDuplicated(make.names(ids)) > 0)
  found <- material_browser_records("reflection", "Ahorn")
  expect_gt(nrow(found), 0)
  expect_true(all(found$material_mode == "reflection"))
  expect_equal(nrow(material_browser_records("transmission", "no-such-material-123")), 0)
})

test_that("workspace selection clears spectral-end decisions when the material changes", {
  records <- material_browser_records("transmission")
  incomplete <- records[records$wavelength_min_nm > 380 | records$wavelength_max_nm < 780, ]
  expect_gte(nrow(incomplete), 2L)
  shiny::testServer(transmissionServer, args = list(workspace = TRUE), {
    session$setInputs(input_source = "catalogue", material_mode = "transmission")
    session$flushReact()
    returned <- session$getReturned()
    expect_true(returned$ready(), info = paste(returned$diagnostics()$requirements, collapse = "; "))
    choose <- function(id, value = 1L) {
      do.call(session$setInputs, stats::setNames(list(value), paste0("pick_", make.names(id))))
      session$flushReact()
    }
    choose(incomplete$catalogue_id[[1]])
    expect_false(returned$ready())
    session$setInputs(lower_tail = "zero", upper_tail = "carry",
      transmittance_type = incomplete$transmittance_type[[1]], scattering = "no")
    session$setInputs(type_ack = TRUE)
    session$flushReact()
    expect_true(returned$ready(), info = paste(returned$diagnostics()$requirements, collapse = "; "))
    first <- returned$metadata()$filter_name
    choose(incomplete$catalogue_id[[2]])
    expect_false(returned$ready())
    expect_false(identical(first, returned$metadata()$filter_name))
    expect_null(returned$applied_snapshot())
  })
})

test_that("workspace renders a single instance of each input and output", {
  markup <- as.character(transmissionUI("work", layout = "workspace", source_ui = material_source_ui("light")))
  expect_match(markup, "material-workspace")
  expect_match(markup, "material-preview-card")
  for (id in c("work-material_mode", "work-input_source", "work-filter_file")) {
    matches <- gregexpr(paste0('id="', id, '"'), markup, fixed = TRUE)[[1L]]
    expect_equal(sum(matches > 0), 1L)
  }
  expect_false(grepl('id="work-catalogue_collection"', markup, fixed = TRUE))
  expect_false(grepl('id="work-catalogue_category"', markup, fixed = TRUE))
  expect_false(grepl('id="work-catalogue_filter"', markup, fixed = TRUE))
})

test_that("required material decisions have local feedback and independent confirmation panels", {
  old_language <- the$language
  withr::defer(the$language <- old_language)
  the$language <- "English"
  shiny::testServer(transmissionServer, args = list(
    workspace = TRUE,
    fixture_data = shiny::reactive(transmission_fixture("neutral"))
  ), {
    session$setInputs(input_source = "upload", material_mode = "transmission")
    session$setInputs(filter_name = "", scale = "", transmittance_type = "",
      scattering = "yes", measurement_geometry = "")
    returned <- session$getReturned()
    expect_false(returned$ready())
    for (field in c("filter_name", "scale", "transmittance_type", "measurement_geometry")) {
      expect_match(output[[paste0(field, "_feedback")]]$html,
        "material-field-message", fixed = TRUE)
    }
    session$setInputs(filter_name = "Measured sample", scale = "fraction",
      transmittance_type = "unknown", measurement_geometry = "8 degree/diffuse")
    expect_false(returned$ready())
    for (field in c("filter_name", "scale", "transmittance_type", "measurement_geometry")) {
      expect_null(output[[paste0(field, "_feedback")]])
    }
    expect_match(output$type_acknowledgement$html, "Required before calculation", fixed = TRUE)
    expect_match(output$scattering_acknowledgement$html, "8 degree/diffuse", fixed = TRUE)
    expect_match(output$material_details$html, "Optional measurement details", fixed = TRUE)
    expect_false(grepl('id="proxy1-type_acknowledgement"', output$material_details$html, fixed = TRUE))
    session$setInputs(type_ack = TRUE, scattering_ack = TRUE)
    expect_true(returned$ready())
    expect_identical(returned$metadata()$measurement_instrument, "")
    expect_identical(returned$metadata()$relative_measurement_error, "")
    session$setInputs(measurement_geometry = "")
    expect_false(returned$ready())
    expect_match(output$measurement_geometry_feedback$html, "measurement geometry", fixed = TRUE)
  })
})

test_that("the unmounted source picker is idle and commits only confirmed imports", {
  old_language <- the$language
  old_palette <- the$palette
  withr::defer(the$language <- old_language)
  withr::defer(the$palette <- old_palette)
  the$language <- "English"
  the$palette <- "Lang"
  committed <- NULL
  state <- new_transmission_active_spectrum(
    transmission_source_fixture("d65"), "Original", "Test", 1L, "import", "node-1")
  shiny::testServer(material_source_server, args = list(
    current = shiny::reactive(state), history = shiny::reactive(NULL),
    on_import = function(request) committed <<- request
  ), {
    session$flushReact()
    expect_null(committed)
    expect_match(output$summary$html, "Original", fixed = TRUE)
    session$setInputs(`picker-examples-illu_eigen` = 100,
      `picker-examples-CCT_norm` = 6500)
    session$flushReact()
    expect_null(committed)
    session$setInputs(`picker-examples-norm-norm` = 1L)
    session$flushReact()
    expect_type(committed, "list")
    expect_equal(material_photopic_lux(committed$spectrum), 100, tolerance = 1e-10)
    expect_equal(state$name, "Original")
  })
})

test_that("workspace corrects negative measurements and allows continuation", {
  source <- transmission_source_fixture("d65")
  source$Bestrahlungsstaerke[[1L]] <- -1e-6
  shiny::testServer(transmissionServer, args = list(
    workspace = TRUE, incident_spectrum = shiny::reactive(source),
    incident_name = shiny::reactive("Measured spectrum")
  ), {
    session$setInputs(input_source = "catalogue", material_mode = "transmission")
    withCallingHandlers(session$setInputs(`apply-apply_filter` = 1L),
      warning = function(w) {
        if (startsWith(conditionMessage(w), "Removed 1 row containing missing values"))
          invokeRestart("muffleWarning")
      })
    session$flushReact()
    returned <- session$getReturned()
    snapshot <- returned$applied_snapshot()
    expect_equal(snapshot$incident_spectrum$Bestrahlungsstaerke[[1L]], 0)
    expect_equal(snapshot$transmitted_spectrum$Bestrahlungsstaerke[[1L]], 0)
    expect_equal(snapshot$metadata$source_preprocessing$samples$measured_irradiance_w_m2_nm, -1e-6)
    expect_true(returned$can_promote())
    expect_true(returned$can_download())
    expect_match(output$workspace_result_actions$html, "set to 0", fixed = TRUE)
    expect_equal(source$Bestrahlungsstaerke[[1L]], -1e-6)
  })
})
