test_that("mode changes invalidate results and TUB defaults are usable without visiting Details", {
  shiny::testServer(
    transmissionServer,
    args = list(
      incident_spectrum = shiny::reactive(transmission_source_fixture("d65")),
      incident_name = shiny::reactive("D65")
    ),
    {
      session$setInputs(
        material_mode = "reflection",
        input_source = "catalogue",
        catalogue_collection = "tub67600",
        catalogue_filter = "tub:reflection:WF5"
      )
      session$flushReact()
      returned <- session$getReturned()
      expect_true(returned$ready())
      expect_equal(returned$metadata()$material_mode, "reflection")
      expect_match(returned$metadata()$filter_name, "Oak")
      session$setInputs(`apply-apply_filter` = 1L)
      session$flushReact()
      expect_equal(returned$applied_snapshot()$material_mode, "reflection")
      expect_equal(
        returned$applied_snapshot()$output_quantity,
        "spectral_radiant_exitance"
      )
      session$setInputs(material_mode = "transmission")
      session$flushReact()
      expect_false(returned$ready())
      expect_true(returned$applied_stale())
      session$setInputs(catalogue_filter = "tub:transmission:G1")
      session$flushReact()
      expect_true(returned$ready())
      session$setInputs(`apply-apply_filter` = 2L)
      session$flushReact()
      expect_equal(returned$applied_snapshot()$material_mode, "transmission")
    }
  )
})

test_that("catalogue updates retain localized names and measurement metadata", {
  old_language <- the$language
  withr::defer(the$language <- old_language)
  the$language <- "Deutsch"
  sent <- new.env(parent = emptyenv())
  original_text <- shiny::updateTextInput
  original_select <- shiny::updateSelectInput
  testthat::local_mocked_bindings(
    updateTextInput = function(
      session,
      inputId,
      label = NULL,
      value = NULL,
      ...
    ) {
      sent[[inputId]] <- list(value = value)
      original_text(session, inputId, label = label, value = value, ...)
    },
    updateSelectInput = function(
      session,
      inputId,
      label = NULL,
      choices = NULL,
      selected = NULL,
      ...
    ) {
      sent[[inputId]] <- list(value = selected)
      original_select(
        session,
        inputId,
        label = label,
        choices = choices,
        selected = selected,
        ...
      )
    },
    .package = "shiny"
  )
  shiny::testServer(transmissionServer, {
    session$setInputs(
      material_mode = "reflection",
      input_source = "catalogue",
      catalogue_collection = "tub67600",
      catalogue_filter = "tub:reflection:WF5"
    )
    session$flushReact()
    expect_match(sent$filter_name$value, "Eiche", fixed = TRUE)
    expect_equal(sent$scattering$value, "unknown")
    expect_match(
      sent$measurement_geometry$value,
      "Ulbricht",
      fixed = TRUE
    )
    session$setInputs(
      material_mode = "transmission",
      catalogue_filter = "tub:transmission:PC2"
    )
    session$flushReact()
    expect_equal(sent$scattering$value, "yes")
    expect_match(
      sent$measurement_geometry$value,
      "Ulbricht",
      fixed = TRUE
    )
  })
})
