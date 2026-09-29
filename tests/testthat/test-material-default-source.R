test_that("automatic D65 starts at 100 lx and never replaces an existing source", {
  old_language <- the$language
  withr::defer(the$language <- old_language)
  the$language <- "Deutsch"
  state <- shiny::reactiveValues()
  shiny::isolate({
    expect_true(activate_spectran_default_daylight(state))
    expect_equal(material_photopic_lux(state$Spectrum), 100, tolerance = 1e-12)
    expect_equal(state$Illu, 100, tolerance = 1e-10)
    expect_identical(state$Name, material_text("default_daylight_name"))
    expect_true(state$automatic_source)
    expect_true(state$committed_state$automatic_source)
    expect_identical(state$provenance$reference_illuminant, "CIE D65")
    expect_identical(state$node_id, "node-1")
    original <- state$Spectrum
    ratios <- original$Bestrahlungsstaerke /
      d65_visible_spectrum()$Bestrahlungsstaerke
    expect_equal(max(ratios), min(ratios), tolerance = 1e-12)
    expect_false(activate_spectran_default_daylight(state))
    expect_identical(state$revision, 1L)
    expect_identical(state$Spectrum, original)
    for (change in c("promotion", "restore")) {
      activate_spectran_spectrum(
        state,
        original,
        "Derived",
        "Test",
        change,
        "node-2"
      )
      expect_true(state$automatic_source)
    }
    state$automatic_source <- FALSE
    restore_spectran_committed_state(state)
    expect_true(state$automatic_source)
    explicit <- transmission_source_fixture("zero")
    activate_spectran_spectrum(
      state,
      explicit,
      "Explicit zero",
      "Import",
      "import",
      "node-1"
    )
    expect_false(state$automatic_source)
    expect_false(activate_spectran_default_daylight(state))
    expect_equal(state$Spectrum, explicit)
    expect_identical(state$Name, "Explicit zero")
  })
})

test_that("the full app opens materials directly and replaces automatic D65 only on import", {
  old_language <- the$language
  withr::defer(the$language <- old_language)
  app <- Spectran("Deutsch")
  sidebar <- as.character(UI_Sidebar())
  expect_match(sidebar, 'data-value="transmission"', fixed = TRUE)
  shiny::testServer(app, {
    # Mock sessions render hidden legacy plots without full browser bindings.
    # Ignore only their empty colour-scale warning; state errors still fail.
    withCallingHandlers(
      {
        session$setInputs(
          inTabset = "tutorial",
          `import-examples-down_import` = "Import",
          `import-examples-illu_eigen` = 100,
          `import-examples-CCT_norm` = 6500,
          `analysis-age-Alter` = 50,
          `analysis-age-Hintergrund` = TRUE,
          `analysis-photo-CIE_grenzen` = TRUE,
          `analysis-photo-Sensitivity` = TRUE,
          `analysis-photo-Hintergrund` = TRUE,
          `analysis-photo-Testfarben` = FALSE,
          `analysis-age-Alter_mel` = TRUE,
          `analysis-age-Alter_inset` = TRUE,
          `analysis-age-Alter_rel` = FALSE,
          `analysis-age-plot_multiplier` = 1,
          `analysis-radio-sensitivitaeten` = unname(Specs$Plot$Names[c(1, 6)]),
          `analysis-alpha-Sensitivity` = TRUE,
          `analysis-alpha-Hintergrund` = TRUE,
          `analysis-alpha-Vergleich` = FALSE
        )
        expect_null(Spectrum$Spectrum)
        expect_null(transmission_module())
        expect_null(Transmission$history())
        session$setInputs(inTabset = "import")
        expect_null(transmission_module())
        session$setInputs(inTabset = "transmission")
        initialized_module <- transmission_module()
        expect_type(initialized_module, "list")
        expect_equal(
          material_photopic_lux(Spectrum$Spectrum),
          100,
          tolerance = 1e-12
        )
        expect_true(Spectrum$automatic_source)
        expect_match(output$material_source_notice$html, "100 lx", fixed = TRUE)
        expect_identical(
          Transmission$history()$nodes[["node-1"]]$name,
          material_text("default_daylight_name")
        )
        original <- Spectrum$Spectrum
        revision <- Spectrum$revision
        session$setInputs(inTabset = "import")
        session$setInputs(inTabset = "transmission")
        expect_identical(transmission_module(), initialized_module)
        expect_identical(Spectrum$revision, revision)
        expect_identical(Spectrum$Spectrum, original)
        session$setInputs(
          `transmission-material_mode` = "transmission",
          `transmission-input_source` = "catalogue",
          `transmission-catalogue_collection` = "tub67600",
          `transmission-catalogue_filter` = "tub:transmission:G1"
        )
        session$setInputs(`transmission-apply-apply_filter` = 1L)
        expect_true(initialized_module$can_promote())
        session$setInputs(
          `transmission-history-promotion_name` = "First result",
          `transmission-history-promote` = 1L
        )
        expect_identical(Spectrum$node_id, "node-2")
        expect_identical(Spectrum$Name, "First result")
        expect_true(Spectrum$automatic_source)
        expect_identical(Transmission$history()$active_node_id, "node-2")
        session$setInputs(inTabset = "analysis")
        session$setInputs(inTabset = "transmission")
        expect_identical(transmission_module(), initialized_module)
        expect_length(Transmission$history()$nodes, 2L)
        session$setInputs(
          `transmission-history-node_action` = list(
            node = "node-1",
            action = "restore"
          )
        )
        expect_identical(Spectrum$node_id, "node-1")
        expect_equal(Spectrum$Spectrum, original)
        expect_length(Transmission$history()$nodes, 2L)
        explicit <- transmission_source_fixture("d65", target_lux = 250)
        request <- list(
          source_id = "explicit",
          spectrum = explicit,
          source_name = "Explicit",
          origin = "Test import"
        )
        Spectrum$import_guard(request)
        session$flushReact()
        expect_true(Spectrum$automatic_source)
        expect_length(Transmission$history()$nodes, 2L)
        session$setInputs(`import-history_reset-cancel_import` = 1L)
        expect_equal(Spectrum$Spectrum, original)
        Spectrum$import_guard(request)
        session$flushReact()
        session$setInputs(`import-history_reset-confirm_import` = 1L)
        session$flushReact()
        expect_length(Transmission$history()$nodes, 1L)
        expect_null(output$material_source_notice)
        session$setInputs(inTabset = "analysis")
        session$setInputs(inTabset = "transmission")
        expect_equal(Spectrum$Spectrum, explicit)
        expect_false(Spectrum$automatic_source)
        expect_identical(transmission_module(), initialized_module)
      },
      warning = function(w) {
        if (startsWith(conditionMessage(w), "No shared levels found between"))
          invokeRestart("muffleWarning")
      }
    )
  })
})
