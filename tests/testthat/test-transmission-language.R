test_that("semantic transmission strings are complete in both languages", {
  workbook_names <- language$ui$Name[
    startsWith(language$ui$Name, "transmission.")
  ]

  expect_gt(length(workbook_names), 150L)
  expect_identical(anyDuplicated(workbook_names), 0L)
  for (setting in c("English", "Deutsch")) {
    values <- language$ui[[setting]][match(workbook_names, language$ui$Name)]
    expect_false(anyNA(values))
    expect_true(all(nzchar(values)))
  }
})

test_that("integrated tabs render English and German from the workbook", {
  old_language <- if (exists("language", envir = the, inherits = FALSE)) {
    the$language
  } else {
    NULL
  }
  withr::defer({
    if (is.null(old_language)) {
      rm("language", envir = the)
    } else {
      the$language <- old_language
    }
  })

  the$language <- "English"
  expect_identical(
    lang$ui(62),
    "Import spectrum and open Analysis"
  )
  expect_identical(
    lang$ui(91),
    "Use spectrum and open Analysis"
  )
  english <- as.character(transmissionUI(
    "english",
    default_source = "catalogue",
    layout = "tabs"
  ))
  expect_match(english, "Transmission sections", fixed = TRUE)
  expect_match(english, "Live normalization preview", fixed = TRUE)
  expect_match(english, "Results", fixed = TRUE)
  expect_match(english, "Promotion and History", fixed = TRUE)
  expect_match(english, ">Export<", fixed = TRUE)
  expect_match(english, "english-history-archive_section", fixed = TRUE)
  expect_identical(
    transmission_text("archive_heading"),
    "Archived promoted results"
  )
  expect_identical(
    transmission_text("continue_details"),
    "Continue to Details"
  )
  expect_identical(
    transmission_text("complete_details"),
    "Complete required details"
  )
  expect_identical(transmission_text("show_action_spectra"), "Action spectra")
  expect_identical(
    transmission_text("plot_result_subtitle"),
    "Transmitted (solid) and incident (dashed)"
  )
  expect_identical(transmission_text("export_plot_settings"), "Graph settings")
  expect_identical(
    transmission_text("download_plot_table"),
    "Plot plus table (PNG)"
  )
  expect_identical(
    transmission_text("download_bundle"),
    "Create selected ZIP bundle"
  )
  expect_identical(
    transmission_text("export_bundle_progress_heading"),
    "Creating ZIP bundle"
  )
  expect_identical(
    transmission_text("export_bundle_progress_detail", 13L),
    paste(
      "13 selected files are being prepared.",
      "The download will be ready when preparation is complete."
    )
  )
  expect_identical(
    transmission_text("download_bundle_ready"),
    "Download prepared ZIP bundle"
  )

  the$language <- "Deutsch"
  expect_identical(
    lang$ui(62),
    "Spektrum importieren und zur Auswertung wechseln"
  )
  expect_identical(
    lang$ui(91),
    "Spektrum übernehmen und zur Auswertung wechseln"
  )
  german <- as.character(transmissionUI(
    "deutsch",
    default_source = "catalogue",
    layout = "tabs"
  ))
  expect_match(german, "Transmissionsbereiche", fixed = TRUE)
  expect_match(german, "Vorschaugrafik", fixed = TRUE)
  expect_match(german, "Ergebnisse", fixed = TRUE)
  expect_match(german, "Übernahme und Verlauf", fixed = TRUE)
  expect_match(german, ">Export<", fixed = TRUE)
  expect_match(german, "deutsch-history-archive_section", fixed = TRUE)
  expect_identical(
    transmission_text("archive_heading"),
    "Archivierte übernommene Ergebnisse"
  )
  expect_identical(
    transmission_text("balance_heading"),
    "Wirkfaktoren und Tageslicht-Effizienzverhältnisse"
  )
  expect_identical(
    transmission_text("metric_l_cone_opic_action_factor"),
    "L-Zapfen-Wirkfaktor"
  )
  expect_identical(
    transmission_text("continue_details"),
    "Weiter zu Details"
  )
  expect_identical(
    transmission_text("complete_details"),
    "Erforderliche Details vervollständigen"
  )
  expect_identical(transmission_text("show_action_spectra"), "Wirkfunktionen")
  expect_identical(
    transmission_text("plot_result_subtitle"),
    "Transmittiert (durchgezogen) und einfallend (gestrichelt)"
  )
  expect_identical(
    transmission_text("export_plot_settings"),
    "Diagrammeinstellungen"
  )
  expect_identical(
    transmission_text("download_plot_table"),
    "Diagramm und Tabelle (PNG)"
  )
  expect_identical(
    transmission_text("download_bundle"),
    "Ausgewähltes ZIP-Paket erstellen"
  )
  expect_identical(
    transmission_text("export_bundle_progress_heading"),
    "ZIP-Paket wird erstellt"
  )
  expect_identical(
    transmission_text("download_bundle_ready"),
    "Vorbereitetes ZIP-Paket herunterladen"
  )
  expect_match(german, "Fassaden- und Fensterverglasungen", fixed = TRUE)
  expect_false(grepl("Choose a transmission spectrum", german, fixed = TRUE))
})

test_that("readiness, metric labels, and Apply states follow German", {
  old_language <- the$language
  withr::defer(the$language <- old_language)
  the$language <- "Deutsch"

  fixture <- shiny::reactive(transmission_fixture("neutral"))
  shiny::testServer(
    transmissionServer,
    args = list(
      fixture_data = fixture,
      incident_spectrum = shiny::reactive(
        transmission_source_fixture("d65")
      ),
      incident_name = shiny::reactive("D65")
    ),
    {
      session$flushReact()
      session$setInputs(
        filter_name = "Neutralfilter 50 %",
        scale = "fraction",
        transmittance_type = "total",
        scattering = "no"
      )
      session$flushReact()

      expect_match(
        output$readiness$html,
        "Bereit: Die vervollständigte Kurve enthält 401 Berechnungszeilen.",
        fixed = TRUE
      )
      expect_match(
        output[["apply-source_summary"]]$html,
        "Gesamtbestrahlungsstärke",
        fixed = TRUE
      )
      expect_match(
        output[["apply-source_summary"]]$html,
        "photopische Beleuchtungsstärke",
        fixed = TRUE
      )

      session$setInputs(`apply-apply_filter` = 1)
      session$flushReact()
      applied_html <- output[["apply-applied_outputs"]]$html
      expect_match(
        applied_html,
        "Tabelle der D65-bezogenen Filtereigenschaften",
        fixed = TRUE
      )
      expect_false(grepl(
        "Current applied transmission results",
        applied_html,
        fixed = TRUE
      ))
    }
  )

  snapshot <- new_transmission_applied_snapshot(
    result = calculate_transmission_result(
      transmission_source_fixture("d65"),
      prepare_transmission_curve(
        transmission_fixture("neutral"),
        "fraction"
      )$completed
    ),
    metadata = list(filter_name = "Neutralfilter 50 %"),
    incident_name = "D65",
    draft_revision = 1L,
    apply_sequence = 1L
  )
  expect_identical(
    transmission_d65_display_table(snapshot)[[1L]][[1L]],
    "Lichttransmissionsgrad"
  )
  expect_identical(
    transmission_apply_status_state(snapshot, FALSE, TRUE)$message,
    "Die Ergebnisse für D65 sind aktuell."
  )
  expect_identical(
    format_transmission_metric(NA_real_, defined = FALSE),
    "Nicht definiert"
  )
  expect_identical(
    format_transmission_percent(NA_real_, defined = FALSE),
    "Nicht definiert"
  )

  comparison_plot <- transmission_spectral_comparison_plot(snapshot)
  expect_identical(comparison_plot$labels$x, "Wellenlänge (nm)")
  expect_identical(
    comparison_plot$labels$y,
    "Spektrale Bestrahlungsstärke (mW/m²/nm)"
  )
  expect_null(comparison_plot$labels$colour)

  construction_plot <- transmission_construction_plot(
    prepare_transmission_curve(transmission_fixture("neutral"), "fraction")
  )
  expect_identical(construction_plot$labels$x, "Wellenlänge (nm)")
  expect_identical(construction_plot$labels$y, "Transmission (Anteil)")
  expect_identical(
    construction_plot$labels$colour,
    "Erzeugung der Kurve"
  )

  zero_snapshot <- new_transmission_applied_snapshot(
    result = calculate_transmission_result(
      transmission_source_fixture("zero"),
      prepare_transmission_curve(
        transmission_fixture("neutral"),
        "fraction"
      )$completed
    ),
    metadata = list(filter_name = "Neutralfilter 50 %"),
    incident_name = "Nullspektrum",
    draft_revision = 1L,
    apply_sequence = 1L
  )
  warning_presentation <- transmission_metric_warning_presentation(
    zero_snapshot
  )
  warning_messages <- unlist(lapply(
    warning_presentation$details,
    `[[`,
    "messages"
  ))
  expect_true(all(grepl("nicht definiert", warning_messages, fixed = TRUE)))
  expect_false(any(grepl(" is undefined ", warning_messages, fixed = TRUE)))

  external_note <- paste0(
    "Outside-domain interval(s) 300–305 nm contain no completed ",
    "380–780 nm grid samples. They are shown for audit and boundary ",
    "context but require no interpolation consent."
  )
  expect_identical(
    unname(transmission_localize_diagnostics(external_note)),
    paste0(
      "Die Intervalle außerhalb des Berechnungsbereichs ",
      "(300–305 nm) enthalten keine Werte des vervollständigten ",
      "380–780-nm-Rasters. Sie werden für Audit und Randkontext ",
      "gezeigt, benötigen aber keine Interpolationsbestätigung."
    )
  )

  tail_diagnostics <- c(
    paste0(
      "Choose how to complete the lower tail (380–419 nm): ",
      "opaque at 0%, transparent at 100%, or carry the first supplied ",
      "value backward."
    ),
    paste0(
      "Choose how to complete the upper tail (761–780 nm): ",
      "opaque at 0%, transparent at 100%, or carry the last supplied ",
      "value forward."
    )
  )
  expect_identical(
    unname(transmission_localize_diagnostics(tail_diagnostics)),
    c(
      paste0(
        "Wählen Sie, wie der untere Randbereich (380–419 nm) ",
        "vervollständigt wird: opak mit 0 %, transparent mit 100 % oder ",
        "durch Rückwärtsfortführung des ersten Messwerts."
      ),
      paste0(
        "Wählen Sie, wie der obere Randbereich (761–780 nm) ",
        "vervollständigt wird: opak mit 0 %, transparent mit 100 % oder ",
        "durch Vorwärtsfortführung des letzten Messwerts."
      )
    )
  )

  diagnostic_examples <- c(
    "Supply at least two spectral samples.",
    paste0(
      "The selected wavelength column is not numeric. Problem values: ",
      "source row 2: \"abc\". Check the decimal mark, header setting, ",
      "rows to skip, and column selections."
    ),
    paste0(
      "Wavelengths contain missing or non-finite values at source row 3: ",
      "NA. Check the decimal mark, header setting, rows to skip, and ",
      "wavelength column."
    ),
    paste0(
      "Duplicate wavelengths are not allowed: 400 nm at source rows 2, 3."
    ),
    paste0(
      "Transmission must be between 0 and 1 after scaling. Affected source ",
      "row 2 (input 50, scaled fraction 50). Check whether Fraction or ",
      "Percent is selected correctly."
    ),
    paste0(
      "The file was parsed into 1 column, but wavelength column 1 and ",
      "transmission column 2 were selected. Check the separator, decimal ",
      "mark, header setting, and rows to skip, or select from column 1."
    )
  )
  localized <- transmission_localize_diagnostics(diagnostic_examples)
  expect_true(all(nzchar(localized)))
  expect_false(any(grepl(
    paste(
      c(
        "Supply at least",
        "selected wavelength",
        "Wavelengths contain",
        "Duplicate wavelengths",
        "Transmission must",
        "The file was parsed"
      ),
      collapse = "|"
    ),
    localized
  )))
})

test_that("German incomplete-tail readiness can be resolved", {
  old_language <- the$language
  withr::defer(the$language <- old_language)
  the$language <- "Deutsch"

  fixture <- shiny::reactive(transmission_fixture("partial"))
  shiny::testServer(
    transmissionServer,
    args = list(fixture_data = fixture),
    {
      session$flushReact()
      session$setInputs(
        filter_name = "Filter mit Teilabdeckung",
        scale = "fraction",
        transmittance_type = "total",
        scattering = "no"
      )
      session$flushReact()

      returned <- session$getReturned()
      requirements <- returned$diagnostics()$requirements
      expect_length(requirements, 2L)
      expect_match(requirements[[1L]], "opak mit 0 %", fixed = TRUE)
      expect_match(requirements[[1L]], "transparent mit 100 %", fixed = TRUE)
      expect_match(requirements[[2L]], "opak mit 0 %", fixed = TRUE)
      expect_match(requirements[[2L]], "transparent mit 100 %", fixed = TRUE)
      expect_false(returned$ready())

      session$setInputs(lower_tail = "zero", upper_tail = "one")
      session$flushReact()
      expect_true(returned$ready())
      expect_equal(
        returned$preparation()$completed$transmittance[c(1L, 401L)],
        c(0, 1)
      )
    }
  )
})
