test_that("production tabs keep a live preview beside every input section", {
  old_language <- the$language
  withr::defer(the$language <- old_language)
  the$language <- "English"

  rendered <- as.character(transmissionUI(
    "tabs",
    default_source = "catalogue",
    layout = "tabs"
  ))

  expect_match(rendered, "tabs-section", fixed = TRUE)
  expect_match(rendered, ">Transmission spectrum<", fixed = TRUE)
  expect_match(rendered, ">Results<", fixed = TRUE)
  expect_match(rendered, ">Promotion and History<", fixed = TRUE)
  expect_match(rendered, ">Export<", fixed = TRUE)
  expect_match(rendered, "tabs-preview_outputs_spectrum", fixed = TRUE)
  expect_match(rendered, "tabs-preview_outputs_measurement", fixed = TRUE)
  expect_match(rendered, "tabs-preview_outputs_normalization", fixed = TRUE)
  expect_match(rendered, "position: sticky", fixed = TRUE)
  expect_match(rendered, "tabs-apply-apply_controls", fixed = TRUE)
  expect_match(rendered, "tabs-history-promote", fixed = TRUE)
  expect_match(rendered, "tabs-navigation_spectrum", fixed = TRUE)
  expect_match(rendered, "tabs-navigation_results", fixed = TRUE)
  expect_match(
    rendered,
    '<details class="transmission-csv-settings">',
    fixed = TRUE
  )
  expect_false(grepl(
    '<details class="transmission-csv-settings" open',
    rendered,
    fixed = TRUE
  ))

  for (context in c("spectrum", "measurement", "normalization")) {
    preview_id <- paste0('id="tabs-preview_outputs_', context, '"')
    matches <- gregexpr(preview_id, rendered, fixed = TRUE)[[1L]]
    expect_length(matches[matches > 0L], 1L)
  }

  expect_match(
    rendered,
    'data-tabsetid="spectran-tabs-section"',
    fixed = TRUE
  )
  for (index in seq_len(6L)) {
    target <- paste0("tab-spectran-tabs-section-", index)
    expect_match(rendered, paste0('href="#', target, '"'), fixed = TRUE)
    expect_match(rendered, paste0('id="', target, '"'), fixed = TRUE)
  }
})

test_that("deterministic Transmission tabsets cannot collide", {
  first <- transmission_tabset_panel(
    id = "transmission-section",
    shiny::tabPanel("Spectrum", value = "spectrum", "First spectrum"),
    shiny::tabPanel("Results", value = "results", "First results"),
    selected = "spectrum"
  )
  second <- transmission_tabset_panel(
    id = "transmission-apply-metric_table_tabs",
    shiny::tabPanel("D65", value = "d65", "Second D65"),
    shiny::tabPanel("Light", value = "light", "Second light"),
    selected = "d65"
  )
  rendered <- paste(as.character(first), as.character(second))

  first_ids <- c(
    "tab-spectran-transmission-section-1",
    "tab-spectran-transmission-section-2"
  )
  second_ids <- c(
    "tab-spectran-transmission-apply-metric_table_tabs-1",
    "tab-spectran-transmission-apply-metric_table_tabs-2"
  )
  for (target in c(first_ids, second_ids)) {
    matches <- gregexpr(
      paste0('id="', target, '"'),
      rendered,
      fixed = TRUE
    )[[1L]]
    expect_length(matches[matches > 0L], 1L)
    expect_match(rendered, paste0('href="#', target, '"'), fixed = TRUE)
  }
  expect_false(any(first_ids %in% second_ids))
})

test_that("programmatic tab scrolling targets the selected tab only", {
  script <- transmission_tab_scroll_script(
    "transmission-section",
    "results"
  )
  expect_match(
    script,
    "document.getElementById('transmission-section')",
    fixed = TRUE
  )
  expect_match(script, 'a[data-value="results"]', fixed = TRUE)
  expect_match(script, "requestAnimationFrame", fixed = TRUE)
  expect_match(
    script,
    "setTimeout(function () { adjustItem(item); }, 320)",
    fixed = TRUE
  )
  expect_match(
    script,
    "setTimeout(function () { adjustItem(item); }, 1600)",
    fixed = TRUE
  )
  expect_match(
    script,
    "itemRect.right > viewRect.right - padding",
    fixed = TRUE
  )
  expect_match(script, "viewRect.left + padding - itemRect.left", fixed = TRUE)
  expect_match(script, "tabset.scrollLeft", fixed = TRUE)
  expect_match(script, "getBoundingClientRect", fixed = TRUE)
  expect_error(
    transmission_tab_scroll_script("unsafe id", "results"),
    "unsupported"
  )

  observer_script <- transmission_tab_observer_script("transmission-section")
  expect_match(observer_script, "MutationObserver", fixed = TRUE)
  expect_match(observer_script, "querySelector('li.active')", fixed = TRUE)
  expect_match(observer_script, "data-spectran-tab-scroll", fixed = TRUE)
  expect_match(observer_script, "setTimeout(adjust, 320)", fixed = TRUE)
  expect_match(observer_script, "setTimeout(adjust, 1600)", fixed = TRUE)
  expect_match(observer_script, "__spectranTabScrollObserver", fixed = TRUE)
  expect_match(observer_script, "shown.bs.tab.spectranTabScroll", fixed = TRUE)
  expect_match(
    observer_script,
    "itemRect.right > viewRect.right - padding",
    fixed = TRUE
  )
  expect_error(
    transmission_tab_observer_script("unsafe id"),
    "unsupported"
  )

  keyboard_script <- transmission_guided_keyboard_script()
  expect_match(
    keyboard_script,
    ".transmission-guided-action",
    fixed = TRUE
  )
  expect_match(keyboard_script, "event.preventDefault()", fixed = TRUE)
  expect_match(keyboard_script, "target.click()", fixed = TRUE)
  expect_match(
    keyboard_script,
    "__spectranTransmissionGuidedKeyboard",
    fixed = TRUE
  )

  checkbox_script <- spectran_checkbox_keyboard_script()
  expect_match(
    checkbox_script,
    "target instanceof HTMLInputElement",
    fixed = TRUE
  )
  expect_match(
    checkbox_script,
    "target.type !== 'checkbox'",
    fixed = TRUE
  )
  expect_match(checkbox_script, "event.key === ' '", fixed = TRUE)
  expect_match(checkbox_script, "event.code === 'Space'", fixed = TRUE)
  expect_match(checkbox_script, "event.preventDefault()", fixed = TRUE)
  expect_match(checkbox_script, "target.click()", fixed = TRUE)
  expect_match(
    checkbox_script,
    "document.getElementById(targetId)",
    fixed = TRUE
  )
  expect_match(
    checkbox_script,
    "current.focus({preventScroll: true})",
    fixed = TRUE
  )
  expect_match(
    checkbox_script,
    "active !== original && active !== document.body",
    fixed = TRUE
  )
  expect_match(
    checkbox_script,
    "__spectranCheckboxKeyboard",
    fixed = TRUE
  )
})

test_that("Transmission source files contain no bound gt Shiny widgets", {
  source_files <- list.files(
    testthat::test_path("..", "..", "R"),
    pattern = "^transmission.*[.]R$",
    full.names = TRUE
  )
  source_lines <- unlist(lapply(source_files, readLines, warn = FALSE))
  code_lines <- source_lines[!grepl("^\\s*#", source_lines)]

  expect_false(any(grepl("gt::gt_output", code_lines, fixed = TRUE)))
  expect_false(any(grepl("gt::render_gt", code_lines, fixed = TRUE)))
})

test_that("tabs are directly accessible while Apply retains scientific gating", {
  old_language <- the$language
  withr::defer(the$language <- old_language)
  the$language <- "English"

  fixture <- shiny::reactive(transmission_fixture("neutral"))
  source <- shiny::reactive(transmission_source_fixture("d65"))

  shiny::testServer(
    transmissionServer,
    args = list(
      fixture_data = fixture,
      incident_spectrum = source,
      incident_name = shiny::reactive("CIE D65 at 250 lx")
    ),
    {
      session$flushReact()
      session$setInputs(
        filter_name = "Neutral 50%",
        scale = "fraction",
        transmittance_type = "total",
        scattering = "no",
        section = "spectrum"
      )
      session$flushReact()
      returned <- session$getReturned()
      expect_identical(returned$active_section(), "spectrum")
      expect_true(returned$ready())
      expect_true(returned$apply_ready())

      session$setInputs(section = "measurement")
      session$flushReact()
      expect_identical(returned$active_section(), "measurement")

      session$setInputs(section = "normalization")
      session$flushReact()
      expect_identical(returned$active_section(), "normalization")

      session$setInputs(section = "results")
      session$flushReact()
      expect_identical(returned$active_section(), "results")
      expect_null(returned$applied_snapshot())

      session$setInputs(`apply-apply_filter` = 1)
      session$flushReact()
      expect_s3_class(
        returned$applied_snapshot(),
        "transmission_applied_snapshot"
      )
      expect_match(
        output[["apply-d65_properties"]]$html,
        "gt_table",
        fixed = TRUE
      )
      expect_match(
        output[["apply-absolute_metrics"]]$html,
        "gt_table",
        fixed = TRUE
      )
      expect_match(
        output[["apply-balance_metrics"]]$html,
        "gt_table",
        fixed = TRUE
      )

      session$setInputs(section = "history")
      session$flushReact()
      expect_identical(returned$active_section(), "history")
    }
  )
})

test_that("the guided Spectrum action applies a complete default draft", {
  old_language <- the$language
  withr::defer(the$language <- old_language)
  the$language <- "English"

  shiny::testServer(
    transmissionServer,
    args = list(
      fixture_data = shiny::reactive(transmission_fixture("neutral")),
      incident_spectrum = shiny::reactive(transmission_source_fixture("d65")),
      incident_name = shiny::reactive("CIE D65 at 250 lx")
    ),
    {
      session$flushReact()
      session$setInputs(
        filter_name = "Neutral 50%",
        scale = "fraction",
        transmittance_type = "total",
        scattering = "no"
      )
      session$flushReact()

      expect_match(
        output$navigation_spectrum$html,
        "Apply filter and show Results",
        fixed = TRUE
      )
      expect_match(
        output[["apply-apply_controls"]]$html,
        "Apply transmission filter",
        fixed = TRUE
      )

      session$setInputs(spectrum_forward = 1)
      session$flushReact()

      expect_s3_class(
        session$getReturned()$applied_snapshot(),
        "transmission_applied_snapshot"
      )
      expect_null(output[["apply-apply_controls"]]$html)
    }
  )
})

test_that("guided labels use the renamed Details destination", {
  old_language <- the$language
  withr::defer(the$language <- old_language)
  the$language <- "English"

  shiny::testServer(
    transmissionServer,
    args = list(
      fixture_data = shiny::reactive(transmission_fixture("neutral")),
      incident_spectrum = shiny::reactive(transmission_source_fixture("d65")),
      incident_name = shiny::reactive("CIE D65 at 250 lx")
    ),
    {
      session$flushReact()
      session$setInputs(
        filter_name = "Qualified filter",
        scale = "fraction",
        transmittance_type = "internal",
        scattering = "no",
        type_ack = FALSE
      )
      session$flushReact()

      expect_match(
        output$navigation_spectrum$html,
        "Continue to Details",
        fixed = TRUE
      )
      expect_match(
        output$navigation_measurement$html,
        "Complete required details",
        fixed = TRUE
      )

      session$setInputs(type_ack = TRUE)
      session$flushReact()
      expect_match(
        output$navigation_measurement$html,
        "Apply filter and show Results",
        fixed = TRUE
      )
    }
  )
})

test_that("missing requirements are grouped by their resolution tab", {
  scale_invalid <- prepare_transmission_curve(
    tibble::tibble(
      source_row = 1:2,
      wavelength_nm = c(380, 780),
      value = c(50, 50)
    ),
    scale = "fraction"
  )
  scale_groups <- transmission_requirement_groups(
    upload = NULL,
    preparation = scale_invalid
  )
  expect_length(scale_groups$spectrum, 0L)
  expect_true(any(grepl(
    "Fraction or Percent",
    scale_groups$measurement,
    fixed = TRUE
  )))

  partial <- prepare_transmission_curve(
    transmission_fixture("partial"),
    scale = "fraction"
  )
  partial_groups <- transmission_requirement_groups(
    upload = NULL,
    preparation = partial,
    metadata_requirements = "Confirm measurement scope."
  )
  expect_true(any(grepl(
    "lower tail",
    partial_groups$normalization,
    fixed = TRUE
  )))
  expect_true("Confirm measurement scope." %in% partial_groups$measurement)

  upload_groups <- transmission_requirement_groups(
    upload = list(error = "Only one column was detected."),
    preparation = NULL
  )
  expect_identical(
    upload_groups$spectrum,
    "Only one column was detected."
  )
})

test_that("invalid sequential Apply opens recovery after state settles", {
  old_language <- the$language
  withr::defer(the$language <- old_language)
  the$language <- "English"

  fixture <- shiny::reactiveVal(transmission_fixture("neutral"))
  active <- shiny::reactiveVal(new_transmission_active_spectrum(
    transmission_source_fixture("d65"),
    "CIE D65 at 250 lx",
    "Development fixture",
    1L,
    "import",
    "node-1"
  ))

  shiny::testServer(
    transmissionServer,
    args = list(
      fixture_data = shiny::reactive(fixture()),
      incident_spectrum = shiny::reactive(active()$spectrum),
      incident_name = shiny::reactive(active()$name),
      active_state = shiny::reactive(active())
    ),
    {
      session$flushReact()
      session$setInputs(
        filter_name = "Neutral 50%",
        scale = "fraction",
        transmittance_type = "total",
        scattering = "no",
        `apply-apply_filter` = 1
      )
      session$flushReact()
      returned <- session$getReturned()

      session$setInputs(
        `history-promotion_name` = "Promoted neutral result",
        `history-promote` = 1
      )
      session$flushReact()
      promotion <- returned$promotion_event()
      expect_s3_class(promotion, "transmission_activation_event")

      active(transmission_active_spectrum_from_event(promotion, 2L))
      session$flushReact()
      session$setInputs(`apply-apply_filter` = 2)
      session$flushReact()
      expect_s3_class(
        returned$applied_snapshot(),
        "transmission_applied_snapshot"
      )

      session$setInputs(
        transmittance_type = "unknown",
        type_ack = FALSE,
        section = "results"
      )
      session$flushReact()
      expect_false(returned$apply_ready())
      before <- returned$missing_modal_sequence()

      session$setInputs(`apply-apply_filter` = 3)
      session$flushReact()
      expect_identical(returned$missing_modal_sequence(), before + 1L)
    }
  )
})

test_that("a parseable percentage upload can reach its scale decision", {
  upload <- tibble::tibble(
    source_row = seq_along(seq(380, 780, by = 5)),
    wavelength_nm = seq(380, 780, by = 5),
    value = rep(50, length(seq(380, 780, by = 5)))
  )
  fraction_draft <- prepare_transmission_curve(upload, "fraction")

  expect_true(transmission_has_parseable_curve(fraction_draft))
  expect_match(
    paste(fraction_draft$diagnostics$errors, collapse = " "),
    "Check whether Fraction or Percent is selected correctly",
    fixed = TRUE
  )

  fixture <- shiny::reactive(upload)
  shiny::testServer(
    transmissionServer,
    args = list(
      fixture_data = fixture,
      incident_spectrum = shiny::reactive(
        transmission_source_fixture("d65")
      ),
      incident_name = shiny::reactive("CIE D65 at 250 lx")
    ),
    {
      session$flushReact()
      session$setInputs(
        filter_name = "Percentage filter",
        scale = "fraction",
        transmittance_type = "total",
        scattering = "no",
        section = "spectrum"
      )
      session$flushReact()
      returned <- session$getReturned()
      expect_true(transmission_has_parseable_curve(returned$preparation()))
      expect_false(returned$ready())

      session$setInputs(section = "measurement")
      session$flushReact()
      expect_identical(returned$active_section(), "measurement")
      expect_false(returned$apply_ready())

      session$setInputs(scale = "percent")
      session$flushReact()
      expect_true(returned$ready())
      expect_true(returned$apply_ready())
    }
  )
})

test_that("source imports reset the tabs to Spectrum", {
  expect_identical(
    transmission_tabs_reset_section(list(change_type = "import")),
    "spectrum"
  )
  expect_null(
    transmission_tabs_reset_section(list(change_type = "promotion"))
  )
  expect_null(
    transmission_tabs_reset_section(list(change_type = "restore"))
  )
  expect_null(transmission_tabs_reset_section(NULL))
})

test_that("Transmission is an optional sidebar page after Export", {
  old_language <- the$language
  withr::defer(the$language <- old_language)
  the$language <- "English"

  rendered <- as.character(UI_Sidebar())
  positions <- c(
    analysis = regexpr('id="analysis"', rendered, fixed = TRUE)[[1L]],
    export = regexpr('id="export"', rendered, fixed = TRUE)[[1L]],
    transmission = regexpr(
      'id="transmission"',
      rendered,
      fixed = TRUE
    )[[1L]]
  )

  expect_true(all(positions > 0L))
  expect_true(positions[["analysis"]] < positions[["export"]])
  expect_true(positions[["export"]] < positions[["transmission"]])
  expect_identical(lang$ui(62), "Import spectrum and open Analysis")
})

test_that("a no-curve tab preview stays compact", {
  shiny::testServer(
    transmissionServer,
    {
      session$flushReact()
      session$setInputs(input_source = "upload")
      session$flushReact()

      preview <- output$preview_outputs$html
      expect_match(preview, "transmission-no-curve-note", fixed = TRUE)
      expect_false(grepl("construction_plot", preview, fixed = TRUE))
      expect_match(
        preview,
        "No parseable curve is available yet",
        fixed = TRUE
      )
    }
  )
})

test_that("live construction plot persists for missing and invalid drafts", {
  old_language <- the$language
  withr::defer(the$language <- old_language)
  the$language <- "English"

  empty_plot <- transmission_construction_plot(NULL)
  empty_build <- ggplot2::ggplot_build(empty_plot)
  expect_equal(empty_build$layout$panel_scales_y[[1L]]$get_limits(), c(0, 1))
  expect_equal(
    empty_build$layout$panel_scales_x[[1L]]$get_breaks(),
    c(380, 480, 580, 680, 780)
  )

  invalid <- prepare_transmission_curve(
    transmission_fixture("invalid"),
    "fraction"
  )
  invalid_plot <- transmission_construction_plot(invalid)
  invalid_build <- suppressWarnings(ggplot2::ggplot_build(invalid_plot))
  expect_equal(
    invalid_build$layout$panel_scales_y[[1L]]$get_limits(),
    c(0, 1)
  )
  expect_true(any(vapply(
    invalid_plot$layers,
    function(layer) inherits(layer$geom, "GeomLabel"),
    logical(1)
  )))

  partial <- prepare_transmission_curve(
    transmission_fixture("partial"),
    "fraction"
  )
  expect_false(partial$diagnostics$ready)
  partial_plot <- transmission_construction_plot(partial)
  partial_build <- ggplot2::ggplot_build(partial_plot)
  expect_equal(
    partial_build$layout$panel_scales_y[[1L]]$get_limits(),
    c(0, 1)
  )
  expect_gt(nrow(partial_build$data[[4L]]), 0L)
  expect_equal(
    partial_build$layout$panel_scales_x[[1L]]$get_breaks(),
    c(380, 480, 580, 680, 780)
  )
})

test_that("focusable edge containers reserve complete outline space", {
  rendered <- as.character(transmissionUI("focus"))

  expect_match(
    rendered,
    "transmission-download-grid",
    fixed = TRUE
  )
  expect_match(
    rendered,
    "max-width: 100%; padding: 7px",
    fixed = TRUE
  )
  expect_match(
    rendered,
    "box-shadow: inset 0 0 0 3px #005fcc",
    fixed = TRUE
  )
})

test_that("integrated Import contains wide content within the page", {
  old_language <- the$language
  withr::defer(the$language <- old_language)
  the$language <- "English"

  rendered <- as.character(importUI("integrated-import"))
  expect_match(rendered, "spectran-import-page", fixed = TRUE)
  expect_match(
    rendered,
    paste0(
      "Analysis opens afterwards, and Transmission is unlocked as an ",
      "optional page after Export."
    ),
    fixed = TRUE
  )

  stylesheet_path <- system.file(
    "app/www/style.css",
    package = "Spectran"
  )
  expect_true(nzchar(stylesheet_path))
  stylesheet <- paste(
    readLines(stylesheet_path, warn = FALSE),
    collapse = "\n"
  )
  expect_match(stylesheet, ".spectran-import-page", fixed = TRUE)
  expect_match(stylesheet, "overflow-x: clip", fixed = TRUE)
  expect_match(stylesheet, "overflow-x: auto", fixed = TRUE)
  expect_match(
    stylesheet,
    paste(
      ".content-wrapper {",
      "    background-color: #F0F0F0;",
      "    overflow-y: hidden;",
      "}",
      sep = "\n"
    ),
    fixed = TRUE
  )
  expect_match(
    stylesheet,
    paste(
      "body {",
      "    overflow-y: scroll;",
      "}",
      sep = "\n"
    ),
    fixed = TRUE
  )
  expect_false(grepl(
    paste(".content,", ".tab-content,", ".tab-pane,", sep = "\n"),
    stylesheet,
    fixed = TRUE
  ))
})
