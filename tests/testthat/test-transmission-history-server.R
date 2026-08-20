test_that("promotion, restore, branch, and import reset cross explicit events", {
  fixture <- shiny::reactiveVal(transmission_fixture("neutral"))
  root <- new_transmission_active_spectrum(
    transmission_source_fixture("d65"),
    "CIE D65 at 250 lx",
    "Development fixture",
    1L,
    "import",
    "node-1"
  )
  active <- shiny::reactiveVal(root)

  shiny::testServer(
    transmissionServer,
    args = list(
      fixture_data = fixture,
      incident_spectrum = shiny::reactive(active()$spectrum),
      incident_name = shiny::reactive(active()$name),
      active_state = shiny::reactive(active())
    ),
    {
      session$flushReact()
      returned <- session$getReturned()
      expect_identical(returned$history()$active_node_id, "node-1")
      expect_equal(nrow(transmission_history_table(returned$history())), 1L)

      session$setInputs(
        filter_name = "Neutral 50%",
        scale = "fraction",
        transmittance_type = "total",
        scattering = "no"
      )
      session$flushReact()
      session$setInputs(`apply-apply_filter` = 1)
      session$flushReact()
      expect_true(returned$can_promote())
      expect_true(returned$can_download())
      export_html <- output[["history-export_panel"]]$html
      expect_match(export_html, "Result plot (PNG)", fixed = TRUE)
      expect_match(export_html, "Transmittance spectrum (PNG)", fixed = TRUE)
      expect_match(export_html, "D65 table (PNG)", fixed = TRUE)
      expect_match(export_html, "D65 properties CSV", fixed = TRUE)
      expect_match(export_html, "Graph settings", fixed = TRUE)
      expect_match(export_html, "Build export bundle", fixed = TRUE)
      expect_match(export_html, "Plot plus table (PNG)", fixed = TRUE)
      expect_match(
        export_html,
        "Quickly download individual files",
        fixed = TRUE
      )
      expect_match(export_html, "history-export_plot_width", fixed = TRUE)
      expect_match(export_html, "history-export_font_size", fixed = TRUE)
      expect_match(
        export_html,
        "transmission-export-setting-row-secondary",
        fixed = TRUE
      )
      session$setInputs(
        `history-export_bundle_contents` = transmission_export_content_ids()
      )
      session$flushReact()
      bundle_html <- output[["history-export_bundle_control"]]$html
      expect_match(bundle_html, "Create selected ZIP bundle", fixed = TRUE)
      expect_match(bundle_html, "history-export_prepare_bundle", fixed = TRUE)

      selected_bundle_items <- transmission_export_content_ids()[-1L]
      session$setInputs(
        `history-export_bundle_contents` = selected_bundle_items
      )
      session$flushReact()
      expect_match(
        output[["history-export_bundle_summary"]]$html,
        "12 files selected",
        fixed = TRUE
      )

      session$setInputs(`history-export_bundle_contents` = character())
      session$flushReact()
      expect_match(
        output[["history-export_bundle_summary"]]$html,
        "0 files selected",
        fixed = TRUE
      )
      expect_match(
        output[["history-export_bundle_control"]]$html,
        'aria-disabled="true"',
        fixed = TRUE
      )

      session$setInputs(
        `history-export_bundle_contents` = "completed_csv",
        `history-export_prepare_bundle` = 1
      )
      session$flushReact()
      expect_match(
        output[["history-export_bundle_build_status"]]$html,
        "The ZIP bundle is ready",
        fixed = TRUE
      )
      expect_match(
        output[["history-export_bundle_control"]]$html,
        "Download prepared ZIP bundle",
        fixed = TRUE
      )

      session$setInputs(
        `history-promotion_name` = "First promoted result",
        `history-promote` = 1
      )
      session$flushReact()
      first_event <- returned$promotion_event()
      expect_s3_class(first_event, "transmission_activation_event")
      expect_identical(first_event$node_id, "node-2")
      expect_identical(first_event$parent_id, "node-1")
      expect_s3_class(
        returned$applied_snapshot(),
        "transmission_applied_snapshot"
      )
      expect_false(returned$can_promote())
      expect_match(
        output[["apply-apply_status"]]$html,
        "A valid promoted result is shown below",
        fixed = TRUE
      )
      expect_match(
        output[["apply-applied_outputs"]]$html,
        "Frozen promoted result",
        fixed = TRUE
      )
      expect_identical(returned$archive_node_id(), "node-2")
      expect_s3_class(
        returned$archived_snapshot(),
        "transmission_applied_snapshot"
      )
      expect_identical(
        returned$archived_snapshot()$incident_name,
        "CIE D65 at 250 lx"
      )
      expect_identical(returned$history()$active_node_id, "node-2")

      active(transmission_active_spectrum_from_event(first_event, 2L))
      session$flushReact()
      session$setInputs(`apply-apply_filter` = 2)
      session$flushReact()
      session$setInputs(
        `history-promotion_name` = "Sequential result",
        `history-promote` = 2
      )
      session$flushReact()
      second_event <- returned$promotion_event()
      expect_identical(second_event$node_id, "node-3")
      expect_identical(second_event$parent_id, "node-2")
      expect_identical(returned$archive_node_id(), "node-3")
      active(transmission_active_spectrum_from_event(second_event, 3L))
      session$flushReact()

      session$setInputs(
        `history-restore_node` = "node-1",
        `history-restore` = 1
      )
      session$flushReact()
      restore_event <- returned$restore_event()
      expect_s3_class(restore_event, "transmission_activation_event")
      expect_identical(restore_event$change_type, "restore")
      expect_identical(restore_event$node_id, "node-1")
      expect_length(returned$history()$nodes, 3L)
      expect_identical(returned$archive_node_id(), "node-3")
      expect_s3_class(
        returned$archived_snapshot(),
        "transmission_applied_snapshot"
      )
      active(transmission_active_spectrum_from_event(restore_event, 4L))
      session$flushReact()

      session$setInputs(`apply-apply_filter` = 3)
      session$flushReact()
      session$setInputs(
        `history-promotion_name` = "Alternative branch",
        `history-promote` = 3
      )
      session$flushReact()
      branch_event <- returned$promotion_event()
      expect_identical(branch_event$node_id, "node-4")
      expect_identical(branch_event$parent_id, "node-1")
      expect_identical(returned$archive_node_id(), "node-4")
      expect_setequal(
        names(returned$history()$nodes),
        paste0("node-", 1:4)
      )

      imported <- new_transmission_active_spectrum(
        transmission_source_fixture("equal_energy"),
        "Equal-energy source at 250 lx",
        "Development fixture",
        5L,
        "import",
        "node-1"
      )
      active(imported)
      session$flushReact()
      reset_history <- returned$history()
      expect_length(reset_history$nodes, 1L)
      expect_identical(reset_history$nodes[[1L]]$name, imported$name)
      expect_true(reset_history$nodes[[1L]]$active)
      expect_null(returned$applied_snapshot())
      expect_null(returned$archived_snapshot())
      expect_null(returned$archive_node_id())
      expect_null(returned$promotion_event())
      expect_null(returned$restore_event())
    }
  )
})

test_that("stale and invalid drafts cannot promote or download", {
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
      fixture_data = fixture,
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
      expect_true(returned$can_promote())

      session$setInputs(scale = "percent")
      session$flushReact()
      expect_true(returned$applied_stale())
      expect_false(returned$can_promote())
      expect_false(returned$can_download())
      session$setInputs(
        `history-promotion_name` = "Must not promote",
        `history-promote` = 1
      )
      session$flushReact()
      expect_null(returned$promotion_event())

      fixture(transmission_fixture("invalid"))
      session$flushReact()
      expect_false(returned$apply_ready())
      expect_false(returned$can_promote())
      expect_false(returned$can_download())
    }
  )
})

test_that("Milestone 3 UI exposes history and download contracts", {
  rendered <- as.character(transmissionUI("review"))

  expect_match(rendered, "review-history-promotion_name", fixed = TRUE)
  expect_match(rendered, "review-history-promote", fixed = TRUE)
  expect_match(rendered, "review-history-history_table", fixed = TRUE)
  expect_match(rendered, "review-history-archive_section", fixed = TRUE)
  expect_match(rendered, "review-history-export_panel", fixed = TRUE)
  expect_match(rendered, "Export transmission results", fixed = TRUE)
  expect_match(
    rendered,
    "History lasts for this Shiny session only",
    fixed = TRUE
  )
  expect_match(rendered, "data-update-on=\"blur\"", fixed = TRUE)
})

test_that("enabled downloads use native keyboard-operable buttons", {
  ns <- shiny::NS("download-test")
  enabled <- as.character(transmission_download_control(
    ns,
    "completed",
    "Completed filter CSV",
    enabled = TRUE
  ))
  disabled <- as.character(transmission_download_control(
    ns,
    "completed",
    "Completed filter CSV",
    enabled = FALSE
  ))

  expect_match(enabled, '<button type="button"', fixed = TRUE)
  expect_match(enabled, "download-test-completed", fixed = TRUE)
  expect_match(enabled, ".click(); return false;", fixed = TRUE)
  expect_match(enabled, "event.key === &#39;Enter&#39;", fixed = TRUE)
  expect_match(enabled, "event.key === &#39; &#39;", fixed = TRUE)
  expect_match(enabled, "event.preventDefault(); this.click();", fixed = TRUE)
  expect_match(enabled, "transmission-download-target", fixed = TRUE)
  expect_match(enabled, 'tabindex="-1"', fixed = TRUE)
  expect_false(grepl('disabled="disabled"', enabled, fixed = TRUE))

  expect_match(disabled, '<button type="button"', fixed = TRUE)
  expect_match(disabled, 'aria-disabled="true"', fixed = TRUE)
  expect_true(grepl("disabled", disabled, fixed = TRUE))
  expect_false(grepl("transmission-download-target", disabled, fixed = TRUE))
})

test_that("the primary export action spans its bundle container", {
  rendered <- as.character(transmissionUI("review"))

  expect_match(
    rendered,
    paste0(
      ".transmission-export-bundle ",
      ".transmission-download-wrapper"
    ),
    fixed = TRUE
  )
  expect_match(rendered, "display: flex; width: 100%;", fixed = TRUE)
})

test_that("desktop header and export layout preserve their visual contracts", {
  css_path <- system.file(
    "app",
    "www",
    "style.css",
    package = "Spectran"
  )
  expect_true(nzchar(css_path))
  css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")

  expect_match(css, "left: 110px", fixed = TRUE)
  expect_match(css, "padding-right: 40px", fixed = TRUE)
  expect_match(
    css,
    "grid-template-columns: repeat(3, minmax(0, 1fr));",
    fixed = TRUE
  )
  expect_match(
    css,
    "@media (max-width: 1023px) and (min-width: 768px)",
    fixed = TRUE
  )
  expect_match(
    css,
    "grid-template-columns: repeat(2, minmax(0, 1fr));",
    fixed = TRUE
  )
  expect_match(
    css,
    ".transmission-export-bundle > .shiny-input-checkboxgroup",
    fixed = TRUE
  )
  expect_match(css, "width: 100%;\n    max-width: none;", fixed = TRUE)
  expect_match(
    css,
    ".transmission-export-setting-row-secondary .control-label",
    fixed = TRUE
  )
})

test_that("source-import reset confirms and preserves state on cancel", {
  root <- new_transmission_active_spectrum(
    transmission_source_fixture("d65"),
    "CIE D65 at 250 lx",
    "Development fixture",
    1L,
    "import",
    "node-1"
  )
  completed <- tibble::tibble(
    wavelength_nm = 380:780,
    transmittance = rep(0.5, 401L),
    status = rep("supplied", 401L)
  )
  snapshot <- new_transmission_applied_snapshot(
    result = calculate_transmission_result(root$spectrum, completed),
    metadata = list(filter_name = "Neutral 50%"),
    incident_name = root$name,
    draft_revision = 1L,
    apply_sequence = 1L
  )
  history <- shiny::reactiveVal(
    transmission_history_promote(
      new_transmission_history(root),
      snapshot,
      "First promoted result"
    )$history
  )
  performed <- list()

  shiny::testServer(
    transmissionSourceImportServer,
    args = list(
      history = shiny::reactive(history()),
      perform_import = function(request) {
        performed[[length(performed) + 1L]] <<- request
      },
      return_focus_id = "import_source"
    ),
    {
      returned <- session$getReturned()
      initial_history <- history()
      request <- list(
        source_id = "equal_energy",
        source_name = "Equal-energy source at 250 lx"
      )

      returned$request(request, "click")
      session$flushReact()
      expect_length(performed, 0L)
      expect_identical(history(), initial_history)
      expect_identical(
        returned$pending()$request$source_name,
        request$source_name
      )
      expect_identical(returned$pending()$promoted_count, 1L)
      expect_match(
        returned$feedback()$message,
        "Confirmation required"
      )

      returned$cancel("keyboard")
      session$flushReact()
      expect_null(returned$pending())
      expect_length(performed, 0L)
      expect_identical(history(), initial_history)
      expect_match(returned$feedback()$message, "history were kept")

      returned$request(
        list(source_id = "d65", source_name = root$name),
        "click"
      )
      session$flushReact()
      expect_false(is.null(returned$pending()))
      expect_length(performed, 0L)

      returned$confirm("keyboard")
      session$flushReact()
      expect_null(returned$pending())
      expect_length(performed, 1L)
      expect_identical(performed[[1L]]$source_id, "d65")
      expect_match(returned$feedback()$message, "Removed 1 promoted node")

      returned$confirm("click")
      session$flushReact()
      expect_length(performed, 1L)
    }
  )
})

test_that("root-only source import proceeds without confirmation", {
  root <- new_transmission_active_spectrum(
    transmission_source_fixture("d65"),
    "CIE D65 at 250 lx",
    "Development fixture",
    1L,
    "import",
    "node-1"
  )
  history <- shiny::reactiveVal(new_transmission_history(root))
  performed <- list()

  shiny::testServer(
    transmissionSourceImportServer,
    args = list(
      history = shiny::reactive(history()),
      perform_import = function(request) {
        performed[[length(performed) + 1L]] <<- request
      },
      return_focus_id = "import_source"
    ),
    {
      returned <- session$getReturned()
      returned$request(list(
        source_id = "equal_energy",
        source_name = "Equal-energy source at 250 lx"
      ))
      session$flushReact()

      expect_length(performed, 1L)
      expect_null(returned$pending())
      expect_match(
        returned$feedback()$message,
        "new session-history root"
      )
    }
  )
})

test_that("source-import confirmation UI exposes explicit safe actions", {
  rendered <- as.character(transmissionSourceImportUI("review"))

  expect_match(rendered, "role=\"alertdialog\"", fixed = TRUE)
  expect_match(rendered, "review-cancel_import", fixed = TRUE)
  expect_match(rendered, "review-confirm_import", fixed = TRUE)
  expect_match(rendered, "Import and clear history", fixed = TRUE)
  expect_match(rendered, "Clear session history?", fixed = TRUE)
})
