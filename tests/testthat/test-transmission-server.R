test_that("module readiness combines curve and metadata requirements", {
  fixture <- shiny::reactive(transmission_fixture("neutral"))

  shiny::testServer(
    transmissionServer,
    args = list(fixture_data = fixture),
    {
      session$flushReact()
      session$setInputs(
        filter_name = "Neutral 50%",
        scale = "fraction",
        transmittance_type = "total",
        scattering = "no",
        measurement_geometry = "",
        measurement_angle = ""
      )
      session$flushReact()

      returned <- session$getReturned()
      expect_true(returned$ready())
      expect_equal(nrow(returned$preparation()$completed), 401)

      session$setInputs(scale = "")
      session$flushReact()
      expect_false(returned$ready())
      expect_match(
        paste(returned$diagnostics()$errors, collapse = " "),
        "Choose whether transmission"
      )
    }
  )
})

test_that("safe defaults disclose the passive-filter model scope", {
  fixture <- shiny::reactive(transmission_fixture("neutral"))

  shiny::testServer(
    transmissionServer,
    args = list(fixture_data = fixture),
    {
      session$flushReact()
      session$setInputs(filter_name = "Neutral 50%")
      session$flushReact()

      returned <- session$getReturned()
      expect_identical(returned$metadata()$scale, "fraction")
      expect_identical(returned$metadata()$transmittance_type, "total")
      expect_identical(
        returned$metadata()$filter_model_scope,
        "passive_non_fluorescent"
      )
      expect_match(
        returned$metadata()$filter_model_limitation,
        "re-emit it at different wavelengths",
        fixed = TRUE
      )
      expect_equal(nrow(returned$preparation()$completed), 401L)
      expect_true(returned$ready())
      expect_length(returned$diagnostics()$requirements, 0L)
    }
  )
})

test_that("module exposes independent tail decisions", {
  fixture <- shiny::reactive(transmission_fixture("partial"))

  shiny::testServer(
    transmissionServer,
    args = list(fixture_data = fixture),
    {
      session$flushReact()
      session$setInputs(
        filter_name = "Partial filter",
        scale = "fraction",
        transmittance_type = "total",
        scattering = "no",
        measurement_geometry = "",
        measurement_angle = ""
      )
      session$flushReact()
      returned <- session$getReturned()
      expect_false(returned$ready())

      session$setInputs(lower_tail = "zero", upper_tail = "one")
      session$flushReact()
      expect_true(returned$ready())
      expect_equal(
        returned$preparation()$completed$transmittance[c(1, 401)],
        c(0, 1)
      )
    }
  )
})

test_that("module exposes carry choices for both incomplete tails", {
  fixture <- shiny::reactive(transmission_fixture("partial"))

  shiny::testServer(
    transmissionServer,
    args = list(fixture_data = fixture),
    {
      session$flushReact()
      session$setInputs(
        filter_name = "Partial filter",
        lower_tail = "carry",
        upper_tail = "carry"
      )
      session$flushReact()
      returned <- session$getReturned()

      expect_true(returned$ready())
      expect_identical(
        returned$preparation()$completed$status[[1L]],
        "carried_lower_tail"
      )
      expect_identical(
        returned$preparation()$completed$status[[401L]],
        "carried_upper_tail"
      )
    }
  )
})

test_that("qualified and scattering transmittance require acknowledgements", {
  fixture <- shiny::reactive(transmission_fixture("neutral"))

  shiny::testServer(
    transmissionServer,
    args = list(fixture_data = fixture),
    {
      session$flushReact()
      session$setInputs(
        filter_name = "Geometry-specific filter",
        scale = "fraction",
        transmittance_type = "internal",
        scattering = "yes",
        measurement_geometry = "",
        measurement_angle = ""
      )
      session$flushReact()
      returned <- session$getReturned()
      unresolved <- paste(
        returned$diagnostics()$requirements,
        collapse = " "
      )
      expect_false(returned$ready())
      expect_match(unresolved, "whole-system total transmission")
      expect_match(unresolved, "measurement geometry")
      expect_match(unresolved, "geometry-specific")

      session$setInputs(
        type_ack = TRUE,
        measurement_geometry = "8 degree/diffuse integrating sphere",
        scattering_ack = TRUE
      )
      session$flushReact()
      expect_true(returned$ready())
    }
  )
})

test_that("a newly selected qualified type requires fresh acknowledgement", {
  fixture <- shiny::reactive(transmission_fixture("neutral"))

  shiny::testServer(
    transmissionServer,
    args = list(fixture_data = fixture),
    {
      session$flushReact()
      session$setInputs(
        filter_name = "Qualification reset filter",
        scale = "fraction",
        transmittance_type = "unknown",
        scattering = "no"
      )
      session$flushReact()
      returned <- session$getReturned()

      session$setInputs(type_ack = TRUE)
      session$flushReact()
      expect_true(returned$metadata()$qualified_type_acknowledged)
      expect_true(returned$ready())

      session$setInputs(transmittance_type = "total")
      session$flushReact()
      expect_false(returned$metadata()$qualified_type_acknowledged)
      expect_true(returned$ready())

      session$setInputs(transmittance_type = "unknown")
      session$flushReact()
      expect_false(returned$metadata()$qualified_type_acknowledged)
      expect_false(returned$ready())

      session$setInputs(type_ack = TRUE)
      session$flushReact()
      expect_true(returned$metadata()$qualified_type_acknowledged)
      expect_true(returned$ready())

      session$setInputs(transmittance_type = "internal")
      session$flushReact()
      expect_false(returned$metadata()$qualified_type_acknowledged)
      expect_false(returned$ready())
    }
  )
})

test_that("a new filter cannot inherit prior metadata or gap consent", {
  fixture <- shiny::reactiveVal(transmission_fixture("large_gap"))

  shiny::testServer(
    transmissionServer,
    args = list(fixture_data = fixture),
    {
      session$flushReact()
      session$setInputs(
        filter_name = "First gap filter",
        scale = "fraction",
        transmittance_type = "internal",
        scattering = "yes",
        measurement_geometry = "8\u00b0/diffuse sphere",
        measurement_angle = "8\u00b0"
      )
      session$flushReact()
      session$setInputs(
        type_ack = TRUE,
        scattering_ack = TRUE,
        large_gap_ack = TRUE
      )
      session$flushReact()
      returned <- session$getReturned()
      expect_true(returned$ready())
      expect_true(returned$preparation()$diagnostics$acknowledge_large_gaps)

      next_filter <- tibble::tibble(
        source_row = 1:4,
        wavelength_nm = c(360, 380, 780, 800),
        value = rep(50, 4)
      )
      attr(next_filter, "filter_name") <- "Second filter"
      fixture(next_filter)
      session$flushReact()

      expect_false(returned$ready())
      expect_identical(returned$metadata()$scale, "fraction")
      expect_identical(returned$metadata()$transmittance_type, "total")
      expect_identical(
        returned$metadata()$filter_model_scope,
        "passive_non_fluorescent"
      )
      expect_identical(returned$metadata()$scattering, "no")

      session$setInputs(
        filter_name = "Second filter",
        scale = "percent",
        transmittance_type = "total",
        scattering = "no"
      )
      session$flushReact()
      expect_false(returned$ready())
      expect_false(returned$preparation()$diagnostics$acknowledge_large_gaps)
      expect_equal(nrow(returned$preparation()$diagnostics$large_gaps), 1L)
    }
  )
})

test_that("parser changes invalidate tail choices but retain reviewed metadata", {
  path <- tempfile(fileext = ".txt")
  writeLines(
    paste(seq(380, 780, by = 5), 0.5),
    con = path
  )

  shiny::testServer(
    transmissionServer,
    {
      session$flushReact()
      session$setInputs(
        `csv-skip` = 0,
        `csv-delimiter` = "",
        `csv-decimal_mark` = ".",
        `csv-wavelength_column` = 1,
        `csv-value_column` = 2,
        `csv-header` = FALSE,
        filter_file = list(
          datapath = path,
          name = "dense-no-header.txt",
          size = file.info(path)$size,
          type = "text/plain"
        )
      )
      session$flushReact()
      session$setInputs(
        filter_name = "Dense filter",
        scale = "fraction",
        transmittance_type = "total",
        scattering = "no"
      )
      session$flushReact()
      returned <- session$getReturned()
      expect_true(returned$ready())

      session$setInputs(lower_tail = "zero")
      session$flushReact()
      session$setInputs(`csv-header` = TRUE)
      session$flushReact()

      expect_false(returned$ready())
      expect_identical(returned$preparation()$diagnostics$lower_tail, NULL)
      expect_match(
        paste(returned$diagnostics()$requirements, collapse = " "),
        "lower tail"
      )
      expect_identical(returned$metadata()$scale, "fraction")
      expect_identical(returned$metadata()$transmittance_type, "total")
      expect_identical(
        returned$metadata()$filter_model_scope,
        "passive_non_fluorescent"
      )
    }
  )
})

test_that("readiness counts actions separately from disclosure notes", {
  fixture_data <- tibble::tibble(
    source_row = 1:4,
    wavelength_nm = c(800, 780, 380, 360),
    value = rep(50, 4)
  )
  attr(fixture_data, "filter_name") <- "Sorted boundary filter"
  fixture <- shiny::reactive(fixture_data)

  shiny::testServer(
    transmissionServer,
    args = list(fixture_data = fixture),
    {
      session$flushReact()
      session$setInputs(
        filter_name = "Sorted boundary filter",
        scale = "percent",
        transmittance_type = "total",
        scattering = "no"
      )
      session$flushReact()

      unresolved <- output$readiness$html
      expect_match(
        unresolved,
        "Not ready: 1 action required; 3 notes.",
        fixed = TRUE
      )
      expect_match(unresolved, "Actions required", fixed = TRUE)
      expect_match(unresolved, "Notes (3)", fixed = TRUE)
      expect_lt(
        regexpr("Actions required", unresolved, fixed = TRUE),
        regexpr("Notes (3)", unresolved, fixed = TRUE)
      )

      session$setInputs(large_gap_ack = TRUE)
      session$flushReact()
      resolved <- output$readiness$html
      expect_match(
        resolved,
        "Ready: the completed curve has 401 calculation rows.",
        fixed = TRUE
      )
      expect_match(resolved, "Notes (3)", fixed = TRUE)
      expect_false(grepl("Actions required", resolved, fixed = TRUE))
    }
  )
})

test_that("production module UI exposes responsive and early-feedback hooks", {
  rendered <- as.character(transmissionUI("review"))

  expect_match(rendered, "transmission-readiness", fixed = TRUE)
  expect_match(rendered, "col-lg-5 transmission-form-column", fixed = TRUE)
  expect_match(rendered, "transmission-table-scroll", fixed = TRUE)
  expect_match(rendered, "overflow-x: auto", fixed = TRUE)
  expect_match(rendered, "100% flat-transmittance CSV template", fixed = TRUE)
  expect_match(rendered, "transmission-template-download", fixed = TRUE)
  expect_match(rendered, "@media (max-width: 479px)", fixed = TRUE)
  expect_match(rendered, "display: flex; width: 100%", fixed = TRUE)
  expect_match(rendered, "Point (example: 0.5)", fixed = TRUE)
  expect_match(rendered, "Receiving file", fixed = TRUE)
  expect_match(rendered, "File received", fixed = TRUE)
  expect_match(rendered, 'value="fraction" selected', fixed = TRUE)
  expect_match(rendered, 'value="total" selected', fixed = TRUE)
  expect_false(grepl("passive_ack", rendered, fixed = TRUE))
  expect_false(grepl("transmission-inline-help", rendered, fixed = TRUE))
  expect_match(rendered, "transmission-info-tooltip", fixed = TRUE)
  expect_match(rendered, 'role="tooltip"', fixed = TRUE)
  expect_match(rendered, "top: 100%", fixed = TRUE)
  expect_match(
    rendered,
    ".transmission-info-tooltip.is-dismissed",
    fixed = TRUE
  )
  tooltip_specs <- transmission_tooltip_specs()
  for (tooltip_key in names(tooltip_specs)) {
    expect_match(
      rendered,
      paste0('id="review-', tooltip_key, '_info"'),
      fixed = TRUE
    )
    expect_match(
      rendered,
      paste0(
        'aria-describedby="review-',
        tooltip_key,
        '_info_content"'
      ),
      fixed = TRUE
    )
    expect_match(
      rendered,
      paste0('aria-label="', tooltip_specs[[tooltip_key]]$trigger_label, '"'),
      fixed = TRUE
    )
  }
  tooltip_occurrences <- gregexpr(
    'class="transmission-info-tooltip"',
    rendered,
    fixed = TRUE
  )[[1]]
  expect_length(tooltip_occurrences, length(tooltip_specs))

  expect_match(rendered, 'for="review-filter_file"', fixed = TRUE)
  expect_match(rendered, 'for="review-filter_name"', fixed = TRUE)
  expect_match(rendered, 'for="review-scale"', fixed = TRUE)
  expect_match(rendered, 'for="review-transmittance_type"', fixed = TRUE)
  expect_match(rendered, 'for="review-scattering"', fixed = TRUE)
  expect_match(rendered, 'for="review-measurement_geometry"', fixed = TRUE)
  expect_match(rendered, 'for="review-measurement_angle"', fixed = TRUE)
  expect_match(rendered, "About transmission scale", fixed = TRUE)
  expect_match(
    rendered,
    "About transmittance type and filter model",
    fixed = TRUE
  )
  expect_match(rendered, "About measurement geometry", fixed = TRUE)
  expect_match(rendered, "About scattering and diffusing samples", fixed = TRUE)
  expect_match(
    rendered,
    paste(
      "A fluorescent material can absorb light and re-emit it at different",
      "wavelengths"
    ),
    fixed = TRUE
  )
  expect_false(grepl(
    "Why this confirmation is required",
    rendered,
    fixed = TRUE
  ))
  expect_identical(
    names(tooltip_specs),
    c(
      "filter_file",
      "filter_name",
      "scale",
      "transmittance_type",
      "scattering",
      "measurement_geometry",
      "measurement_angle"
    )
  )
  expect_match(rendered, "Flexible CSV settings", fixed = TRUE)
  expect_match(rendered, "<details", fixed = TRUE)
  expect_match(
    transmission_passive_filter_limitation(),
    "absorb light and re-emit it at different wavelengths",
    fixed = TRUE
  )

  readiness_position <- regexpr("review-readiness", rendered, fixed = TRUE)
  form_position <- regexpr("Transmission spectrum", rendered, fixed = TRUE)
  expect_lt(as.integer(readiness_position), as.integer(form_position))
})

test_that("Apply is gated by both filter readiness and an active source", {
  fixture <- shiny::reactive(transmission_fixture("neutral"))

  shiny::testServer(
    transmissionServer,
    args = list(fixture_data = fixture),
    {
      session$flushReact()
      session$setInputs(
        filter_name = "Neutral filter",
        scale = "fraction",
        transmittance_type = "total",
        scattering = "no"
      )
      session$flushReact()

      returned <- session$getReturned()
      expect_true(returned$ready())
      expect_false(returned$apply_ready())
      expect_null(returned$applied_snapshot())

      session$setInputs(`apply-apply_filter` = 1)
      session$flushReact()
      expect_null(returned$applied_snapshot())
      expect_equal(returned$apply_sequence(), 0L)
    }
  )
})

test_that("Apply freezes metrics and calculation edits make them stale", {
  fixture <- shiny::reactiveVal(transmission_fixture("neutral"))
  source <- shiny::reactiveVal(transmission_source_fixture("d65"))
  source_name <- shiny::reactiveVal("CIE D65 at 250 lx")

  shiny::testServer(
    transmissionServer,
    args = list(
      fixture_data = fixture,
      incident_spectrum = source,
      incident_name = source_name
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
      returned <- session$getReturned()
      expect_true(returned$apply_ready())

      applied_revision <- returned$draft_revision()
      session$setInputs(`apply-apply_filter` = 1)
      session$flushReact()

      snapshot <- returned$applied_snapshot()
      expect_s3_class(snapshot, "transmission_applied_snapshot")
      expect_equal(snapshot$draft_revision, applied_revision)
      expect_equal(snapshot$incident_name, "CIE D65 at 250 lx")
      expect_equal(nrow(snapshot$metrics), 28L)
      expect_equal(snapshot$d65_properties$transmitted_value, rep(0.5, 6L))
      expect_false(returned$applied_stale())
      expect_true(returned$can_promote())
      expect_true(returned$can_download())

      session$setInputs(filter_name = "User-edited promotion name")
      session$flushReact()
      expect_false(returned$applied_stale())
      expect_true(returned$can_promote())
      expect_equal(
        returned$applied_snapshot()$metadata$filter_name,
        "Neutral 50%"
      )

      session$setInputs(scale = "percent")
      session$flushReact()
      expect_gt(returned$draft_revision(), applied_revision)
      expect_true(returned$applied_stale())
      expect_false(returned$can_promote())
      expect_false(returned$can_download())
      expect_equal(
        returned$applied_snapshot()$d65_properties$transmitted_value,
        rep(0.5, 6L)
      )

      session$setInputs(`apply-apply_filter` = 2)
      session$flushReact()
      expect_false(returned$applied_stale())
      expect_equal(returned$apply_sequence(), 2L)
      expect_equal(
        returned$applied_snapshot()$d65_properties$transmitted_value,
        rep(0.005, 6L)
      )

      source(transmission_source_fixture("equal_energy"))
      source_name("Equal-energy source at 250 lx")
      session$flushReact()
      expect_true(returned$applied_stale())
      expect_false(returned$can_promote())

      session$setInputs(`apply-apply_filter` = 3)
      session$flushReact()
      expect_false(returned$applied_stale())
      expect_equal(returned$apply_sequence(), 3L)
      expect_equal(
        returned$applied_snapshot()$incident_name,
        "Equal-energy source at 250 lx"
      )
    }
  )
})

test_that("zero source applies with explicit undefined denominator warnings", {
  fixture <- shiny::reactive(transmission_fixture("neutral"))
  source <- shiny::reactive(transmission_source_fixture("zero"))
  source_name <- shiny::reactive("Zero spectrum")

  shiny::testServer(
    transmissionServer,
    args = list(
      fixture_data = fixture,
      incident_spectrum = source,
      incident_name = source_name
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
      returned <- session$getReturned()
      expect_true(returned$apply_ready())

      session$setInputs(`apply-apply_filter` = 1)
      session$flushReact()
      snapshot <- returned$applied_snapshot()
      retained <- snapshot$active_metrics$comparison_type == "retained"

      expect_false(any(snapshot$active_metrics$comparison_defined[retained]))
      expect_true(any(grepl("machine precision", snapshot$warnings)))
      expect_false(returned$applied_stale())
      expect_true(returned$can_download())
    }
  )
})

test_that("Milestone 2 UI exposes Apply, comparison, and stale-state hooks", {
  rendered <- as.character(transmissionUI("review"))

  expect_match(rendered, "review-apply-apply_controls", fixed = TRUE)
  expect_match(rendered, "transmission-applied-results", fixed = TRUE)
  expect_match(rendered, "is-stale", fixed = TRUE)
  expect_match(rendered, "action-button:focus-visible", fixed = TRUE)
  expect_match(rendered, "outline: 3px solid #005fcc", fixed = TRUE)
  expect_match(rendered, "review-apply-applied_outputs", fixed = TRUE)
})

test_that("production Apply status uses state guidance without audit counters", {
  snapshot <- list(incident_name = "Test source", draft_revision = 19L)

  ready <- transmission_apply_status_state(NULL, FALSE, TRUE)
  current <- transmission_apply_status_state(snapshot, FALSE, TRUE)
  stale_valid <- transmission_apply_status_state(snapshot, TRUE, TRUE)
  stale_invalid <- transmission_apply_status_state(snapshot, TRUE, FALSE)
  archived <- transmission_apply_status_state(
    snapshot,
    TRUE,
    TRUE,
    archived = TRUE,
    active_source_name = "Promoted source"
  )

  expect_identical(ready$message, "Ready to apply the current filter.")
  expect_identical(current$message, "Results are current for Test source.")
  expect_identical(
    stale_valid$message,
    "Results are out of date. Apply the current filter again."
  )
  expect_identical(stale_valid$state_class, "is-stale stale-valid")
  expect_identical(
    stale_invalid$message,
    paste(
      "Results are out of date.",
      "Resolve the current readiness actions before applying again."
    )
  )
  expect_identical(stale_invalid$state_class, "is-stale stale-invalid")
  expect_identical(archived$state_class, "is-archived")
  expect_identical(
    archived$message,
    paste(
      "A valid promoted result is shown below.",
      "Apply again to create a separate sequential calculation using",
      "Promoted source as the active source."
    )
  )
  expect_false(any(grepl(
    "revision",
    c(
      ready$message,
      current$message,
      stale_valid$message,
      stale_invalid$message,
      archived$message
    ),
    ignore.case = TRUE
  )))
})

test_that("keyboard activation recognition covers Enter and Space once", {
  expect_true(is_transmission_activation_key(list(key = "Enter")))
  expect_true(is_transmission_activation_key(list(key = " ")))
  expect_true(is_transmission_activation_key(list(key = "Spacebar")))
  expect_true(is_transmission_activation_key(list(code = "Space")))
  expect_true(is_transmission_activation_key(list(which = 13L)))
  expect_true(is_transmission_activation_key(list(which = 32L)))
  expect_false(is_transmission_activation_key(list(key = "Escape")))
  expect_false(is_transmission_activation_key(list(
    key = "Enter",
    "repeat" = TRUE
  )))
  expect_false(is_transmission_activation_key(NULL))

  expect_true(is_transmission_escape_key(list(key = "Escape")))
  expect_true(is_transmission_escape_key(list(code = "Escape")))
  expect_true(is_transmission_escape_key(list(which = 27L)))
  expect_false(is_transmission_escape_key(list(key = "Enter")))
  expect_false(is_transmission_escape_key(list(
    key = "Escape",
    "repeat" = TRUE
  )))
  expect_false(is_transmission_escape_key(NULL))

  expect_true(is_transmission_apply_key(list(key = "Enter")))
  expect_true(is_transmission_apply_key(list(key = " ")))
  expect_true(is_transmission_apply_key(list(key = "Spacebar")))
  expect_true(is_transmission_apply_key(list(code = "Space")))
  expect_true(is_transmission_apply_key(list(which = 13L)))
  expect_true(is_transmission_apply_key(list(which = 32L)))
  expect_false(is_transmission_apply_key(list(key = "Escape")))
  expect_false(is_transmission_apply_key(list(key = "Enter", "repeat" = TRUE)))
  expect_false(is_transmission_apply_key(NULL))
})

test_that("undefined metric guidance is concise, grouped, and collapsed", {
  completed <- function(value) {
    tibble::tibble(
      wavelength_nm = 380:780,
      transmittance = rep(value, 401L),
      status = rep("supplied", 401L)
    )
  }
  applied <- function(source, value) {
    new_transmission_applied_snapshot(
      result = calculate_transmission_result(source, completed(value)),
      metadata = list(filter_name = "Test filter"),
      incident_name = "Test source",
      draft_revision = 1L,
      apply_sequence = 1L
    )
  }

  zero <- transmission_metric_warning_presentation(applied(
    transmission_source_fixture("zero"),
    0.5
  ))
  expect_identical(
    zero$summary,
    paste(
      "This source has zero irradiance and illuminance.",
      paste(
        "Retained percentages, action factors, and DERs cannot be calculated;",
        "affected cells are shown as Undefined."
      )
    )
  )
  expect_identical(zero$affected_metrics, 22L)
  expect_identical(zero$detail_messages, 17L)
  expect_identical(
    vapply(zero$details, function(group) length(group$messages), integer(1)),
    c(7L, 5L, 5L)
  )
  expect_identical(
    vapply(zero$details, `[[`, character(1), "label"),
    c(
      "Retained proportions",
      "Action factors",
      "Daylight efficacy ratios"
    )
  )

  zero_ui <- as.character(transmission_metric_warning_ui(zero))
  expect_match(zero_ui, "Why some values are Undefined", fixed = TRUE)
  expect_match(
    zero_ui,
    "Technical denominator details (17 explanations)",
    fixed = TRUE
  )
  expect_match(
    zero_ui,
    "Retained proportions (7 explanations)",
    fixed = TRUE
  )
  expect_match(zero_ui, "Action factors (5 explanations)", fixed = TRUE)
  expect_match(
    zero_ui,
    "Daylight efficacy ratios (5 explanations)",
    fixed = TRUE
  )
  expect_match(zero_ui, "<details", fixed = TRUE)
  expect_false(grepl("<details[^>]*\\sopen(?:=|\\s|>)", zero_ui))

  opaque <- transmission_metric_warning_presentation(applied(
    transmission_source_fixture("d65"),
    0
  ))
  expect_match(
    opaque$summary,
    "transmitted light has zero photopic illuminance",
    ignore.case = TRUE
  )

  tiny_source <- transmission_source_fixture("zero")
  tiny_source$Bestrahlungsstaerke <- rep(
    .Machine$double.eps / 1e12,
    401L
  )
  tiny <- transmission_metric_warning_presentation(applied(tiny_source, 0.5))
  expect_match(tiny$summary, "machine precision", fixed = TRUE)

  ordinary <- transmission_metric_warning_presentation(applied(
    transmission_source_fixture("d65"),
    0.5
  ))
  expect_length(ordinary$summary, 0L)
  expect_null(transmission_metric_warning_ui(ordinary))
})

test_that("transmission plots use exact physical zero baselines", {
  zero_spectrum_plot <- ggplot2::ggplot(
    data.frame(wavelength_nm = c(380, 780), irradiance = c(0, 0)),
    ggplot2::aes(x = .data$wavelength_nm, y = .data$irradiance)
  ) +
    ggplot2::geom_line() +
    transmission_irradiance_y_scale()
  positive_spectrum_plot <- ggplot2::ggplot(
    data.frame(wavelength_nm = c(380, 780), irradiance = c(0.5, 2)),
    ggplot2::aes(x = .data$wavelength_nm, y = .data$irradiance)
  ) +
    ggplot2::geom_line() +
    transmission_irradiance_y_scale()
  construction_plot <- ggplot2::ggplot(
    data.frame(wavelength_nm = c(380, 780), transmittance = c(0.5, 0.5)),
    ggplot2::aes(x = .data$wavelength_nm, y = .data$transmittance)
  ) +
    ggplot2::geom_line() +
    transmission_fraction_y_scale()

  zero_range <- ggplot2::ggplot_build(zero_spectrum_plot)$layout$panel_params[[
    1L
  ]]$y.range
  positive_range <- ggplot2::ggplot_build(
    positive_spectrum_plot
  )$layout$panel_params[[1L]]$y.range
  construction_range <- ggplot2::ggplot_build(
    construction_plot
  )$layout$panel_params[[1L]]$y.range

  expect_equal(zero_range[[1L]], 0, tolerance = 0)
  expect_gt(zero_range[[2L]], 0)
  expect_equal(positive_range[[1L]], 0, tolerance = 0)
  expect_equal(construction_range, c(0, 1), tolerance = 0)
})
