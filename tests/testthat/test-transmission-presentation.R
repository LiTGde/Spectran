transmission_presentation_snapshot <- function(source = "d65") {
  preparation <- prepare_transmission_curve(
    transmission_fixture("neutral"),
    "fraction"
  )
  new_transmission_applied_snapshot(
    result = calculate_transmission_result(
      transmission_source_fixture(source),
      preparation$completed
    ),
    metadata = list(filter_name = "Neutral 50%"),
    incident_name = if (identical(source, "zero")) "Zero spectrum" else "D65",
    draft_revision = 1L,
    apply_sequence = 1L
  )
}

test_that("gt presentation preserves immutable result data", {
  old_language <- the$language
  withr::defer(the$language <- old_language)
  the$language <- "English"

  snapshot <- transmission_presentation_snapshot()
  original <- snapshot
  tables <- list(
    transmission_d65_gt(snapshot),
    transmission_absolute_gt(snapshot),
    transmission_balance_gt(snapshot)
  )

  expect_true(all(vapply(tables, inherits, logical(1), what = "gt_tbl")))
  expect_identical(snapshot, original)

  html <- vapply(tables, gt::as_raw_html, character(1))
  expect_match(html[[1L]], "D65-referenced filter properties", fixed = TRUE)
  expect_match(html[[1L]], "Transmittance", fixed = TRUE)
  expect_match(html[[1L]], "τ<sub>v,D65</sub>", fixed = TRUE)
  expect_match(html[[2L]], "Light values", fixed = TRUE)
  expect_match(html[[2L]], "E<sub>e</sub>", fixed = TRUE)
  expect_match(html[[2L]], "E<sub>v,mel,D65</sub>", fixed = TRUE)
  expect_match(
    html[[3L]],
    "Action factors and daylight efficacy ratios",
    fixed = TRUE
  )
  expect_match(html[[3L]], "Change", fixed = TRUE)
  expect_match(html[[3L]], "γ<sub>mel,v,D65</sub>", fixed = TRUE)

  rendered <- transmission_gt_html(tables[[1L]])
  expect_s3_class(rendered, "html")
  expect_match(as.character(rendered), "gt_table", fixed = TRUE)
  expect_identical(snapshot, original)
})

test_that("visible scientific symbols use subscripts without changing data", {
  snapshot <- transmission_presentation_snapshot()
  original_symbol <- snapshot$d65_properties$symbol[[1L]]

  expect_identical(
    transmission_symbol_html(c("τv,D65", "Ee", "γmel,v,D65")),
    c(
      "τ<sub>v,D65</sub>",
      "E<sub>e</sub>",
      "γ<sub>mel,v,D65</sub>"
    )
  )
  expect_identical(snapshot$d65_properties$symbol[[1L]], original_symbol)
})

test_that("gt result metrics use exactly three decimal places", {
  table <- data.frame(value = c(0.2998, 1.2349, 2)) |>
    gt::gt() |>
    transmission_gt_format_numbers(columns = "value")
  visible <- gt::extract_cells(table, columns = "value", output = "plain")

  expect_identical(visible, c("0.300", "1.235", "2.000"))

  old_language <- the$language
  withr::defer(the$language <- old_language)
  the$language <- "Deutsch"
  german_table <- data.frame(value = c(0.1, 0.2998, 2)) |>
    gt::gt() |>
    transmission_gt_format_numbers(columns = "value")
  german_visible <- gt::extract_cells(
    german_table,
    columns = "value",
    output = "plain"
  )

  expect_identical(german_visible, c("0,100", "0,300", "2,000"))
})

test_that("source-data previews can retain compact numeric formatting", {
  table <- data.frame(value = c(0.2998, 1.2349, 2)) |>
    gt::gt() |>
    transmission_gt_format_numbers(
      columns = "value",
      trim_trailing_zeros = TRUE
    )
  visible <- gt::extract_cells(table, columns = "value", output = "plain")

  expect_identical(visible, c("0.3", "1.235", "2"))
})

test_that("display-only gt outputs register only a Shiny output binding", {
  rendered <- as.character(transmission_gt_output("metric-table"))

  expect_match(rendered, 'id="metric-table"', fixed = TRUE)
  expect_match(rendered, "shiny-html-output", fixed = TRUE)
  expect_false(grepl("shiny-input", rendered, fixed = TRUE))
})

test_that("gt tables retain explicit Undefined values", {
  old_language <- the$language
  withr::defer(the$language <- old_language)
  the$language <- "English"

  snapshot <- transmission_presentation_snapshot("zero")
  absolute_html <- gt::as_raw_html(transmission_absolute_gt(snapshot))
  balance_html <- gt::as_raw_html(transmission_balance_gt(snapshot))

  expect_match(absolute_html, "Undefined", fixed = TRUE)
  expect_match(balance_html, "Undefined", fixed = TRUE)
})

test_that("preview gt tables expose readable labels", {
  old_language <- the$language
  withr::defer(the$language <- old_language)
  the$language <- "English"

  preparation <- prepare_transmission_curve(
    transmission_fixture("neutral"),
    "fraction"
  )
  input_html <- gt::as_raw_html(transmission_input_preview_gt(preparation))
  status_html <- gt::as_raw_html(transmission_status_summary_gt(preparation))

  expect_match(input_html, "Wavelength (nm)", fixed = TRUE)
  expect_match(input_html, "Scaled transmittance (fraction)", fixed = TRUE)
  expect_match(status_html, "Supplied measurement", fixed = TRUE)
})

test_that("history gt table identifies the active branch", {
  snapshot <- transmission_presentation_snapshot()
  root <- new_transmission_active_spectrum(
    snapshot$incident_spectrum,
    snapshot$incident_name,
    "Test source",
    1L,
    "import",
    "node-1"
  )
  promoted <- transmission_history_promote(
    new_transmission_history(root),
    snapshot,
    "Neutral branch"
  )$history
  html <- gt::as_raw_html(transmission_history_gt(promoted))

  expect_match(html, "Session history tree", fixed = TRUE)
  expect_match(html, "Neutral branch", fixed = TRUE)
  expect_match(html, "#FFF8BE", fixed = TRUE)
})

test_that("result plot optionally adds an integrated transmittance panel", {
  old_language <- the$language
  withr::defer(the$language <- old_language)
  the$language <- "English"

  snapshot <- transmission_presentation_snapshot()
  single <- transmission_spectral_comparison_plot(snapshot)
  selected <- transmission_spectral_comparison_plot(
    snapshot,
    response_curves = c("melanopic", "photopic")
  )
  combined <- transmission_spectral_comparison_plot(
    snapshot,
    show_transmittance_panel = TRUE
  )
  panel <- transmission_filter_panel_plot(snapshot)
  compact_panel <- transmission_filter_panel_plot(
    snapshot,
    compact_x = TRUE
  )
  single_build <- ggplot2::ggplot_build(single)
  selected_build <- ggplot2::ggplot_build(selected)
  panel_build <- ggplot2::ggplot_build(panel)
  compact_panel_build <- ggplot2::ggplot_build(compact_panel)

  expect_s3_class(single, "ggplot")
  expect_s3_class(combined, "patchwork")
  expect_identical(single$labels$title, "D65 × Neutral 50%")
  expect_identical(
    single$labels$subtitle,
    "Transmitted (solid) and incident (dashed)"
  )
  expect_s3_class(single$layers[[1L]]$geom, "GeomRidgelineGradient")
  expect_s3_class(single$layers[[2L]]$geom, "GeomRidgeline")
  expect_identical(single$layers[[2L]]$aes_params$fill, "white")
  expect_identical(single$layers[[2L]]$aes_params$alpha, 0.85)
  expect_s3_class(single$layers[[3L]]$geom, "GeomRidgelineGradient")
  expect_identical(single$layers[[4L]]$aes_params$linetype, "22")
  expect_identical(single$layers[[5L]]$aes_params$linetype, "solid")
  no_incident_fill <- transmission_spectral_comparison_plot(
    snapshot,
    incident_fill = FALSE
  )
  expect_length(no_incident_fill$layers, 3L)
  expect_s3_class(no_incident_fill$layers[[1L]]$geom, "GeomRidgelineGradient")
  expect_identical(no_incident_fill$layers[[2L]]$aes_params$linetype, "22")
  expect_identical(no_incident_fill$layers[[3L]]$aes_params$linetype, "solid")
  expect_identical(transmission_result_panel_layout(699), "stack")
  expect_identical(transmission_result_panel_layout(700), "side")
  expect_identical(transmission_result_panel_layout(NULL), "side")
  expect_identical(transmission_result_title_wrap_width(240), 20L)
  expect_identical(transmission_result_title_wrap_width(320), 20L)
  expect_identical(transmission_result_title_wrap_width(349), 20L)
  expect_identical(transmission_result_title_wrap_width(350), 28L)
  expect_identical(transmission_result_title_wrap_width(390), 28L)
  expect_identical(transmission_result_title_wrap_width(479), 28L)
  expect_identical(transmission_result_title_wrap_width(480), 40L)
  expect_identical(transmission_result_title_wrap_width(699), 40L)
  expect_identical(transmission_result_title_wrap_width(700), 56L)
  expect_identical(transmission_result_title_wrap_width(NULL), 56L)
  expect_identical(transmission_result_title_word_wrap_width(240), 10L)
  expect_identical(transmission_result_title_word_wrap_width(259), 10L)
  expect_null(transmission_result_title_word_wrap_width(260))
  expect_null(transmission_result_title_word_wrap_width(NULL))
  expect_identical(transmission_archived_result_title_wrap_width(232), 16L)
  expect_identical(transmission_archived_result_title_wrap_width(259), 16L)
  expect_identical(transmission_archived_result_title_wrap_width(260), 20L)
  expect_identical(transmission_archived_result_title_right_margin(232), 16)
  expect_identical(transmission_archived_result_title_right_margin(259), 16)
  expect_identical(transmission_archived_result_title_right_margin(260), 8)
  expect_identical(
    transmission_result_title_subtitle_gap(
      "Norm-\nTageslicht\nspektrum 6500K\n× IPLUS",
      adaptive = TRUE
    ),
    12
  )
  expect_identical(
    transmission_result_title_subtitle_gap(
      "CIE Standard\nIlluminant D\n6500K × IPLUS",
      adaptive = TRUE
    ),
    12
  )
  expect_identical(
    transmission_result_title_subtitle_gap(
      "CIE D65\n× IPLUS",
      adaptive = TRUE
    ),
    0
  )
  expect_identical(
    transmission_result_title_subtitle_gap(
      "D65 × IPLUS",
      adaptive = TRUE
    ),
    0
  )
  expect_identical(
    transmission_result_title_subtitle_gap(
      "Norm-\nTageslicht\nspektrum 6500K\n× IPLUS",
      adaptive = FALSE
    ),
    0
  )
  expect_gt(length(selected_build$data), length(single_build$data))
  expect_true(any(vapply(
    selected_build$data,
    function(layer) {
      "label" %in% names(layer) && any(layer$label == "V(λ)")
    },
    logical(1)
  )))
  expect_lt(single_build$layout$panel_params[[1L]]$x.range[[1L]], 380)
  expect_gt(single_build$layout$panel_params[[1L]]$x.range[[2L]], 780)
  expect_equal(
    single_build$layout$panel_scales_x[[1L]]$get_breaks(),
    c(400, 500, 600, 700, 780)
  )
  expect_equal(
    panel_build$layout$panel_scales_y[[1L]]$get_limits(),
    c(0, 1)
  )
  expect_equal(
    compact_panel_build$layout$panel_scales_x[[1L]]$get_breaks(),
    c(400, 500, 600, 700, 780)
  )
})

test_that("result plot keeps long titles complete at narrow app widths", {
  snapshot <- transmission_presentation_snapshot()
  snapshot$incident_name <- "CIE Standard Illuminant D 6500K"
  snapshot$metadata$filter_name <- "IPLUS"
  narrow <- transmission_spectral_comparison_plot(
    snapshot,
    title_wrap_width = transmission_result_title_wrap_width(240)
  )
  german_snapshot <- snapshot
  german_snapshot$incident_name <- "Norm-Tageslichtspektrum 6500K"
  german_narrow <- transmission_spectral_comparison_plot(
    german_snapshot,
    title_wrap_width = transmission_result_title_wrap_width(240)
  )
  german_archived_narrow <- transmission_spectral_comparison_plot(
    german_snapshot,
    title_wrap_width = transmission_archived_result_title_wrap_width(232),
    title_word_wrap_width = transmission_result_title_word_wrap_width(232),
    plot_margin_right = transmission_archived_result_title_right_margin(232),
    adaptive_title_spacing = TRUE
  )
  english_archived_narrow <- transmission_spectral_comparison_plot(
    snapshot,
    title_wrap_width = transmission_archived_result_title_wrap_width(232),
    title_word_wrap_width = transmission_result_title_word_wrap_width(232),
    plot_margin_right = transmission_archived_result_title_right_margin(232),
    adaptive_title_spacing = TRUE
  )
  exported <- transmission_spectral_comparison_plot(snapshot)

  expect_match(narrow$labels$title, "\n", fixed = TRUE)
  expect_identical(
    gsub("\n", " ", narrow$labels$title, fixed = TRUE),
    "CIE Standard Illuminant D 6500K \u00d7 IPLUS"
  )
  expect_identical(
    exported$labels$title,
    "CIE Standard Illuminant D 6500K \u00d7 IPLUS"
  )
  expect_match(
    german_narrow$labels$title,
    "Norm-\nTageslichtspektrum",
    fixed = TRUE
  )
  expect_identical(
    gsub(
      "\n",
      " ",
      gsub("-\n", "-", german_narrow$labels$title, fixed = TRUE),
      fixed = TRUE
    ),
    "Norm-Tageslichtspektrum 6500K \u00d7 IPLUS"
  )
  expect_match(
    german_archived_narrow$labels$title,
    "Norm-\nTageslicht\nspektrum",
    fixed = TRUE
  )
  expect_match(
    german_archived_narrow$labels$title,
    "spektrum 6500K\n× IPLUS",
    fixed = TRUE
  )
  expect_identical(
    english_archived_narrow$labels$title,
    "CIE Standard\nIlluminant D\n6500K × IPLUS"
  )
  expect_equal(
    as.numeric(german_archived_narrow$theme$plot.title$margin[[3L]]),
    12
  )
  expect_equal(
    as.numeric(english_archived_narrow$theme$plot.title$margin[[3L]]),
    12
  )
  expect_identical(
    gsub(
      "[[:space:]]+",
      "",
      german_archived_narrow$labels$title
    ),
    "Norm-Tageslichtspektrum6500K\u00d7IPLUS"
  )
})

test_that("response-curve selections retain stable identifiers", {
  expect_identical(
    normalize_transmission_response_curves(
      c("photopic", "photopic", "unsupported")
    ),
    "photopic"
  )
  expect_setequal(
    normalize_transmission_response_curves(c("alphaopic", "photopic")),
    names(transmission_response_curve_map())
  )
  expect_identical(
    unname(transmission_response_curve_labels()[["photopic"]]),
    "V(λ)"
  )
})

test_that("plot downloads write nonempty watermarked PNGs", {
  snapshot <- transmission_presentation_snapshot()
  paths <- c(
    tempfile(fileext = ".png"),
    tempfile(fileext = ".png"),
    tempfile(fileext = ".png")
  )
  withr::defer(unlink(paths))

  write_transmission_result_plot(
    snapshot,
    paths[[1L]],
    show_transmittance_panel = FALSE
  )
  write_transmission_result_plot(
    snapshot,
    paths[[2L]],
    show_transmittance_panel = TRUE,
    response_curves = c("melanopic", "photopic"),
    width = 10,
    height = 5,
    font_size = 12,
    max_irradiance = 8
  )
  write_transmission_filter_plot(snapshot, paths[[3L]])

  expect_true(all(file.exists(paths)))
  expect_true(all(file.info(paths)$size > 1000))
  annotation <- transmission_plot_footnote(language_direct = "English")
  expect_identical(annotation$caption, "created with **LiTG Spectran**")
})
