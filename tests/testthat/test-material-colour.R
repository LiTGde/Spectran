colour_test_filter <- function(value) {
  tibble::tibble(
    wavelength_nm = 380:780,
    transmittance = rep_len(value, 401L),
    status = "supplied"
  )
}

test_that("D65 object colours preserve neutral reflectance and lightness", {
  white <- material_reflection_colour(colour_test_filter(1))
  grey <- material_reflection_colour(colour_test_filter(.5))
  black <- material_reflection_colour(colour_test_filter(0))
  expect_identical(white$hex, "#FFFFFF")
  # The finite 380-780 nm grid and rounded conversion matrices can move
  # a neutral channel across a single 8-bit rounding boundary.
  expect_lte(max(abs(grDevices::col2rgb(grey$hex) - 188)), 1)
  expect_identical(black$hex, "#000000")
  expect_equal(unname(white$xyz['Y']), 1)
  expect_equal(unname(grey$xyz['Y']), .5)
  expect_equal(unname(black$xyz), rep(0, 3L))
  # Independent R colour conversion provides a cross-check of XYZ -> sRGB.
  rgb <- grDevices::convertColor(
    t(grey$xyz),
    from = "XYZ",
    to = "sRGB",
    from.ref.white = "D65",
    to.ref.white = "D65"
  )
  expect_lte(
    max(abs(
      grDevices::col2rgb(grDevices::rgb(rgb)) -
        grDevices::col2rgb(grey$hex)
    )),
    1
  )
})

test_that("material coefficients compare incident weighting with D65 in percentage points", {
  filter <- colour_test_filter(seq(.1, .8, length.out = 401L))
  source <- transmission_source_fixture("d65")
  source$Bestrahlungsstaerke <- stats::approx(
    examplespectra$CIE$Wellenlaenge,
    examplespectra$CIE$A,
    xout = 380:780
  )$y
  snapshot <- new_transmission_applied_snapshot(
    calculate_material_result(source, filter, "reflection"),
    metadata = list(material_mode = "reflection"),
    incident_name = "CIE A",
    draft_revision = 1L,
    apply_sequence = 1L
  )
  frozen <- snapshot
  table <- material_coefficient_comparison(snapshot)
  expect_equal(nrow(table), 6L)
  expect_equal(table$difference_pp, table$incident_percent - table$d65_percent)
  # Independently form the photopic weighted mean from the current spectrum.
  weights <- source$Bestrahlungsstaerke * spectral_action_weighting("V(lambda)")
  expect_equal(
    table$incident_percent[[1]],
    100 * sum(weights * filter$transmittance) / sum(weights)
  )
  expect_gt(abs(table$difference_pp[[1]]), .01)
  expect_true(all(table$incident_spectrum_name == "CIE A"))
  expect_identical(snapshot, frozen)
  zero <- new_transmission_applied_snapshot(
    calculate_material_result(
      transmission_source_fixture("zero"),
      filter,
      "reflection"
    ),
    metadata = list(material_mode = "reflection"),
    incident_name = "Zero",
    draft_revision = 1L,
    apply_sequence = 1L
  )
  zero_table <- material_coefficient_comparison(zero)
  expect_true(all(is.na(zero_table$incident_percent)))
  expect_true(all(is.na(zero_table$difference_pp)))
  expect_true(all(is.finite(zero_table$d65_percent)))
})

test_that("the relative colour follows source shape, not illuminance", {
  filter <- colour_test_filter(.5)
  source <- transmission_source_fixture("d65")
  original <- material_reflection_colour(filter, source)
  brighter <- source
  brighter$Bestrahlungsstaerke <- source$Bestrahlungsstaerke * 100
  expect_equal(material_reflection_colour(filter, brighter), original)
  warm <- source
  warm$Bestrahlungsstaerke <- stats::approx(
    examplespectra$CIE$Wellenlaenge,
    examplespectra$CIE$A,
    xout = 380:780
  )$y
  warm_colour <- material_reflection_colour(filter, warm)
  expect_false(identical(warm_colour$hex, original$hex))
  expect_equal(unname(warm_colour$xyz['Y']), .5)
  expect_gt(warm_colour$linear_rgb[['R']], warm_colour$linear_rgb[['B']])
  zero <- material_reflection_colour(
    filter,
    transmission_source_fixture("zero")
  )
  expect_false(zero$defined)
  expect_true(is.na(zero$hex))
  expect_named(zero, names(original))
  expect_error(material_reflection_colour(colour_test_filter(1.1)))
})

test_that("narrow spectra are explicitly clipped to valid screen colours", {
  source <- transmission_source_fixture("d65")
  source$Bestrahlungsstaerke <- as.numeric(source$Wellenlaenge == 530)
  colour <- material_reflection_colour(colour_test_filter(1), source)
  expect_true(colour$out_of_gamut)
  expect_match(colour$hex, "^#[0-9A-F]{6}$")
  expect_equal(unname(colour$xyz['Y']), 1)
})

test_that("the colour preview shows only D65 and explains incomplete curves", {
  old_language <- the$language
  withr::defer(the$language <- old_language)
  for (locale in c("Deutsch", "English")) {
    the$language <- locale
    ui <- as.character(material_colour_preview_ui(
      colour_test_filter(.5)
    ))
    expect_match(
      ui,
      material_reflection_colour(colour_test_filter(.5))$hex,
      fixed = TRUE
    )
    expect_match(ui, material_text("colour_d65"), fixed = TRUE)
    expect_match(ui, material_text("colour_luminance"), fixed = TRUE)
    expect_match(ui, 'role="img"', fixed = TRUE)
    expect_length(gregexpr('class="material-colour-swatch"', ui)[[1]], 1L)
    expect_false(grepl(
      "Under the incident light|Unter dem einfallenden Licht",
      ui
    ))
    expect_match(ui, material_text("colour_note"), fixed = TRUE)
    expect_match(ui, material_text("colour_method"), fixed = TRUE)
    incomplete <- as.character(material_colour_preview_ui(NULL))
    expect_match(incomplete, material_text("colour_incomplete"), fixed = TRUE)
  }
})

test_that("German catalogue names identify DIN descriptions and TUB aliases", {
  records <- tub_material_records
  glass <- records[records$category_id == "glazing", ]
  expect_equal(nrow(glass), 12L)
  expect_match(
    glass$display_name_de[[1]],
    "Einfachverglasung (FG)",
    fixed = TRUE
  )
  expect_match(glass$display_name_de[[2]], "Doppel-ISV (FG-A-FG)", fixed = TRUE)
  expect_true(all(grepl(
    "DIN/TS 67600:2022-08, Tabelle 6",
    glass$source_description_de,
    fixed = TRUE
  )))
  brick <- records[records$catalogue_id == "tub:reflection:S1", ]
  expect_match(brick$display_name_de, "gelb (TUB: beige)", fixed = TRUE)
  expect_match(brick$source_description_de, "416", fixed = TRUE)
  expect_match(brick$source_description_en, "yellow brick", fixed = TRUE)
  concrete <- records[records$catalogue_id == "tub:reflection:S3", ]
  expect_match(concrete$display_name_de, "Beton fein", fixed = TRUE)
  expect_equal(nrow(records), 55L)
})

test_that("alpha-opic tables and dedicated exports separate EDI and DER metrics", {
  old_language <- the$language
  withr::defer(the$language <- old_language)
  filter <- colour_test_filter(.5)
  for (mode in c("reflection", "transmission")) {
    snapshot <- new_transmission_applied_snapshot(
      calculate_material_result(
        transmission_source_fixture("d65"),
        filter,
        mode
      ),
      metadata = list(filter_name = "Neutral", material_mode = mode),
      incident_name = "D65",
      draft_revision = 1L,
      apply_sequence = 1L
    )
    exported <- transmission_balance_metrics_export(snapshot)
    expect_equal(nrow(exported), 11L)
    expect_identical(exported$metric_group, c(rep("EDI", 5), rep("DER", 6)))
    expect_false(any(grepl("_action_factor$", exported$metric_id)))
    expect_true("melanopic_der_effective" %in% exported$metric_id)
    for (locale in c("Deutsch", "English")) {
      the$language <- locale
      html <- gt::as_raw_html(transmission_balance_gt(snapshot))
      expect_false(grepl("Wirkfaktor|action factor", html, ignore.case = TRUE))
      expect_match(html, "DER", fixed = TRUE)
    }
  }
  the$language <- "Deutsch"
  expect_identical(transmission_text("result_tab_light"), "Licht und Strahlung")
  expect_identical(transmission_text("result_tab_balance"), "alpha-opisch")
  expect_identical(transmission_text("result_tab_d65"), "Transmissionsgrad")
  expect_identical(
    material_labeler("transmission")("catalogue_all"),
    "Alle Materialien für Transmission"
  )
  expect_identical(
    material_labeler("reflection")("catalogue_all"),
    "Alle Materialien für Reflexion"
  )
  html <- gt::as_raw_html(transmission_d65_gt(new_transmission_applied_snapshot(
    calculate_material_result(
      transmission_source_fixture("d65"),
      filter,
      "reflection"
    ),
    metadata = list(material_mode = "reflection"),
    incident_name = "D65",
    draft_revision = 1L,
    apply_sequence = 1L
  )))
  expect_match(html, "Photopischer Reflexionsgrad", fixed = TRUE)
  expect_false(grepl("LichtReflexionsgrad", html, fixed = TRUE))
})

test_that("result tabs prioritize coefficients and freeze reflection colours", {
  old_language <- the$language
  withr::defer(the$language <- old_language)
  the$language <- "Deutsch"
  source <- shiny::reactiveVal(transmission_source_fixture("d65"))
  shiny::testServer(
    transmissionServer,
    args = list(
      incident_spectrum = source,
      incident_name = shiny::reactive("Testlicht")
    ),
    {
      session$setInputs(
        material_mode = "reflection",
        input_source = "catalogue",
        catalogue_collection = "tub67600",
        catalogue_filter = "tub:reflection:S1"
      )
      session$flushReact()
      session$setInputs(`apply-apply_filter` = 1L)
      session$flushReact()
      html <- output[["apply-applied_outputs"]]$html
      expect_match(html, 'data-value="light"', fixed = TRUE)
      positions <- vapply(
        c('data-value="d65"', 'data-value="light"', 'data-value="balance"'),
        function(label) regexpr(label, html, fixed = TRUE)[[1]],
        integer(1)
      )
      expect_true(all(positions > 0L))
      expect_true(all(diff(positions) > 0L))
      expect_match(html, 'class="active"', fixed = TRUE)
      expect_match(html, "Ungefähre Materialfarbe", fixed = TRUE)
      returned <- session$getReturned()
      frozen_colour <- material_reflection_colour(
        returned$applied_snapshot()$filter
      )$hex
      source(transmission_source_fixture("zero"))
      session$flushReact()
      expect_match(
        output[["apply-applied_outputs"]]$html,
        frozen_colour,
        fixed = TRUE
      )
      expect_true(returned$applied_stale())
      session$setInputs(
        material_mode = "transmission",
        catalogue_filter = "tub:transmission:G1"
      )
      session$flushReact()
      session$setInputs(`apply-apply_filter` = 2L)
      session$flushReact()
      html <- output[["apply-applied_outputs"]]$html
      expect_match(html, "Transmissionsgrad", fixed = TRUE)
      expect_false(grepl("Ungefähre Materialfarbe", html, fixed = TRUE))
      expect_false(grepl("Reflexionsgrad", html, fixed = TRUE))
    }
  )
})

for (locale in c("English", "Deutsch")) {
  test_that(paste("D65 alone persists in every reflection view in", locale), {
    old_language <- the$language
    withr::defer(the$language <- old_language)
    the$language <- locale
    active <- shiny::reactiveVal(new_transmission_active_spectrum(
      transmission_source_fixture("d65"),
      "Source",
      "Test",
      1L,
      "import",
      "node-1"
    ))
    shiny::testServer(
      transmissionServer,
      args = list(
        incident_spectrum = shiny::reactive(active()$spectrum),
        incident_name = shiny::reactive(active()$name),
        active_state = shiny::reactive(active())
      ),
      {
        session$setInputs(
          material_mode = "reflection",
          input_source = "catalogue",
          catalogue_collection = "tub67600",
          catalogue_filter = "tub:reflection:S1"
        )
        session$flushReact()
        session$setInputs(`apply-apply_filter` = 1L)
        session$flushReact()
        returned <- session$getReturned()
        frozen <- returned$applied_snapshot()
        d65 <- material_reflection_colour(frozen$filter)$hex
        session$setInputs(
          `history-promotion_lux` = 0,
          `history-promotion_name` = "Zero receiver",
          `history-promote` = 1L
        )
        session$flushReact()
        event <- returned$promotion_event()
        expect_s3_class(event, "transmission_activation_event")
        active(transmission_active_spectrum_from_event(event, 2L))
        session$flushReact()
        expect_true(all(active()$spectrum$Bestrahlungsstaerke == 0))
        for (id in c(
          "preview_outputs_spectrum",
          "preview_outputs_normalization",
          "apply-applied_outputs",
          "history-archived_outputs"
        )) {
          html <- output[[id]]$html
          expect_match(html, d65, fixed = TRUE)
          expect_match(html, material_text("colour_luminance"), fixed = TRUE)
          expect_length(
            gregexpr('class="material-colour-swatch"', html)[[1]],
            1L
          )
          expect_false(grepl(
            "Under the incident light|Unter dem einfallenden Licht",
            html
          ))
        }
        # Draft changes affect the draft colour, while the archived material
        # remains the one actually applied before the zero-lux promotion.
        session$setInputs(catalogue_filter = "tub:reflection:S2")
        session$flushReact()
        expect_identical(returned$archived_snapshot(), frozen)
        expect_match(
          output[["history-archived_outputs"]]$html,
          d65,
          fixed = TRUE
        )
      }
    )
  })
}
