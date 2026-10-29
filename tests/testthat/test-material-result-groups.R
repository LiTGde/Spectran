test_that("light and alpha-opic tables keep disjoint metric groups and exact snapshot values", {
  for (mode in c("transmission", "reflection")) {
    for (source_id in c("d65", "equal_energy", "zero")) {
      source <- transmission_source_fixture(source_id)
      filter <- tibble::tibble(
        wavelength_nm = 380:780,
        transmittance = seq(.2, .8, length.out = 401),
        status = "supplied"
      )
      snapshot <- new_transmission_applied_snapshot(
        calculate_material_result(source, filter, mode),
        metadata = list(material_mode = mode),
        incident_name = source_id,
        draft_revision = 1L,
        apply_sequence = 1L
      )
      light <- transmission_light_metrics_export(snapshot)
      alpha <- transmission_balance_metrics_export(snapshot)
      expect_identical(light$metric_id[[1]], "photopic_illuminance")
      expect_equal(nrow(light), 7L)
      expect_true(all(grepl("_irradiance$", light$metric_id[-1])))
      expect_equal(nrow(alpha), 11L)
      expect_identical(alpha$metric_group, c(rep("EDI", 5), rep("DER", 6)))
      expect_true(all(alpha$unit[alpha$metric_group == "EDI"] == "lx"))
      expect_length(intersect(light$metric_id, alpha$metric_id), 0L)
      for (records in list(light, alpha)) {
        rows <- match(records$metric_id, snapshot$active_metrics$metric_id)
        expect_identical(
          records$incident_value,
          snapshot$active_metrics$incident_value[rows]
        )
        output_column <- if (mode == "reflection") "outcome_value" else
          "transmitted_value"
        expect_identical(
          records[[output_column]],
          snapshot$active_metrics$transmitted_value[rows]
        )
        expect_identical(
          records$absolute_change,
          snapshot$active_metrics$absolute_change[rows]
        )
      }
      table <- transmission_balance_gt(snapshot)
      expect_identical(table$`_row_groups`, c("edi", "der"))
      expect_identical(table$`_data`$unit[1:5], rep("lx", 5))
      if (source_id == "zero") {
        expect_equal(table$`_data`$incident[1:5], rep(0, 5))
        expect_equal(table$`_data`$transmitted[1:5], rep(0, 5))
        expect_equal(table$`_data`$absolute_change[1:5], rep(0, 5))
        expect_true(all(is.na(table$`_data`$relative_change[1:5])))
      }
    }
  }
})

test_that("D65 comparisons display zero without spurious signs and preserve precision", {
  old_language <- the$language
  withr::defer(the$language <- old_language)
  the$language <- "English"
  state <- shiny::reactiveValues()
  shiny::isolate(activate_spectran_default_daylight(state))
  filter <- tibble::tibble(
    wavelength_nm = 380:780,
    transmittance = seq(.2, .8, length.out = 401),
    status = "supplied"
  )
  snapshot <- new_transmission_applied_snapshot(
    calculate_material_result(
      shiny::isolate(state$Spectrum),
      filter,
      "transmission"
    ),
    incident_name = "Automatic D65",
    metadata = list(),
    draft_revision = 1L,
    apply_sequence = 1L
  )
  original <- material_coefficient_comparison(snapshot)
  expect_equal(original$difference_pp, rep(0, 6), tolerance = 1e-12)
  for (locale in c("Deutsch", "English")) {
    the$language <- locale
    table <- transmission_d65_gt(snapshot)
    values <- gt::extract_cells(
      table,
      columns = "difference_pp",
      output = "plain"
    )
    expect_identical(values, rep(if (locale == "Deutsch") "0,0" else "0.0", 6))
    expect_identical(table$`_data`$difference_pp, original$difference_pp)
  }
})
