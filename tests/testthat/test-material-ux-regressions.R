for (locale in c("English", "Deutsch")) {
  for (mode in c("reflection", "transmission")) {
    test_that(
      paste(
        "partial upload previews recover without calculation errors",
        locale,
        mode
      ),
      {
        old_language <- the$language
        withr::defer(the$language <- old_language)
        the$language <- locale
        path <- tempfile(fileext = ".csv")
        withr::defer(unlink(path))
        shiny::testServer(
          transmissionServer,
          args = list(
            incident_spectrum = shiny::reactive(transmission_source_fixture(
              "d65"
            )),
            incident_name = shiny::reactive("D65")
          ),
          {
            session$flushReact()
            session$setInputs(
              material_mode = mode,
              input_source = "upload",
              `csv-skip` = 0,
              `csv-delimiter` = ",",
              `csv-decimal_mark` = ".",
              `csv-wavelength_column` = 1,
              `csv-value_column` = 2,
              `csv-header` = TRUE
            )
            returned <- session$getReturned()
            preview_ids <- c(
              "preview_outputs_spectrum",
              "preview_outputs_normalization"
            )
            cases <- list(
              both = transmission_fixture("partial"),
              lower = subset(
                transmission_fixture("neutral"),
                wavelength_nm >= 420
              ),
              upper = subset(
                transmission_fixture("neutral"),
                wavelength_nm <= 700
              ),
              gap = transmission_fixture("large_gap")
            )
            for (case in names(cases)) {
              utils::write.csv(
                cases[[case]][c("wavelength_nm", "value")],
                path,
                row.names = FALSE
              )
              session$setInputs(
                filter_file = list(
                  datapath = path,
                  name = paste0(case, ".csv"),
                  size = file.info(path)$size,
                  type = "text/csv"
                )
              )
              session$flushReact()
              session$setInputs(
                filter_name = paste("Test", case),
                scale = "fraction",
                transmittance_type = "total",
                scattering = "no",
                lower_tail = "",
                upper_tail = "",
                large_gap_ack = FALSE
              )
              session$flushReact()
              expect_false(returned$ready())
              expect_length(returned$diagnostics()$errors, 0L)
              expect_gt(length(returned$diagnostics()$requirements), 0L)
              for (id in preview_ids) {
                html <- output[[id]]$html
                expect_match(html, "construction_plot", fixed = TRUE)
                expect_false(grepl(
                  "material-colour-swatch",
                  html,
                  fixed = TRUE
                ))
                if (mode == "reflection")
                  expect_match(
                    html,
                    material_text("colour_incomplete"),
                    fixed = TRUE
                  )
              }
              session$setInputs(
                lower_tail = "carry",
                upper_tail = "carry",
                large_gap_ack = TRUE
              )
              session$flushReact()
              expect_true(returned$ready())
              for (id in preview_ids) {
                html <- output[[id]]$html
                expect_match(html, "construction_plot", fixed = TRUE)
                expect_identical(
                  grepl("material-colour-swatch", html, fixed = TRUE),
                  mode == "reflection"
                )
                expect_false(grepl(
                  material_text("colour_incomplete"),
                  html,
                  fixed = TRUE
                ))
              }
            }
            # Actual bad supplied samples still fail validation, rather than
            # being treated as an undecided tail or interpolatable gap.
            for (bad in c("range", "nonfinite")) {
              data <- transmission_fixture("neutral")
              data$value[[3L]] <- if (bad == "range") 1.2 else Inf
              utils::write.csv(
                data[c("wavelength_nm", "value")],
                path,
                row.names = FALSE
              )
              session$setInputs(
                filter_file = list(
                  datapath = path,
                  name = paste0(bad, ".csv"),
                  size = file.info(path)$size,
                  type = "text/csv"
                )
              )
              session$flushReact()
              expect_false(returned$ready())
              expect_gt(length(returned$diagnostics()$errors), 0L)
              for (id in preview_ids)
                expect_false(grepl(
                  "material-colour-swatch",
                  output[[id]]$html,
                  fixed = TRUE
                ))
            }
          }
        )
      }
    )
  }
  for (input_scale in c("fraction", "percent")) {
    test_that(
      paste(
        "uploaded metadata and tails survive mode changes in",
        locale,
        input_scale
      ),
      {
        old_language <- the$language
        withr::defer(the$language <- old_language)
        the$language <- locale
        data <- transmission_fixture("partial")
        # Low percent values also fit the fraction range, so readiness alone
        # cannot detect an accidental reset of the selected scale.
        path <- tempfile(fileext = ".csv")
        withr::defer(unlink(path))
        utils::write.csv(
          data[c("wavelength_nm", "value")],
          path,
          row.names = FALSE
        )
        shiny::testServer(
          transmissionServer,
          args = list(
            incident_spectrum = shiny::reactive(transmission_source_fixture(
              "d65"
            )),
            incident_name = shiny::reactive("D65")
          ),
          {
            session$flushReact()
            session$setInputs(
              material_mode = "reflection",
              input_source = "upload",
              `csv-skip` = 0,
              `csv-delimiter` = ",",
              `csv-decimal_mark` = ".",
              `csv-wavelength_column` = 1,
              `csv-value_column` = 2,
              `csv-header` = TRUE,
              filter_file = list(
                datapath = path,
                name = "partial-material.csv",
                size = file.info(path)$size,
                type = "text/csv"
              )
            )
            session$flushReact()
            session$setInputs(
              filter_name = "Partial wall",
              scale = input_scale,
              transmittance_type = "total",
              scattering = "no",
              measurement_geometry = "Integrating sphere",
              measurement_angle = "8 degrees"
            )
            session$flushReact()
            returned <- session$getReturned()
            messages <- returned$diagnostics()$requirements
            expect_length(messages, 2L)
            expect_false(any(grepl("opaque|transparent|opak", messages)))
            expect_true(all(grepl(
              if (locale == "English") "reflectance" else "Reflexion",
              messages
            )))
            groups <- transmission_requirement_groups(
              NULL,
              returned$preparation(),
              mode = "reflection"
            )
            expect_identical(groups$normalization, messages)
            controls <- output$coverage_controls$html
            expect_false(grepl("Opaque|Transparent|Opak", controls))
            session$setInputs(lower_tail = "zero", upper_tail = "one")
            session$flushReact()
            expect_true(returned$ready())
            expect_equal(
              returned$preparation()$completed$transmittance[c(1L, 401L)],
              c(0, 1)
            )
            expect_equal(
              returned$metadata()$normalization_decisions$lower_tail,
              "zero"
            )
            expect_equal(
              returned$metadata()$normalization_decisions$upper_tail,
              "one"
            )
            completed <- returned$preparation()$completed
            for (mode in c("transmission", "reflection")) {
              session$setInputs(material_mode = mode)
              session$flushReact()
              # testServer does not rebind renderUI defaults as a browser does.
              # Check the rebuilt controls as well as the retained server draft.
              details <- output$material_details$html
              expect_match(details, 'value="Partial wall"', fixed = TRUE)
              expect_match(
                details,
                paste0('value="', input_scale, '" selected'),
                fixed = TRUE
              )
              expect_match(details, 'value="Integrating sphere"', fixed = TRUE)
              expect_match(details, 'value="8 degrees"', fixed = TRUE)
              labels <- material_labeler(mode)
              controls <- output$coverage_controls$html
              expect_match(controls, labels("tail_opaque"), fixed = TRUE)
              expect_match(controls, labels("tail_transparent"), fixed = TRUE)
              expect_match(controls, 'value="zero" selected', fixed = TRUE)
              expect_match(controls, 'value="one" selected', fixed = TRUE)
              expect_true(returned$ready())
              expect_identical(returned$preparation()$completed, completed)
              expect_identical(
                returned$metadata()$normalization_decisions$lower_tail,
                "zero"
              )
              expect_identical(
                returned$metadata()$normalization_decisions$upper_tail,
                "one"
              )
              session$setInputs(
                `apply-apply_filter` = if (mode == "transmission") 1L else 2L
              )
              session$flushReact()
              snapshot <- returned$applied_snapshot()
              expect_identical(snapshot$filter, completed)
              audit <- transmission_flatten_metadata(snapshot$metadata)
              expect_identical(
                audit$value[audit$field == "filter_name"],
                "Partial wall"
              )
              expect_identical(audit$value[audit$field == "scale"], input_scale)
              expect_identical(
                audit$value[audit$field == "material_mode"],
                mode
              )
              session$setInputs(lower_tail = "", upper_tail = "")
              session$flushReact()
              expect_false(returned$ready())
              expect_identical(
                returned$diagnostics()$requirements,
                transmission_requirement_groups(
                  NULL,
                  returned$preparation(),
                  mode = mode
                )$normalization
              )
              expect_true(all(grepl(
                if (mode == "reflection") "reflectance|Reflexion" else
                  "opaque|opak",
                returned$diagnostics()$requirements
              )))
              session$setInputs(lower_tail = "zero", upper_tail = "one")
              session$flushReact()
            }
            receipt <- output$file_transport_status$html
            expect_match(
              receipt,
              if (locale == "English") "readiness panel" else "Datei empfangen",
              fixed = TRUE
            )
            expect_false(grepl("panel above", receipt, fixed = TRUE))
            if (locale == "Deutsch")
              expect_false(grepl("File received", receipt, fixed = TRUE))
          }
        )
      }
    )
  }

  test_that(paste("material CSV column wording is neutral in", locale), {
    old_language <- the$language
    withr::defer(the$language <- old_language)
    the$language <- locale
    ui <- as.character(transmissionUI("material", layout = "tabs"))
    expect_match(
      ui,
      if (locale == "English") "Material coefficient column" else
        "Spalte des Materialkoeffizienten",
      fixed = TRUE
    )
  })

  test_that(
    paste(
      "export wording follows the selected mixed-history result in",
      locale
    ),
    {
      old_language <- the$language
      withr::defer(the$language <- old_language)
      the$language <- locale
      active <- shiny::reactiveVal(new_transmission_active_spectrum(
        transmission_source_fixture("d65"),
        "D65",
        "Fixture",
        1L,
        "import",
        "node-1"
      ))
      shiny::testServer(
        transmissionServer,
        args = list(
          incident_spectrum = shiny::reactive(active()$spectrum),
          incident_name = shiny::reactive(active()$name),
          active_state = active
        ),
        {
          session$setInputs(
            material_mode = "reflection",
            input_source = "catalogue",
            catalogue_collection = "tub67600",
            catalogue_filter = "tub:reflection:WF5"
          )
          session$flushReact()
          session$setInputs(`apply-apply_filter` = 1L)
          session$flushReact()
          returned <- session$getReturned()
          reflection_label <- if (locale == "English")
            "Reflectance spectrum (PNG)" else "Reflexionsspektrum (PNG)"
          transmission_label <- if (locale == "English")
            "Transmittance spectrum (PNG)" else "Transmissionsspektrum (PNG)"
          quantity_label <- if (locale == "English")
            "irradiance / exitance" else "spezifische Ausstrahlung"
          expect_match(
            output[["history-export_panel"]]$html,
            reflection_label,
            fixed = TRUE
          )
          expect_match(
            output[["history-export_panel"]]$html,
            quantity_label,
            fixed = TRUE
          )
          session$setInputs(
            `history-promotion_name` = "Oak receiver",
            `history-promote` = 1L
          )
          session$flushReact()
          active(transmission_active_spectrum_from_event(
            returned$promotion_event(),
            2L
          ))
          session$flushReact()
          session$setInputs(
            material_mode = "transmission",
            catalogue_filter = "tub:transmission:G1"
          )
          session$flushReact()
          session$setInputs(
            `apply-apply_filter` = 2L,
            `history-export_result` = "current"
          )
          session$flushReact()
          expect_match(
            output[["history-export_panel"]]$html,
            transmission_label,
            fixed = TRUE
          )
          expect_false(grepl(
            quantity_label,
            output[["history-export_panel"]]$html,
            fixed = TRUE
          ))
          session$setInputs(`history-export_result` = "archive:node-2")
          session$flushReact()
          expect_match(
            output[["history-export_panel"]]$html,
            reflection_label,
            fixed = TRUE
          )
          expect_match(
            output[["history-export_panel"]]$html,
            quantity_label,
            fixed = TRUE
          )
          expect_false(grepl(
            transmission_label,
            output[["history-export_panel"]]$html,
            fixed = TRUE
          ))
        }
      )
    }
  )
}

test_that("interaction changes preserve measurement details and require fresh qualification", {
  shiny::testServer(
    transmissionServer,
    args = list(
      fixture_data = shiny::reactive(transmission_fixture("neutral"))
    ),
    {
      session$flushReact()
      session$setInputs(
        material_mode = "reflection",
        filter_name = "Qualified material",
        scale = "percent",
        transmittance_type = "internal",
        scattering = "yes",
        measurement_geometry = "Directional measurement",
        measurement_angle = "45 degrees"
      )
      session$flushReact()
      returned <- session$getReturned()
      for (mode in c("transmission", "reflection")) {
        session$setInputs(type_ack = TRUE, scattering_ack = TRUE)
        session$flushReact()
        expect_true(returned$ready())
        session$setInputs(material_mode = mode)
        session$flushReact()
        details <- output$material_details$html
        expect_match(details, 'value="internal" selected', fixed = TRUE)
        expect_match(details, 'value="yes" selected', fixed = TRUE)
        expect_match(details, 'value="Directional measurement"', fixed = TRUE)
        expect_match(details, 'value="45 degrees"', fixed = TRUE)
        expect_false(returned$metadata()$qualified_type_acknowledged)
        expect_false(returned$metadata()$scattering_acknowledged)
        expect_false(returned$ready())
        expect_false(grepl(
          'checked',
          output$type_acknowledgement$html,
          fixed = TRUE
        ))
        expect_false(grepl(
          'checked',
          output$scattering_acknowledgement$html,
          fixed = TRUE
        ))
      }
      session$setInputs(type_ack = TRUE, scattering_ack = TRUE)
      session$flushReact()
      expect_true(returned$ready())
    }
  )
})

test_that("reflection bundle files and bilingual normalization records describe reflectance", {
  source <- transmission_source_fixture("d65")
  preparation <- prepare_transmission_curve(
    transmission_fixture("partial"),
    "fraction",
    lower_tail = "zero",
    upper_tail = "one"
  )
  snapshot <- new_transmission_applied_snapshot(
    calculate_material_result(source, preparation$completed, "reflection"),
    metadata = list(
      filter_name = "Partial wall",
      material_mode = "reflection",
      normalization_decisions = list(lower_tail = "zero", upper_tail = "one")
    ),
    incident_name = "D65",
    draft_revision = 1L,
    apply_sequence = 1L
  )
  active <- new_transmission_active_spectrum(
    source,
    "D65",
    "Fixture",
    1L,
    "import",
    "node-1"
  )
  audit <- transmission_decisions_warnings_export(snapshot)
  expect_false(any(grepl(
    "opaque|transparent|opak",
    audit$value,
    ignore.case = TRUE
  )))
  expect_match(
    audit$value[audit$item == "lower_tail_English"],
    "zero reflectance",
    fixed = TRUE
  )
  expect_match(
    audit$value[audit$item == "upper_tail_Deutsch"],
    "Vollst\u00e4ndige Reflexion",
    fixed = TRUE
  )
  snapshot$metadata$normalization_decisions$upper_tail <- "carry"
  carry_audit <- transmission_decisions_warnings_export(snapshot)
  expect_match(
    carry_audit$value[carry_audit$item == "upper_tail_English"],
    "Carry",
    fixed = TRUE
  )
  readme <- transmission_audit_readme(snapshot, active, Sys.time())
  expect_false(any(grepl("transmittierte Spektren", readme, fixed = TRUE)))
  path <- tempfile(fileext = ".zip")
  withr::defer(unlink(path))
  write_transmission_export_bundle(
    path,
    snapshot,
    new_transmission_history(active),
    active,
    contents = "filter_plot"
  )
  expect_identical(
    utils::unzip(path, list = TRUE)$Name,
    "partial-wall-reflectance-spectrum.png"
  )
})
