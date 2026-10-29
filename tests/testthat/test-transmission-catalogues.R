test_that("bundled transmission catalogue counts are pinned", {
  records <- transmission_catalogue_records_data()

  expect_equal(nrow(records), 147L)
  expect_equal(sum(records$catalogue == "facade_windows"), 26L)
  expect_equal(sum(records$catalogue == "spitschan2019"), 121L)
  expect_equal(anyDuplicated(records$catalogue_id), 0L)
  expect_true(all(
    records$catalogue_id %in%
      transmission_catalogue_curves$catalogue_id
  ))
})

test_that("catalogue curves satisfy the installed-data contract", {
  records <- transmission_catalogue_records_data()
  curves <- transmission_catalogue_curves

  expect_true(all(is.finite(curves$wavelength_nm)))
  expect_true(all(is.finite(curves$transmittance)))
  expect_true(all(curves$transmittance >= 0 & curves$transmittance <= 1))

  counts <- table(curves$catalogue_id)
  expect_equal(
    unname(as.integer(counts[records$catalogue_id])),
    records$source_points
  )

  by_record <- split(curves, curves$catalogue_id)
  expect_true(all(vapply(
    by_record,
    function(curve) {
      !anyDuplicated(curve$wavelength_nm) &&
        identical(curve$wavelength_nm, sort(curve$wavelength_nm))
    },
    logical(1)
  )))
})

test_that("façade catalogue retains bracketing data and total type", {
  records <- transmission_catalogue_records_data()
  facade <- records[records$catalogue == "facade_windows", ]
  iplus <- facade[facade$catalogue_id == "facade:IPLUS", ]

  expect_true(all(facade$wavelength_min_nm <= 380))
  expect_true(all(facade$wavelength_max_nm >= 780))
  expect_true(all(facade$transmittance_type == "total"))
  expect_true(all(facade$source_version == "0.6.1"))
  expect_true(all(grepl("CIE", facade$source_reference, fixed = TRUE)))
  expect_equal(nrow(iplus), 1L)
  expect_equal(iplus$thickness_mm, 4)
  expect_match(
    iplus$transformation,
    "thickness metadata converted from metres to millimetres",
    fixed = TRUE
  )
})

test_that("Spitschan catalogue keeps pinned provenance and missing tails", {
  records <- transmission_catalogue_records_data()
  spitschan <- records[records$catalogue == "spitschan2019", ]
  provenance <- transmission_catalogue_provenance$spitschan2019

  expect_identical(
    provenance$commit,
    "3f68efef2101a4eacc4a756e4c44df46dd90a7e5"
  )
  expect_identical(
    provenance$archive_sha256,
    "5ab0d81db46cb17ddf179df36fd9104dd3b7ac33442036bb58968aa0445518f9"
  )
  expect_equal(provenance$record_count, 121L)
  expect_true(any(spitschan$wavelength_min_nm > 380))
  expect_true(any(spitschan$wavelength_max_nm < 780))
  expect_true(all(spitschan$wavelength_min_nm >= 380))
  expect_true(all(spitschan$wavelength_max_nm <= 780))
  expect_true(all(spitschan$transmittance_type == "unknown"))
  expect_true(any(spitschan$featured))
  expect_true(any(spitschan$category_en == "Medical lenses"))
})

test_that("all catalogue records carry citation and licence metadata", {
  records <- transmission_catalogue_records_data()

  expect_true(all(nzchar(records$citation)))
  expect_true(all(nzchar(records$licence)))
  expect_true(all(nzchar(records$source_url)))
  expect_true(all(nzchar(records$transformation)))
  expect_true(all(vapply(
    transmission_catalogue_provenance,
    function(source) nzchar(source$citation) && nzchar(source$licence),
    logical(1)
  )))
})

test_that("catalogue narratives are explicit bilingual source data", {
  records <- transmission_catalogue_records_data()
  localized_fields <- c(
    "source_description",
    "measurement_geometry",
    "licence",
    "transformation",
    "source_tail_treatment"
  )
  expected_columns <- unlist(lapply(
    localized_fields,
    function(field) paste0(field, c("_en", "_de"))
  ))

  expect_true(all(expected_columns %in% names(records)))
  expect_true(all(nzchar(records$licence_en)))
  expect_true(all(nzchar(records$licence_de)))
  expect_true(all(nzchar(records$transformation_en)))
  expect_true(all(nzchar(records$transformation_de)))

  fl41 <- records[records$catalogue_id == "spitschan2019:074", ]
  expect_identical(nrow(fl41), 1L)
  expect_match(
    transmission_catalogue_localized_value(
      fl41,
      "source_tail_treatment",
      language_direct = "Deutsch"
    ),
    "Quellpublikation",
    fixed = TRUE
  )
  expect_false(grepl(
    "source paper",
    transmission_catalogue_localized_value(
      fl41,
      "source_tail_treatment",
      language_direct = "Deutsch"
    ),
    fixed = TRUE
  ))
})

test_that("catalogue search and choice helpers are deterministic", {
  featured <- filter_transmission_catalogue(collection = "featured")
  medical <- filter_transmission_catalogue(
    collection = "spitschan2019",
    category = "spitschan_1"
  )
  no_ir <- filter_transmission_catalogue(
    collection = "all",
    query = "NoIR"
  )

  expect_true(nrow(featured) > 0L)
  expect_true(all(featured$featured))
  expect_equal(nrow(medical), 76L)
  expect_true(nrow(no_ir) >= 1L)
  expect_true(all(grepl("NoIR", paste(no_ir$display_name, no_ir$source_file))))

  choices <- transmission_catalogue_choices(no_ir)
  expect_equal(length(choices), nrow(no_ir))
  expect_setequal(unname(choices), no_ir$catalogue_id)
})

test_that("façade catalogue records use the shared preparation path", {
  facade <- transmission_catalogue_records_data()
  facade <- facade[facade$catalogue == "facade_windows", , drop = FALSE]
  selected_id <- facade$catalogue_id[[1L]]

  shiny::testServer(
    transmissionServer,
    args = list(
      incident_spectrum = shiny::reactive(
        transmission_source_fixture("d65")
      ),
      incident_name = shiny::reactive("CIE D65 at 250 lx")
    ),
    {
      session$setInputs(
        input_source = "catalogue",
        catalogue_collection = "facade_windows",
        catalogue_category = "all",
        catalogue_filter = selected_id,
        scattering = "no"
      )
      session$flushReact()
      session$setInputs(
        filter_name = facade$display_name[[1L]],
        scale = "fraction",
        transmittance_type = "total",
        scattering = "no"
      )
      session$flushReact()
      returned <- session$getReturned()

      expect_true(returned$ready())
      expect_equal(nrow(returned$preparation()$completed), 401L)
      expect_identical(
        returned$metadata()$input_record$record_type,
        "catalogue"
      )
      expect_identical(
        returned$metadata()$input_record$catalogue_id,
        selected_id
      )
      expect_match(
        returned$metadata()$input_record$citation,
        "Aphalo",
        fixed = TRUE
      )
    }
  )
})

test_that("Spitschan catalogue records retain qualification and tail gates", {
  records <- transmission_catalogue_records_data()
  candidates <- records[
    records$catalogue == "spitschan2019" &
      records$wavelength_min_nm > 380 &
      records$wavelength_max_nm < 780,
    ,
    drop = FALSE
  ]
  expect_gt(nrow(candidates), 0L)
  selected_id <- candidates$catalogue_id[[1L]]

  shiny::testServer(
    transmissionServer,
    args = list(
      incident_spectrum = shiny::reactive(
        transmission_source_fixture("d65")
      ),
      incident_name = shiny::reactive("CIE D65 at 250 lx")
    ),
    {
      session$setInputs(
        input_source = "catalogue",
        catalogue_collection = "spitschan2019",
        catalogue_category = "all",
        catalogue_filter = selected_id,
        scattering = "no"
      )
      session$flushReact()
      session$setInputs(
        filter_name = candidates$display_name[[1L]],
        scale = "fraction",
        transmittance_type = "unknown",
        scattering = "no"
      )
      session$flushReact()
      returned <- session$getReturned()

      expect_identical(returned$metadata()$transmittance_type, "unknown")
      expect_false(returned$ready())
      expect_true(returned$preparation()$diagnostics$lower_missing)
      expect_true(returned$preparation()$diagnostics$upper_missing)

      session$setInputs(
        type_ack = TRUE,
        lower_tail = "zero",
        upper_tail = "one"
      )
      session$flushReact()

      expect_true(returned$ready())
      expect_equal(nrow(returned$preparation()$completed), 401L)
      expect_identical(
        returned$metadata()$input_record$source_commit,
        "3f68efef2101a4eacc4a756e4c44df46dd90a7e5"
      )
    }
  )
})

test_that("selected catalogue cards disclose pinned measurement provenance", {
  old_language <- the$language
  withr::defer(the$language <- old_language)
  the$language <- "English"

  records <- transmission_catalogue_records_data()
  selected <- c(
    records$catalogue_id[records$catalogue == "facade_windows"][[1L]],
    "spitschan2019:074",
    "spitschan2019:078"
  )
  expect_true(all(selected %in% records$catalogue_id))

  shiny::testServer(
    transmissionServer,
    {
      session$setInputs(
        input_source = "catalogue",
        catalogue_collection = "all",
        catalogue_category = "all",
        catalogue_filter = selected[[1L]]
      )
      session$flushReact()
      facade_card <- output$catalogue_info$html
      expect_match(facade_card, "Source version", fixed = TRUE)
      expect_match(facade_card, "0.6.1", fixed = TRUE)
      expect_match(
        facade_card,
        "Measurement and processing details",
        fixed = TRUE
      )
      expect_match(facade_card, "Measurement geometry", fixed = TRUE)
      expect_match(facade_card, "Source measurement points", fixed = TRUE)
      expect_match(facade_card, "Processing for Spectran", fixed = TRUE)

      for (catalogue_id in selected[2:3]) {
        session$setInputs(catalogue_filter = catalogue_id)
        session$flushReact()
        spitschan_card <- output$catalogue_info$html
        expect_match(spitschan_card, "Pinned Git revision", fixed = TRUE)
        expect_false(grepl("Source version", spitschan_card, fixed = TRUE))
        expect_false(grepl("Pinned Git commit", spitschan_card, fixed = TRUE))
        expect_match(
          spitschan_card,
          "3f68efef2101a4eacc4a756e4c44df46dd90a7e5",
          fixed = TRUE
        )
      }
    }
  )

  the$language <- "Deutsch"
  shiny::testServer(
    transmissionServer,
    {
      for (catalogue_id in selected[2:3]) {
        session$setInputs(
          input_source = "catalogue",
          catalogue_collection = "all",
          catalogue_category = "all",
          catalogue_filter = catalogue_id
        )
        session$flushReact()
        german_spitschan_card <- output$catalogue_info$html
        expect_match(
          german_spitschan_card,
          "Fixierter Git-Stand",
          fixed = TRUE
        )
        expect_false(grepl(
          "Quellversion",
          german_spitschan_card,
          fixed = TRUE
        ))
        expect_match(
          german_spitschan_card,
          "Quellpublikation",
          fixed = TRUE
        )
        expect_false(grepl(
          "The source paper's analysis",
          german_spitschan_card,
          fixed = TRUE
        ))
        expect_match(
          german_spitschan_card,
          "Datensatz im Oxford University Research Archive",
          fixed = TRUE
        )
      }
    }
  )
})
