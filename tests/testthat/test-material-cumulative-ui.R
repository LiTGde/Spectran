for (locale in c("Deutsch", "English")) {
  test_that(
    paste("rescaled cumulative tables become a recoverable notice in", locale),
    {
      old_language <- the$language
      withr::defer(the$language <- old_language)
      the$language <- locale
      source <- transmission_source_fixture("d65", target_lux = 200)
      root <- new_transmission_active_spectrum(
        source,
        "Root",
        "Test",
        1L,
        "import",
        "node-1"
      )
      active <- shiny::reactiveVal(root)
      shiny::testServer(
        transmissionServer,
        args = list(
          fixture_data = shiny::reactive(transmission_fixture("neutral")),
          incident_spectrum = shiny::reactive(active()$spectrum),
          incident_name = shiny::reactive(active()$name),
          active_state = active
        ),
        {
          session$flushReact()
          session$setInputs(
            filter_name = "Wall",
            scale = "fraction",
            transmittance_type = "total",
            scattering = "no"
          )
          session$flushReact()
          session$setInputs(`apply-apply_filter` = 1L)
          session$flushReact()
          session$setInputs(
            `history-promotion_lux` = 300,
            `history-promotion_name` = "Scaled",
            `history-promote` = 1L
          )
          session$flushReact()
          returned <- session$getReturned()
          expect_identical(returned$history()$active_node_id, "node-2")
          session$setInputs(
            `history-node_action` = list(node = "node-2", action = "show")
          )
          session$flushReact()
          html <- output[["history-cumulative_summary"]]$html
          expect_match(html, material_text("cumulative_rescaled"), fixed = TRUE)
          expect_false(grepl(
            "cumulative_material|cumulative_actual|cumulative_csv",
            html
          ))
          session$setInputs(
            `history-node_action` = list(node = "node-1", action = "show")
          )
          session$flushReact()
          expect_identical(returned$history()$active_node_id, "node-2")
          expect_identical(returned$archive_node_id(), "node-1")
          expect_null(returned$archived_snapshot())
          html <- output[["history-cumulative_summary"]]$html
          expect_match(html, "cumulative_material", fixed = TRUE)
          expect_false(grepl("cumulative_actual", html, fixed = TRUE))
          expect_match(html, "cumulative_csv", fixed = TRUE)
          metrics <- output[["history-cumulative_material"]]$html
          expect_false(grepl(
            "Wirkfaktor|action factor",
            metrics,
            ignore.case = TRUE
          ))
          expect_match(metrics, "DIN/TS 67600:2022-08", fixed = TRUE)
          session$setInputs(
            `history-node_action` = list(node = "node-2", action = "show")
          )
          session$flushReact()
          expect_identical(returned$archive_node_id(), "node-2")
          expect_s3_class(
            returned$archived_snapshot(),
            "transmission_applied_snapshot"
          )
          expect_match(
            output[["history-cumulative_summary"]]$html,
            material_text("cumulative_rescaled"),
            fixed = TRUE
          )
        }
      )
    }
  )
}
