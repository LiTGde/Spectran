test_that("material measurement metadata is optional, editable and tied to its result", {
  old_language <- the$language
  withr::defer(the$language <- old_language)
  the$language <- "English"
  records <- material_catalogue_records_data("transmission")
  tub <- records$catalogue_id[records$catalogue == "tub67600"][[1]]
  facade <- records$catalogue_id[records$catalogue == "facade_windows"][[1]]
  source <- transmission_source_fixture("d65")
  shiny::testServer(transmissionServer, args = list(
    incident_spectrum = shiny::reactive(source), incident_name = shiny::reactive("Source")), {
    session$setInputs(input_source = "catalogue", catalogue_collection = "tub67600",
      catalogue_filter = tub, material_mode = "transmission")
    session$setInputs(`apply-apply_filter` = 1L)
    session$flushReact()
    result <- session$getReturned()
    snapshot <- result$applied_snapshot()
    expect_match(snapshot$metadata$measurement_instrument, "OMEGA 20", fixed = TRUE)
    expect_identical(snapshot$metadata$relative_measurement_error, "")
    session$setInputs(measurement_instrument = "Test meter; 2025", relative_measurement_error = "2% (test)")
    expect_false(result$can_download())
    expect_identical(snapshot$metadata$relative_measurement_error, "")
    session$setInputs(`apply-apply_filter` = 2L)
    expect_identical(result$applied_snapshot()$metadata$measurement_instrument, "Test meter; 2025")
    expect_identical(result$applied_snapshot()$metadata$relative_measurement_error, "2% (test)")
    session$setInputs(catalogue_collection = "facade_windows", catalogue_filter = facade)
    session$setInputs(`apply-apply_filter` = 3L)
    expect_identical(result$applied_snapshot()$metadata$measurement_instrument, "")
    expect_identical(result$applied_snapshot()$metadata$relative_measurement_error, "")
    expect_true(result$can_promote())
  })
})

test_that("visible provenance follows catalogue and uploaded material switches", {
  old_language <- the$language
  withr::defer(the$language <- old_language)
  the$language <- "English"
  source <- transmission_source_fixture("d65")
  file <- tempfile(fileext = ".csv")
  withr::defer(unlink(file))
  readr::write_csv(data.frame(wavelength_nm = 380:780, value = .5), file)
  records <- material_catalogue_records_data("transmission")
  tub <- records$catalogue_id[records$catalogue == "tub67600"][[1]]
  shiny::testServer(transmissionServer, args = list(
    incident_spectrum = shiny::reactive(source), workspace = TRUE), {
    session$setInputs(input_source = "catalogue", catalogue_collection = "tub67600",
      catalogue_filter = tub, material_mode = "transmission")
    expect_match(output$catalogue_info$html, "Rudawski et al. (2022)", fixed = TRUE)
    session$setInputs(input_source = "upload", filter_file = list(
      name = "own-material.csv", datapath = file, size = file.info(file)$size, type = "text/csv"))
    session$setInputs(measurement_instrument = "Review meter; 2025", relative_measurement_error = "2 %")
    for (mode in c("transmission", "reflection")) {
      session$setInputs(material_mode = mode)
      visible <- output$catalogue_info$html
      expect_match(visible, "own-material.csv", fixed = TRUE)
      expect_match(visible, "Review meter; 2025", fixed = TRUE)
      expect_match(visible, "2 %", fixed = TRUE)
      expect_false(grepl("OMEGA|Rudawski|G1", visible))
    }
    session$setInputs(material_mode = "transmission", input_source = "catalogue")
    expect_match(output$catalogue_info$html, "OMEGA 20", fixed = TRUE)
    expect_false(grepl("own-material.csv|Review meter", output$catalogue_info$html))
  })
})
