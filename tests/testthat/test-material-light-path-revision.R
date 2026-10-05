path_test_snapshot <- function(source, coefficient = .5, mode = "transmission", sequence = 1L) {
  filter <- tibble::tibble(wavelength_nm = 380:780,
    transmittance = rep_len(coefficient, 401L), status = "supplied")
  new_transmission_applied_snapshot(calculate_material_result(source, filter, mode),
    metadata = list(filter_name = "Material", material_mode = mode), incident_name = "Source",
    draft_revision = sequence, apply_sequence = sequence)
}

test_that("light-level scaling reaches the selected response without changing shape", {
  source <- transmission_source_fixture("equal_energy")
  original <- source
  for (metric in c("photopic", "melanopic")) {
    scaled <- material_scale_light(source, 125, metric)
    expect_equal(material_light_level(scaled, metric), 125, tolerance = 1e-10)
    expect_equal(scaled$Bestrahlungsstaerke / source$Bestrahlungsstaerke,
      rep(125 / material_light_level(source, metric), 401))
    expect_true(all(material_scale_light(source, 0, metric)$Bestrahlungsstaerke == 0))
  }
  expect_identical(source, original)
  expect_error(material_scale_light(source, -1))
  expect_error(material_scale_light(source, Inf))
  expect_error(material_scale_light(source, NA_real_))
  expect_error(material_scale_light(source, c(1, 2)))
  expect_error(material_scale_light(transmission_source_fixture("zero"), 1, "melanopic"))
  expect_equal(material_scale_light(transmission_source_fixture("zero"), 0), transmission_source_fixture("zero"))
  raw <- examplespectra$CIE[c("Wellenlaenge", "A")]
  scaled <- material_scale_source_data(raw, 180, "melanopic")
  visible <- tibble::tibble(Wellenlaenge = 380:780,
    Bestrahlungsstaerke = stats::approx(scaled[[1]], scaled[[2]], xout = 380:780)$y)
  expect_equal(material_light_level(visible, "melanopic"), 180, tolerance = 1e-10)
})

test_that("saving preserves the active source and path plots exclude sibling branches", {
  root <- new_transmission_active_spectrum(transmission_source_fixture("d65"), "Start", "Test", 1L, "import", "node-1")
  history <- new_transmission_history(root)
  first <- transmission_history_promote(history, path_test_snapshot(root$spectrum), "Glass", activate = FALSE)
  expect_identical(first$history$active_node_id, "node-1")
  expect_true(first$history$nodes[["node-1"]]$active)
  expect_false(first$node$active)
  expect_equal(first$node$spectrum$Bestrahlungsstaerke, root$spectrum$Bestrahlungsstaerke * .5)
  branch <- transmission_history_promote(first$history, path_test_snapshot(root$spectrum, .8, sequence = 2L), "Sibling")
  restored <- transmission_history_restore(branch$history, "node-2")
  last <- transmission_history_promote(restored$history,
    path_test_snapshot(first$node$spectrum, .25, "reflection", 3L), "Wall")
  data <- material_path_spectra(last$history, last$node$node_id)
  expect_identical(unique(data$node_id), c("node-1", "node-2", "node-4"))
  expect_equal(data$irradiance_w_m2_nm[data$node_id == "node-4"], root$spectrum$Bestrahlungsstaerke * .125)
  expect_false(any(data$light_level_adjusted))
  plot <- material_path_plot(last$history, last$node$node_id)
  built <- ggplot2::ggplot_build(plot)
  lines <- built$data[[length(built$data)]]
  expect_setequal(unique(lines$linetype), c("longdash", "dashed", "solid"))
  expect_equal(nrow(lines), 3L * 401L)
  file <- tempfile(fileext = ".png")
  withr::defer(unlink(file))
  expect_no_error(write_material_path_plot(last$history, last$node$node_id, file))
  expect_gt(file.info(file)$size, 10000)
})

test_that("saved adjusted spectra retain their level and adjustment flag in plot data", {
  root <- new_transmission_active_spectrum(transmission_source_fixture("equal_energy"), "Start", "Test", 1L, "import", "node-1")
  snapshot <- path_test_snapshot(root$spectrum)
  target <- material_photopic_lux(material_scale_light(snapshot$transmitted_spectrum, 200, "melanopic"))
  saved <- transmission_history_promote(new_transmission_history(root), snapshot, "Adjusted", target_lux = target, activate = FALSE)
  expect_equal(material_light_level(saved$node$spectrum, "melanopic"), 200, tolerance = 1e-10)
  data <- material_path_spectra(saved$history, saved$node$node_id)
  expect_true(all(data$light_level_adjusted[data$node_id == saved$node$node_id]))
  expect_true(material_cumulative_has_rescaling(calculate_material_cumulative(saved$history, saved$node$node_id)))
})

test_that("single source previews and long path names keep their display meaning", {
  root <- new_transmission_active_spectrum(transmission_source_fixture("d65"),
    "Saved wall spectrum", "Preview", 0L, "import", "node-4")
  history <- new_transmission_history(root)
  single <- material_path_plot(history)
  expect_identical(single$scales$get_scales("colour")$labels, "Saved wall spectrum")
  expect_null(single$labels$caption)
  name <- paste(rep("UnbrokenSourceName", 10), collapse = "")
  next_step <- transmission_history_promote(history, path_test_snapshot(root$spectrum), name)
  device <- grDevices::dev.cur()
  plot <- material_path_plot(next_step$history, font_size = 10, label_width = 32, width_px = 240)
  expect_identical(grDevices::dev.cur(), device)
  labels <- plot$scales$get_scales("colour")$labels
  expect_lte(max(nchar(unlist(strsplit(labels, "\n", fixed = TRUE)))), 32)
  expect_match(gsub("\n", "", labels[[2L]], fixed = TRUE), name, fixed = TRUE)
})

test_that("workspace save and source-restore cross explicit module boundaries", {
  root <- new_transmission_active_spectrum(transmission_source_fixture("d65"), "Start", "Test", 1L, "import", "node-1")
  active <- shiny::reactiveVal(root)
  shiny::testServer(transmissionServer, args = list(workspace = TRUE,
    incident_spectrum = shiny::reactive(active()$spectrum), incident_name = shiny::reactive(active()$name),
    active_state = shiny::reactive(active())), {
    session$setInputs(input_source = "catalogue", material_mode = "transmission")
    session$setInputs(`apply-apply_filter` = 1L)
    session$flushReact()
    returned <- session$getReturned()
    expect_true(returned$can_promote())
    session$setInputs(`history-promotion_name` = "Saved glass", `history-save_result` = 1L)
    session$flushReact()
    expect_identical(returned$history()$active_node_id, "node-1")
    expect_length(returned$history()$nodes, 2L)
    expect_null(returned$promotion_event())
    expect_identical(returned$saved_event()$node_id, "node-2")
    expect_false(returned$can_promote())
    session$setInputs(`history-save_result` = 2L)
    expect_length(returned$history()$nodes, 2L)
    returned$restore_node("node-2")
    session$flushReact()
    expect_identical(returned$restore_event()$node_id, "node-2")
    expect_identical(returned$history()$active_node_id, "node-2")
    expect_length(returned$history()$nodes, 2L)
  })
})

test_that("the workspace source picker imports the requested melanopic EDI", {
  old_language <- the$language
  withr::defer(the$language <- old_language)
  the$language <- "English"
  committed <- NULL
  restored <- NULL
  root <- new_transmission_active_spectrum(transmission_source_fixture("d65"), "Start", "Test", 1L, "import", "node-1")
  saved <- transmission_history_promote(new_transmission_history(root), path_test_snapshot(root$spectrum), "Glass", activate = FALSE)
  source_history <- shiny::reactiveVal(NULL)
  shiny::testServer(material_source_server, args = list(current = shiny::reactive(root), history = source_history,
    on_import = function(request) committed <<- request, on_restore = function(node) restored <<- node), {
    session$flushReact()
    session$setInputs(`picker-examples-illu_eigen` = 150, `picker-examples-level_metric` = "melanopic", `picker-examples-CCT_norm` = 5000)
    session$setInputs(`picker-examples-norm-norm` = 1L)
    session$flushReact()
    expect_equal(material_light_level(committed$spectrum, "melanopic"), 150, tolerance = 1e-10)
    expect_null(restored)
    source_history(saved$history)
    session$setInputs(path_source = "node-2", use_path_source = 1L)
    expect_identical(restored, "node-2")
  })
})
