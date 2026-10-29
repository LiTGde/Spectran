test_that("active-spectrum adapters and activation events enforce contracts", {
  source <- transmission_source_fixture("d65")
  active <- new_transmission_active_spectrum(
    spectrum = source,
    name = "CIE D65 at 250 lx",
    origin = "Development fixture",
    revision = 0L,
    change_type = "import",
    node_id = "node-1"
  )

  expect_s3_class(active, "transmission_active_spectrum")
  expect_identical(active$revision, 0L)
  expect_identical(active$change_type, "import")
  expect_equal(active$spectrum, source)
  expect_error(
    new_transmission_active_spectrum(
      source[-1, ],
      "Broken",
      "Test",
      1L,
      "import",
      "node-1"
    ),
    "exactly one row"
  )

  event <- new_transmission_activation_event(
    action_sequence = 1L,
    change_type = "promotion",
    node_id = "node-2",
    parent_id = "node-1",
    spectrum = source,
    name = "Promoted source",
    provenance = list(origin = "Transmission")
  )
  expect_s3_class(event, "transmission_activation_event")

  promoted <- transmission_active_spectrum_from_event(event, revision = 2L)
  expect_identical(promoted$name, "Promoted source")
  expect_identical(promoted$origin, "Transmission")
  expect_identical(promoted$revision, 2L)
  expect_identical(promoted$change_type, "promotion")
  expect_identical(promoted$node_id, "node-2")
})

test_that("promotion, restore, and branching preserve the complete tree", {
  completed <- tibble::tibble(
    wavelength_nm = 380:780,
    transmittance = rep(0.5, 401L),
    status = rep("supplied", 401L)
  )
  make_snapshot <- function(source, source_name, sequence) {
    new_transmission_applied_snapshot(
      result = calculate_transmission_result(source, completed),
      metadata = list(filter_name = "Neutral 50%"),
      incident_name = source_name,
      draft_revision = sequence,
      apply_sequence = sequence
    )
  }

  root_state <- new_transmission_active_spectrum(
    spectrum = transmission_source_fixture("d65"),
    name = "Root source",
    origin = "Development fixture",
    revision = 1L,
    change_type = "import",
    node_id = "node-1"
  )
  history <- new_transmission_history(root_state)
  expect_s3_class(history, "transmission_history")
  expect_identical(history$active_node_id, "node-1")
  expect_equal(nrow(transmission_history_table(history)), 1L)

  first <- transmission_history_promote(
    history,
    make_snapshot(root_state$spectrum, root_state$name, 1L),
    "Root source × Neutral 50%"
  )
  history <- first$history
  expect_identical(first$node$parent_id, "node-1")
  expect_identical(history$active_node_id, "node-2")

  second <- transmission_history_promote(
    history,
    make_snapshot(first$node$spectrum, first$node$name, 2L),
    "Sequential result"
  )
  history <- second$history
  expect_identical(second$node$parent_id, "node-2")
  expect_identical(history$active_node_id, "node-3")

  restored <- transmission_history_restore(history, "node-1")
  history <- restored$history
  expect_identical(history$active_node_id, "node-1")
  expect_length(history$nodes, 3L)

  branch <- transmission_history_promote(
    history,
    make_snapshot(root_state$spectrum, root_state$name, 3L),
    "Alternative branch"
  )
  history <- branch$history
  table <- transmission_history_table(history)

  expect_identical(branch$node$node_id, "node-4")
  expect_identical(branch$node$parent_id, "node-1")
  expect_setequal(table$node_id, paste0("node-", 1:4))
  expect_identical(
    table$parent_id[table$node_id == "node-3"],
    "node-2"
  )
  expect_true(table$active[table$node_id == "node-4"])
  expect_false(any(table$active[table$node_id != "node-4"]))
  expect_equal(nrow(transmission_history_spectra(history)), 4L * 401L)

  restored_branch <- transmission_history_restore(history, "node-3")$history
  expect_length(restored_branch$nodes, 4L)
  expect_identical(restored_branch$active_node_id, "node-3")
})

test_that("a new source import creates a fresh visible root", {
  first <- new_transmission_active_spectrum(
    transmission_source_fixture("d65"),
    "First source",
    "Development fixture",
    1L,
    "import",
    "node-1"
  )
  second <- new_transmission_active_spectrum(
    transmission_source_fixture("equal_energy"),
    "Second source",
    "Development fixture",
    2L,
    "import",
    "node-1"
  )

  old_history <- new_transmission_history(first)
  new_history <- new_transmission_history(second)

  expect_length(old_history$nodes, 1L)
  expect_length(new_history$nodes, 1L)
  expect_identical(new_history$nodes[[1L]]$name, "Second source")
  expect_true(new_history$nodes[[1L]]$active)
  expect_identical(new_history$next_sequence, 2L)
  expect_error(
    new_transmission_history(
      new_transmission_active_spectrum(
        second$spectrum,
        second$name,
        second$origin,
        3L,
        "restore",
        "node-1"
      )
    ),
    "must start from an imported source"
  )
})

test_that("source imports require confirmation only after promotion", {
  root <- new_transmission_active_spectrum(
    transmission_source_fixture("d65"),
    "Root source",
    "Development fixture",
    1L,
    "import",
    "node-1"
  )
  history <- new_transmission_history(root)

  expect_false(transmission_import_requires_confirmation(NULL))
  expect_false(transmission_import_requires_confirmation(history))
  expect_identical(transmission_import_promoted_count(history), 0L)

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
  promoted <- transmission_history_promote(
    history,
    snapshot,
    "Promoted source"
  )$history

  expect_true(transmission_import_requires_confirmation(promoted))
  expect_identical(transmission_import_promoted_count(promoted), 1L)
})

test_that("history choices localize the active-state suffix", {
  old_language <- the$language
  withr::defer(the$language <- old_language)
  root <- new_transmission_active_spectrum(
    transmission_source_fixture("d65"),
    "Norm-Tageslichtspektrum 6500K",
    "Testquelle",
    1L,
    "import",
    "node-1"
  )

  the$language <- "Deutsch"
  labels <- names(transmission_history_choices(
    new_transmission_history(root)
  ))

  expect_match(labels, "(aktiv)", fixed = TRUE)
  expect_false(grepl("(active)", labels, fixed = TRUE))
})
