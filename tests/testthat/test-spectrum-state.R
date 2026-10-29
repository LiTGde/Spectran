test_that("central activation records revisions and committed state", {
  old_language <- if (exists("language", envir = the, inherits = FALSE)) {
    the$language
  } else {
    NULL
  }
  withr::defer({
    if (is.null(old_language)) {
      rm("language", envir = the)
    } else {
      the$language <- old_language
    }
  })
  the$language <- "English"
  state <- shiny::reactiveValues()
  source <- transmission_source_fixture("d65")

  shiny::isolate(activate_spectran_spectrum(
    Spectrum = state,
    spectrum = source,
    name = "CIE D65 at 250 lx",
    origin = "Test import",
    change_type = "import",
    node_id = "node-1",
    provenance = list(record = "fixture")
  ))

  shiny::isolate({
    expect_identical(state$revision, 1L)
    expect_identical(state$change_type, "import")
    expect_identical(state$node_id, "node-1")
    expect_identical(state$Analysis, 1L)
    expect_equal(state$Spectrum, source)
    expect_equal(state$committed_state$Spectrum, source)
    expect_identical(state$committed_state$Name, "CIE D65 at 250 lx")
    expect_identical(state$import_attempt, 0L)
  })

  event <- new_transmission_activation_event(
    action_sequence = 1L,
    change_type = "promotion",
    node_id = "node-2",
    parent_id = "node-1",
    spectrum = source,
    name = "Promoted spectrum",
    provenance = list(filter = "Neutral 50%")
  )
  shiny::isolate(activate_spectran_transmission_event(state, event))

  shiny::isolate({
    expect_identical(state$revision, 2L)
    expect_identical(state$change_type, "promotion")
    expect_identical(state$node_id, "node-2")
    expect_identical(state$Analysis, 2L)
    expect_identical(state$committed_state$Name, "Promoted spectrum")
  })
})

test_that("legacy file validation is inactive for non-file sources", {
  invalid_file_shape <- shiny::reactive(data.frame(only = 1:3))
  active <- shiny::reactiveVal(FALSE)
  settings <- shiny::reactive(list(
    x_y = 1L,
    x_y2 = 2L,
    row_nr = 0L,
    multiplikator = 1
  ))

  shiny::testServer(
    import_data_checkServer,
    args = list(
      dat = invalid_file_shape,
      dat0 = invalid_file_shape,
      importfile = shiny::reactive("not-used.csv"),
      csv_settings = settings,
      active = active
    ),
    {
      session$flushReact()
      expect_false(session$getReturned()$x)
    }
  )
})

test_that("file import controls do not claim origin during initialization", {
  old_language <- if (exists("language", envir = the, inherits = FALSE)) {
    the$language
  } else {
    NULL
  }
  withr::defer({
    if (is.null(old_language)) {
      rm("language", envir = the)
    } else {
      the$language <- old_language
    }
  })
  the$language <- "English"
  state <- shiny::reactiveValues(
    Spectrum = NULL,
    Spectrum_raw = NULL,
    Name = "Non-file source",
    Origin = "Example",
    Destination = lang$ui(69)
  )

  shiny::testServer(
    import_dataServer,
    args = list(Spectrum = state),
    {
      session$flushReact()
      expect_identical(shiny::isolate(state$Origin), "Example")
    }
  )
})

test_that("a guarded import restores committed state and freezes its request", {
  old_language <- if (exists("language", envir = the, inherits = FALSE)) {
    the$language
  } else {
    NULL
  }
  withr::defer({
    if (is.null(old_language)) {
      rm("language", envir = the)
    } else {
      the$language <- old_language
    }
  })
  the$language <- "English"
  state <- shiny::reactiveValues()
  source <- transmission_source_fixture("d65")
  captured <- NULL

  shiny::isolate(activate_spectran_spectrum(
    Spectrum = state,
    spectrum = source,
    name = "Committed source",
    origin = "Test import",
    change_type = "import",
    node_id = "node-1"
  ))
  shiny::isolate({
    state$import_guard <- function(request) {
      captured <<- request
      invisible(NULL)
    }
  })

  shiny::testServer(
    import_verifierServer,
    args = list(Spectrum = state),
    {
      session$flushReact()
      state$Name <- "Pending source"
      state$Origin <- "Pending import"
      state$Destination <- lang$ui(69)
      state$Spectrum_raw <- tibble::tibble(
        wavelength = 380:780,
        irradiance = rep(0.25, 401L)
      )
      signal_spectran_import_attempt(state)
      session$flushReact()

      expect_type(captured, "list")
      expect_identical(captured$source_name, "Pending source")
      expect_identical(captured$origin, "Pending import")
      expect_equal(nrow(captured$spectrum), 401L)
      expect_true(all(captured$spectrum$Bestrahlungsstaerke == 0.25))
      expect_identical(state$Name, "Committed source")
      expect_identical(state$Origin, "Test import")
      expect_equal(state$Spectrum, source)
      expect_identical(state$revision, 1L)
    }
  )
})

test_that("an identical source can request confirmation again after cancel", {
  old_language <- the$language
  withr::defer(the$language <- old_language)
  the$language <- "English"
  state <- shiny::reactiveValues()
  source <- transmission_source_fixture("d65")
  captured <- list()

  shiny::isolate(activate_spectran_spectrum(
    Spectrum = state,
    spectrum = source,
    name = "Committed source",
    origin = "Test import",
    change_type = "import",
    node_id = "node-1"
  ))
  shiny::isolate({
    state$import_guard <- function(request) {
      captured[[length(captured) + 1L]] <<- request
      invisible(NULL)
    }
  })

  shiny::testServer(
    import_verifierServer,
    args = list(Spectrum = state),
    {
      session$flushReact()
      for (attempt in 1:2) {
        state$Name <- "Repeated source"
        state$Origin <- "Example"
        state$Destination <- lang$ui(69)
        state$Spectrum_raw <- tibble::tibble(
          wavelength = 380:780,
          irradiance = rep(0.25, 401L)
        )
        signal_spectran_import_attempt(state)
        session$flushReact()
      }

      expect_length(captured, 2L)
      expect_identical(captured[[1L]]$source_name, "Repeated source")
      expect_identical(captured[[2L]]$source_name, "Repeated source")
      expect_identical(state$Name, "Committed source")
      expect_identical(state$revision, 1L)
    }
  )
})

test_that("import requests retain frozen spectrum and audit fields", {
  request <- list(
    source_id = "source-2",
    source_name = "New source",
    spectrum = transmission_source_fixture("equal_energy"),
    origin = "Example",
    destination = "Analysis",
    provenance = list(record = "example"),
    notification = list(title = "OK")
  )

  normalized <- as_transmission_import_request(request)

  expect_identical(normalized$source_id, request$source_id)
  expect_identical(normalized$source_name, request$source_name)
  expect_identical(normalized$spectrum, request$spectrum)
  expect_identical(normalized$provenance, request$provenance)
  expect_identical(normalized$notification, request$notification)
})
