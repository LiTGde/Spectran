test_that("contextual explanations preserve the return page across topics", {
  active <- shiny::reactiveVal("transmission")
  visits <- character()
  navigate <- function(page) {
    visits <<- c(visits, page)
    active(page)
  }
  shiny::testServer(spectran_explanations_server,
    args = list(active_page = active, navigate = navigate), {
      session$flushReact()
      expect_identical(previous(), "transmission")
      session$setInputs(from_setup_model = 1)
      expect_identical(topic(), "receiver")
      expect_identical(active(), "explanations")
      session$setInputs(topic = "material_spectra")
      session$setInputs(home = 1)
      expect_identical(topic(), "home")
      expect_identical(previous(), "transmission")
      session$setInputs(back = 1)
      expect_identical(active(), "transmission")
      active("analysis")
      session$flushReact()
      session$setInputs(from_quantities = 1)
      expect_identical(topic(), "quantities")
      expect_identical(previous(), "analysis")
      session$setInputs(back = 2)
      expect_identical(active(), "analysis")
    })
  expect_true(all(visits %in% c("transmission", "analysis", "explanations")))
})

test_that("direct help navigation and repeated context links stay coherent", {
  active <- shiny::reactiveVal(NULL)
  shiny::testServer(spectran_explanations_server,
    args = list(active_page = active, navigate = function(page) active(page)), {
      session$flushReact()
      active("import")
      session$flushReact()
      active("explanations")
      session$flushReact()
      expect_identical(previous(), "import")
      expect_identical(topic(), "home")
      session$setInputs(topic_material = 1)
      expect_identical(topic(), "material")
      session$setInputs(topic = "not-a-topic")
      expect_identical(topic(), "material")
      session$setInputs(back = 1)
      expect_identical(active(), "import")
      session$setInputs(from_import = 1)
      expect_identical(topic(), "workflow")
      session$setInputs(back = 2)
      session$setInputs(from_import = 2)
      expect_identical(topic(), "workflow")
      expect_identical(previous(), "import")
    })
})

test_that("the packaged visual explanations are available in both languages", {
  files <- c("01-strahlenwege", "02-spektren", "03-gemeinsamer-lichtweg", "04-material-und-empfaenger",
    "05-spektrum-lesen", "06-beleuchtungsstaerke-edi-der", "07-lichtfarbe-farbwiedergabe",
    "08-alter-und-auge", "09-import-skalierung-export", "10-schritte-und-gesamtwirkung")
  for (language in c("de", "en")) {
    paths <- system.file("app/www/explanations", paste0(files, "-", language, ".svg"), package = "Spectran")
    expect_true(all(file.exists(paths)))
    for (path in paths) {
      svg <- paste(readLines(path, warn = FALSE), collapse = "\n")
      expect_match(svg, "<svg")
      expect_false(grepl("VARIANTE", svg, fixed = TRUE))
    }
  }
})

test_that("help focuses a visible destination and returns to its initiating link", {
  focused <- character()
  local_mocked_bindings(transmission_focus_element = function(id) {
    focused <<- c(focused, id)
  })
  active <- shiny::reactiveVal("transmission")
  shiny::testServer(spectran_explanations_server,
    args = list(active_page = active, navigate = function(page) active(page)), {
      session$flushReact()
      expect_length(focused, 0)
      session$setInputs(from_result_model = 1)
      expect_match(tail(focused, 1), "-heading$")
      session$setInputs(back = 1)
      expect_match(tail(focused, 1), "-from_result_model$")
      session$setInputs(from_result_model = 2)
      # Leaving through the main sidebar must discard the old focus target.
      active("import")
      session$flushReact()
      active("explanations")
      session$flushReact()
      before_back <- focused
      session$setInputs(back = 2)
      expect_identical(active(), "import")
      expect_identical(focused, before_back)
      session$setInputs(from_import = 1)
      expect_match(tail(focused, 1), "-heading$")
      session$setInputs(back = 3)
      expect_match(tail(focused, 1), "-from_import$")
    })
})

test_that("another module can open help and regain focus after topic navigation", {
  focused <- character()
  local_mocked_bindings(transmission_focus_element = function(id) {
    focused <<- c(focused, id)
  })
  active <- shiny::reactiveVal("tutorial")
  shiny::testServer(spectran_explanations_server,
    args = list(active_page = active, navigate = function(page) active(page)), {
      session$flushReact()
      help <- session$getReturned()
      help$open_from("home", "tutorial", "intro-to_explanations")
      session$flushReact()
      expect_identical(active(), "explanations")
      session$setInputs(topic_path = 1)
      session$setInputs(back = 1)
      expect_identical(active(), "tutorial")
      expect_identical(tail(focused, 1), "intro-to_explanations")
      help$open_from("home", "tutorial", "intro-to_explanations")
      session$flushReact()
      expect_identical(topic(), "home")
      # A sidebar visit clears the introduction return target as usual.
      active("import")
      session$flushReact()
      active("explanations")
      session$flushReact()
      before_back <- focused
      session$setInputs(back = 2)
      expect_identical(active(), "import")
      expect_identical(focused, before_back)
    })
})

test_that("sequential reading follows the topic order without wrapping at the ends", {
  active <- shiny::reactiveVal("explanations")
  shiny::testServer(spectran_explanations_server,
    args = list(active_page = active, navigate = function(page) active(page)), {
      session$flushReact()
      expect_null(output$topic_navigation)
      # Stale clicks outside a topic must not start a reading sequence.
      session$setInputs(next_topic = 1)
      expect_identical(topic(), "home")
      session$setInputs(topic = "spectrum")
      session$setInputs(previous_topic = 1)
      expect_identical(topic(), "spectrum")
      expect_match(output$topic_navigation$html, "disabled")
      ordered_topics <- names(spectran_explanation_topics())
      for (target in ordered_topics[-1L]) {
        before <- topic()
        # Dynamic buttons remount at zero, which is not a user click.
        session$setInputs(next_topic = 0)
        expect_identical(topic(), before)
        session$setInputs(next_topic = 1)
        expect_identical(topic(), target)
        if (!identical(target, "path")) expect_false(grepl("disabled", output$topic_navigation$html, fixed = TRUE))
      }
      session$setInputs(next_topic = 2)
      expect_identical(topic(), "path")
      expect_match(output$topic_navigation$html, "disabled")
      for (target in rev(ordered_topics[-length(ordered_topics)])) {
        before <- topic()
        session$setInputs(previous_topic = 0)
        expect_identical(topic(), before)
        session$setInputs(previous_topic = 1)
        expect_identical(topic(), target)
      }
      session$setInputs(previous_topic = 2)
      expect_identical(topic(), "spectrum")
      session$setInputs(home = 1)
      expect_null(output$topic_navigation)
    })
})

test_that("sequential reading starts at the selected topic and retains its return link", {
  focused <- character()
  local_mocked_bindings(transmission_focus_element = function(id) {
    focused <<- c(focused, id)
  })
  active <- shiny::reactiveVal("transmission")
  shiny::testServer(spectran_explanations_server,
    args = list(active_page = active, navigate = function(page) active(page)), {
      session$flushReact()
      session$setInputs(from_setup_model = 1)
      session$setInputs(next_topic = 1)
      expect_identical(topic(), "path")
      expect_match(tail(focused, 1), "-heading$")
      session$setInputs(topic = "workflow")
      session$setInputs(next_topic = 2)
      expect_identical(topic(), "material")
      session$setInputs(previous_topic = 1)
      expect_identical(topic(), "workflow")
      expect_identical(previous(), "transmission")
      session$setInputs(back = 1)
      expect_identical(active(), "transmission")
      expect_match(tail(focused, 1), "-from_setup_model$")
    })
})
