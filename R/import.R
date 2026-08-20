# UI ----------------------------------------------------------------------

importUI <- function(
  id
) {
  ns <- shiny::NS(id)
  htmltools::tags$div(
    class = "spectran-import-page",
    shiny::withMathJax(),
    htmltools::h3(id = ns("heading"), tabindex = "-1", "Import"),
    htmltools::p(
      class = "spectran-import-destination-note",
      transmission_text("import_unlocks_transmission")
    ),
    shiny::tabsetPanel(
      id = ns("inTabset"),
      shiny::tabPanel(
        title = lang$ui(69),
        import_dataUI(shiny::NS(id, "fileimport"))
      ),
      shiny::tabPanel(
        title = lang$ui(93),
        import_examplesUI(shiny::NS(id, "examples"))
      ),
      shiny::tabPanel(
        title = lang$ui(94),
        import_eigenUI(shiny::NS(id, "eigen"))
      )
    ),
    transmissionSourceImportUI(ns("history_reset"))
  )
}

# Server ------------------------------------------------------------------

importServer <-
  function(id, Spectrum = NULL, transmission_history = NULL) {
    if (!is.null(transmission_history)) {
      stopifnot(shiny::is.reactive(transmission_history))
    }
    shiny::moduleServer(id, function(input, output, session) {
      #Set up a container for the spectra to go into, if it isn´t already defined
      if (is.null(Spectrum)) {
        Spectrum <-
          shiny::reactiveValues(
            Spectrum = NULL,
            Spectrum_raw = NULL,
            Name = NULL,
            Origin = NULL,
            Destination = NULL,
            Illu = NULL,
            Analysis = 0L,
            revision = 0L,
            change_type = NULL,
            node_id = NULL,
            provenance = list()
          )
      }
      initialize_spectran_spectrum_state(Spectrum)

      if (!is.null(transmission_history)) {
        perform_import <- function(request) {
          activate_spectran_spectrum(
            Spectrum = Spectrum,
            spectrum = request$spectrum,
            name = request$source_name,
            origin = request$origin %||% "Import",
            change_type = "import",
            node_id = "node-1",
            provenance = request$provenance %||% list(),
            destination = request$destination %||% lang$ui(69)
          )
          show_import_verifier_alert(request$notification)
          invisible(NULL)
        }
        history_reset <- transmissionSourceImportServer(
          "history_reset",
          history = transmission_history,
          perform_import = perform_import,
          return_focus_id = session$ns("heading")
        )
        Spectrum$import_guard <- function(request) {
          history_reset$request(request, "click")
          invisible(NULL)
        }
      }

      import_verifierServer("verify_import", Spectrum = Spectrum)
      import_dataServer("fileimport", Spectrum = Spectrum)
      import_examplesServer("examples", Spectrum = Spectrum)
      import_eigenServer("eigen", Spectrum = Spectrum)

      # The legacy file-import validator uses global notification IDs. Clear
      # any stale file-only messages after activating a non-file source. The
      # low priority lets the legacy observers finish first in the same flush.
      shiny::observeEvent(
        list(Spectrum$revision, Spectrum$Origin),
        {
          if (
            identical(Spectrum$change_type, "import") &&
              !identical(Spectrum$Origin, "File")
          ) {
            for (notification_id in c(
              "is_sufficient",
              "is_integer",
              "is_numeric",
              "belowz",
              "success"
            )) {
              shiny::removeNotification(notification_id)
            }
          }
        },
        ignoreInit = TRUE,
        priority = -1000
      )

      #Update the Navbar when a Spectrum is imported
      shiny::observe({
        shiny::updateNavbarPage(
          session,
          inputId = "inTabset",
          selected = Spectrum$Destination
        )
      }) %>%
        shiny::bindEvent(Spectrum$Spectrum, Spectrum$Destination)

      #remove notifications
      notification_remover(shiny::reactive(input$inTabset))

      #Return value
      Spectrum
    })
  }

# App ---------------------------------------------------------------------

importApp <- function(lang_setting = "Deutsch") {
  #add a resource path to the www folder
  shiny::addResourcePath(
    "extr",
    system.file("app/www", package = "Spectran")
  )
  # on.exit(shiny::removeResourcePath("extr"), add = TRUE)

  #set the language for the program
  the$language <- lang_setting

  ui <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(),
    shinydashboard::dashboardSidebar(),
    shinydashboard::dashboardBody(
      shiny::verbatimTextOutput("Data_ok"),
      importUI("import")
    )
  )

  server <- function(input, output, session) {
    Spectrum <- importServer("import")

    output$Data_ok <- shiny::renderPrint({
      print("Developer Troubleshoot\n")
      print(Spectrum$Name)
      print(Spectrum$Destination)
      print(Spectrum$Other)
      print(Spectrum$Spectrum %>% utils::head())
      print(Spectrum$Spectrum %>% utils::tail())
    })
  }
  shiny::shinyApp(ui, server)
}
