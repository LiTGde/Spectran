#' Unlock the Spectrum: Easy, Educational, and Engaging Analysis of Light Spectra
#'
#' @param lang_setting A language for the application. Currently **Deutsch** for German and **English** (default) are implemented. Expects a *character*.
#' @param lang_link Only relevant for the App deployed on *Shinyapps.io*. Handles whether a link to the German/English Version of the App is present in the header. Expects a *logical* (default FALSE)
#' @param color_palette A color palette for the application. Currently `**Lang**` (default), `**Lang_bright**`, `**Dan_Bruton**`, and `**Rainbow**` are implemented. Expects a `*character*`. In terms of `color accuracy`, the decending order is likely `**Dan_Bruton**`, `**Lang**`, `**Lang_bright**`, and `**Rainbow**`. However, all of them are wrong in the sense, that monochromatic light can not well be recreated with RGB colors. Look at the documentation for [ColorP] for more information about these palettes.
#' @param ... Any other settings that get passed to shinyApp
#'
#' @return Open a viewer with the shiny app
#' @export
#'
#' @examples
#' if(interactive()) {
#' Spectran()}
#'
#' #try another language
#' if(interactive()) {
#' Spectran(lang_setting = "Deutsch")}
#'
#' #or try another color palette
#' if(interactive()) {
#' Spectran(color_palette = "Dan_Bruton")}
#'
Spectran <- function(
  lang_setting = "English",
  lang_link = FALSE,
  color_palette = "Lang",
  ...
) {
  #add a resource path to the www folder
  shiny::addResourcePath(
    "extr",
    system.file("app/www", package = "Spectran")
  )
  # on.exit(shiny::removeResourcePath("extr"), add = TRUE)

  #set the language and color palette for the program
  the$language <- lang_setting
  the$palette <- color_palette
  review_build_id <- getOption("Spectran.review_build_id", NULL)

  #create an Environment that holds the plotwidths of users
  theuser <- new.env(parent = emptyenv())
  theuser$Plotbreite_temp <- 200

  #UI
  ui <-
    shinydashboard::dashboardPage(
      skin = "yellow",
      #Header
      UI_Header(lang_link),
      #Sidebar
      UI_Sidebar(),
      #Body
      shinydashboard::dashboardBody(
        #Add a link to the css resource
        htmltools::tags$link(
          rel = "stylesheet",
          type = "text/css",
          href = "extr/style.css"
        ),
        if (
          is.character(review_build_id) &&
            length(review_build_id) == 1L &&
            !is.na(review_build_id) &&
            nzchar(review_build_id)
        ) {
          htmltools::tags$aside(
            class = "spectran-review-build",
            role = "status",
            htmltools::tags$strong("Review build: "),
            review_build_id
          )
        },
        shinydashboard::tabItems(
          #add a tab for the introduction
          shinydashboard::tabItem(
            tabName = "tutorial",
            introductionUI("intro")
          ),
          #add a tab for the import
          shinydashboard::tabItem(tabName = "import", importUI("import")),
          #add a tab for the analysis
          shinydashboard::tabItem(tabName = "analysis", analysisUI("analysis")),
          #add a tab for the export
          shinydashboard::tabItem(tabName = "export", exportUI("export")),
          #add the optional transmission-filter tab after export
          shinydashboard::tabItem(
            tabName = "transmission",
            transmissionUI(
              "transmission",
              default_source = "catalogue",
              layout = "tabs"
            )
          ),
          #add a tab for the validity
          shinydashboard::tabItem(tabName = "validity", validityUI("validity")),
          #add a tab for the impressum
          shinydashboard::tabItem(
            tabName = "impressum",
            impressumUI("impressum")
          )
        ),
        shiny::fluidPage(
          (shiny::plotOutput("Plotbreite", height = "1px"))
        ),
        waiter::useWaiter(),
        # waiter::waiterOnBusy(html = waiter::spin_solar(),
        #                      color = "#2874A625", fadeout = 100),
        waiter::autoWaiter(
          html = waiter::spin_solar(),
          color = "#2874A625",
          fadeout = 100
        ),
        waiter::waiterPreloader(
          html = waiter::spin_solar(),
          color = "#2874A625",
          fadeout = 500
        )
      )
    )

  #Server
  server <- function(input, output, session) {
    #allow reconnect
    session$allowReconnect(TRUE)

    #Introduction
    zu_Import <- introductionServer("intro")

    #Shared active-spectrum state
    Spectrum <- shiny::reactiveValues()
    initialize_spectran_spectrum_state(Spectrum)

    #Transmission filters
    Transmission <- transmissionServer(
      "transmission",
      incident_spectrum = shiny::reactive(Spectrum$Spectrum),
      incident_name = shiny::reactive(Spectrum$Name),
      active_state = shiny::reactive(
        spectran_transmission_active_state(Spectrum)
      )
    )

    #Import. Source changes are guarded when promoted history exists.
    Spectrum <- importServer(
      "import",
      Spectrum = Spectrum,
      transmission_history = Transmission$history
    )

    last_transmission_activation <- shiny::reactiveVal(0L)
    activate_transmission_event <- function(event) {
      if (
        is.null(event) ||
          event$action_sequence <= last_transmission_activation()
      ) {
        return(invisible(NULL))
      }
      activate_spectran_transmission_event(Spectrum, event)
      last_transmission_activation(event$action_sequence)
      invisible(NULL)
    }
    shiny::observeEvent(
      Transmission$promotion_event(),
      {
        activate_transmission_event(Transmission$promotion_event())
        shinydashboard::updateTabItems(
          session,
          inputId = "inTabset",
          selected = "analysis"
        )
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )
    shiny::observeEvent(
      Transmission$restore_event(),
      activate_transmission_event(Transmission$restore_event()),
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    #Analysis
    Analysis <- analysisServer(
      "analysis",
      Spectrum = Spectrum,
      Tabactive = shiny::reactive(input$inTabset)
    )

    #Export
    Export <- exportServer(
      "export",
      Analysis,
      Spectrum,
      Tabactive = shiny::reactive(input$inTabset)
    )

    output$Plotbreite <- shiny::renderPlot(
      {
      },
      bg = "transparent"
    )
    # bg = "white")

    # Delete Notifications between tab changes
    notification_remover(shiny::reactive(input$inTabset))

    #Update the Navbar, when the Introduction is finished
    shiny::observe({
      shinydashboard::updateTabItems(
        session,
        inputId = "inTabset",
        selected = "import"
      )
    }) %>%
      shiny::bindEvent(
        zu_Import(),
        ignoreInit = TRUE
      )

    #Enable/disable spectrum-dependent menus when no source is active
    output$transmission <- shinydashboard::renderMenu({
      if (!is.null(Spectrum$Spectrum)) {
        shinydashboard::menuItem(
          transmission_text("menu"),
          tabName = "transmission",
          icon = shiny::icon("filter")
        )
      } else {
        shinydashboard::menuItem(
          htmltools::HTML(
            paste0(
              "<span style='color:grey;'>",
              transmission_text("menu"),
              "</span>"
            )
          ),
          tabName = "import",
          icon = shiny::icon("lock")
        )
      }
    })
    output$analysis <- shinydashboard::renderMenu({
      if (!is.null(Analysis$Settings$Spectrum)) {
        shinydashboard::menuItem(
          lang$ui(23),
          tabName = "analysis",
          icon = shiny::icon(
            "magnifying-glass-chart"
          )
        )
      } else {
        shinydashboard::menuItem(
          htmltools::HTML(
            paste0("<span style='color:grey;'>", lang$ui(23), "</span>")
          ),
          tabName = "import",
          icon = shiny::icon(
            "lock"
          )
        )
      }
    })
    output$export <- shinydashboard::renderMenu({
      if (!is.null(Analysis$Settings$Spectrum)) {
        shinydashboard::menuItem(
          "Export",
          tabName = "export",
          icon = shiny::icon("file-export")
        )
      } else {
        shinydashboard::menuItem(
          htmltools::HTML(
            paste0("<span style='color:grey;'>", "Export", "</span>")
          ),
          tabName = "import",
          icon = shiny::icon(
            "lock"
          )
        )
      }
    })

    #Update the Navbar when hitting the Export-Button in Analysis
    shiny::observe({
      shinydashboard::updateTabItems(
        session,
        inputId = "inTabset",
        selected = "export"
      )
    }) %>%
      shiny::bindEvent(Analysis$to_export)

    # Open Analysis after a source import has crossed the shared activation
    # boundary. Transmission remains an optional spectrum-dependent page.
    # Raw import controls may update legacy fields before a guarded history
    # reset is confirmed, so they must not drive navigation.
    shiny::observeEvent(
      Spectrum$revision,
      {
        shiny::req(Spectrum$Spectrum, Spectrum$Destination)
        if (
          identical(Spectrum$change_type, "import") &&
            Spectrum$Destination == lang$ui(69) &&
            input$inTabset == "import"
        ) {
          shinydashboard::updateTabItems(
            session,
            inputId = "inTabset",
            selected = "analysis"
          )
        }
      },
      ignoreInit = TRUE
    )

    #close the waiting screen
    # waiter::waiter_hide()
  }
  shiny::shinyApp(ui, server, ...)
}
