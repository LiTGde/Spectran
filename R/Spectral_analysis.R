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
  help_links <- spectran_explanation_links_ui("explanations")

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
          shinydashboard::tabItem(tabName = "import", importUI("import", help_ui = help_links$from_import)),
          #add a tab for the analysis
          shinydashboard::tabItem(tabName = "analysis", analysisUI("analysis", help_links = help_links)),
          #add a tab for the export
          shinydashboard::tabItem(tabName = "export", exportUI("export", help_ui = help_links$from_export)),
          #add the optional transmission-filter tab after export
          shinydashboard::tabItem(
            tabName = "transmission",
            transmissionUI(
              "transmission",
              default_source = "catalogue",
              layout = "workspace",
              source_ui = material_source_ui("material_source"),
              help_links = help_links
            )
          ),
          shinydashboard::tabItem(tabName = "explanations", spectran_explanations_ui("explanations")),
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
    explanations <- spectran_explanations_server("explanations", shiny::reactive(input$inTabset),
      function(page) shinydashboard::updateTabItems(session, "inTabset", selected = page))

    #Introduction
    intro_navigation <- introductionServer("intro")

    #Shared active-spectrum state
    Spectrum <- shiny::reactiveValues()
    initialize_spectran_spectrum_state(Spectrum)

    # Register the material workflow only when it is first opened. The proxy
    # reactives keep import guards and activation observers connected before
    # that point, then follow the same module for the rest of this session.
    transmission_module <- shiny::reactiveVal(NULL)
    Transmission <- list(
      history = shiny::reactive({
        module <- transmission_module()
        if (!is.null(module)) module$history()
      }),
      promotion_event = shiny::reactive({
        module <- transmission_module()
        if (!is.null(module)) module$promotion_event()
      }),
      restore_event = shiny::reactive({
        module <- transmission_module()
        if (!is.null(module)) module$restore_event()
      })
    )
    material_loading <- shiny::reactiveVal(FALSE)
    material_initialize <- shiny::reactiveVal(0L)
    shiny::observeEvent(input$inTabset, {
      if (identical(input$inTabset, "transmission") &&
          is.null(transmission_module()) && !isTRUE(material_loading())) {
        material_loading(TRUE)
        shiny::showModal(shiny::modalDialog(
          title = material_workspace_text("loading_title"),
          htmltools::div(class = "material-loading", role = "status", `aria-live` = "polite",
            shiny::icon("spinner", class = "fa-spin"),
            htmltools::p(material_workspace_text("loading"))),
          footer = NULL, easyClose = FALSE, fade = FALSE, size = "s"
        ))
        # Send the modal before registering and evaluating the material outputs.
        session$onFlushed(function() {
          material_initialize(shiny::isolate(material_initialize()) + 1L)
        }, once = TRUE)
      }
    })
    shiny::observeEvent(material_initialize(), {
      if (material_initialize() == 0L) return()
      tryCatch({
        activate_spectran_default_daylight(Spectrum)
        transmission_module(transmissionServer(
            "transmission",
            workspace = TRUE,
            incident_spectrum = shiny::reactive(Spectrum$Spectrum),
            incident_name = shiny::reactive(Spectrum$Name),
            active_state = shiny::reactive(
              spectran_transmission_active_state(Spectrum)
            )
        ))
      }, error = function(error) {
        shiny::showNotification(material_workspace_text("loading_error"), type = "error", duration = NULL)
        warning(conditionMessage(error), call. = FALSE)
      })
      session$onFlushed(function() {
        shiny::removeModal()
        material_loading(FALSE)
      }, once = TRUE)
    }, ignoreInit = TRUE)

    #Import. Source changes are guarded when promoted history exists.
    Spectrum <- importServer(
      "import",
      Spectrum = Spectrum,
      transmission_history = Transmission$history
    )

    material_source_server(
      "material_source",
      current = shiny::reactive(spectran_transmission_active_state(Spectrum)),
      history = Transmission$history,
      on_restore = function(node_id) {
        module <- transmission_module()
        if (!is.null(module)) module$restore_node(node_id)
      },
      automatic = shiny::reactive(Spectrum$automatic_source),
      on_import = function(request) activate_spectran_spectrum(
        Spectrum, request$spectrum, request$name, request$origin,
        "import", "node-1", request$provenance, destination = request$destination
      )
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
      activate_transmission_event(Transmission$promotion_event()),
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

    # Introduction returns a destination; the parent owns cross-module routing.
    shiny::observeEvent(intro_navigation(), {
      event <- intro_navigation()
      shiny::req(event$page)
      if (identical(event$page, "explanations")) {
        explanations$open_from("home", "tutorial", shiny::NS("intro")("to_explanations"))
        return(invisible(NULL))
      }
      shinydashboard::updateTabItems(
        session,
        inputId = "inTabset",
        selected = event$page
      )
    }, ignoreInit = TRUE)

    #Enable/disable spectrum-dependent menus when no source is active
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
