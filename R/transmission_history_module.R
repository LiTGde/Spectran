# Promotion, restore, history, and downloads -----------------------------

#' UI for promotion, history restore, and exports
#'
#' @param id Shiny module identifier.
#'
#' @return Shiny UI tags.
#' @noRd
transmissionHistoryControlsUI <- function(id, compact = FALSE) {
  ns <- shiny::NS(id)
  promotion_controls <- htmltools::tagList(
    shiny::textInput(
      ns("promotion_name"),
      label = transmission_text("promotion_name"),
      value = "",
      placeholder = transmission_text("promotion_placeholder"),
      updateOn = "blur"
    ),
    shiny::actionButton(
      ns("promote"),
      label = transmission_text("promote_button"),
      icon = shiny::icon("level-up"),
      class = "btn-primary"
    )
  )
  restore_controls <- htmltools::tagList(
    shiny::selectInput(
      ns("restore_node"),
      label = transmission_text("restore_node"),
      choices = character()
    ),
    shiny::actionButton(
      ns("restore"),
      label = transmission_text("restore_button"),
      icon = shiny::icon("history")
    )
  )
  controls <- if (isTRUE(compact)) {
    htmltools::tagList(
      htmltools::tags$div(
        class = "transmission-history-control-group",
        promotion_controls
      ),
      htmltools::tags$div(
        class = "transmission-history-control-group",
        restore_controls
      )
    )
  } else {
    shiny::fluidRow(
      shiny::column(
        width = 12,
        class = "col-lg-6 transmission-history-column",
        promotion_controls
      ),
      shiny::column(
        width = 12,
        class = "col-lg-6 transmission-history-column",
        restore_controls
      )
    )
  }

  htmltools::tags$div(
    class = "transmission-history-controls",
    `aria-labelledby` = ns("heading"),
    htmltools::h3(id = ns("heading"), transmission_text("promote_heading")),
    htmltools::p(
      transmission_text("promote_intro")
    ),
    controls,
    shiny::uiOutput(ns("status"))
  )
}

#' UI for the history tree and archived results
#'
#' @param id Shiny module identifier.
#'
#' @return Shiny UI tags.
#' @noRd
transmissionHistoryDetailsUI <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tags$div(
    class = "transmission-history-details",
    htmltools::h4(transmission_text("history_tree")),
    htmltools::tags$div(
      class = "transmission-gt-scroll",
      tabindex = "0",
      `aria-label` = transmission_text("aria_history_table"),
      transmission_gt_output(ns("history_table"))
    ),
    shiny::uiOutput(ns("archive_section"))
  )
}

#' UI for exporting current and archived Transmission results
#'
#' @param id Shiny module identifier.
#'
#' @return Shiny UI tags.
#' @noRd
transmissionHistoryExportUI <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tags$section(
    class = "transmission-export-section",
    `aria-labelledby` = ns("export_heading"),
    htmltools::h3(
      id = ns("export_heading"),
      transmission_text("export_heading")
    ),
    htmltools::p(transmission_text("export_intro")),
    shiny::uiOutput(ns("export_panel"))
  )
}

#' Complete UI for promotion, history restore, and exports
#'
#' @param id Shiny module identifier.
#'
#' @return Shiny UI tags.
#' @noRd
transmissionHistoryUI <- function(id) {
  htmltools::tags$section(
    class = "transmission-history-section",
    htmltools::hr(),
    transmissionHistoryControlsUI(id),
    transmissionHistoryDetailsUI(id),
    transmissionHistoryExportUI(id)
  )
}

#' Create one enabled or visibly disabled download control
#'
#' @param ns Module namespace function.
#' @param output_id Download output identifier.
#' @param label Visible label.
#' @param icon Font Awesome icon name.
#' @param enabled Whether downloading is currently permitted.
#' @param button_class CSS classes for the visible native button.
#'
#' @return A download-link tag.
#' @noRd
transmission_download_control <- function(
  ns,
  output_id,
  label,
  icon = "download",
  enabled = FALSE,
  button_class = "btn btn-default transmission-download-control"
) {
  if (!isTRUE(enabled)) {
    return(htmltools::tags$button(
      type = "button",
      class = paste(button_class, "disabled"),
      disabled = NA,
      `aria-disabled` = "true",
      shiny::icon(icon),
      label
    ))
  }
  target_id <- ns(output_id)
  htmltools::tags$span(
    class = "transmission-download-wrapper",
    htmltools::tags$button(
      type = "button",
      class = button_class,
      onclick = paste0(
        "document.getElementById('",
        target_id,
        "').click(); return false;"
      ),
      onkeydown = paste0(
        "if ((event.key === 'Enter' || event.key === ' ') && ",
        "!event.repeat) { event.preventDefault(); this.click(); }"
      ),
      shiny::icon(icon),
      label
    ),
    shiny::downloadLink(
      target_id,
      label = "",
      class = "transmission-download-target",
      `aria-hidden` = "true"
    )
  )
}

#' Server for promotion, restore, history, and downloads
#'
#' @param id Shiny module identifier.
#' @param applied_snapshot Reactive immutable applied snapshot.
#' @param can_promote Reactive promotion gate.
#' @param can_download Reactive download gate.
#' @param active_state Reactive active-spectrum adapter.
#' @param clear_snapshot Function that clears the provisional snapshot.
#' @param mark_snapshot_archived Function that preserves the promoted snapshot
#'   as a frozen visible result.
#' @param show_transmittance_panel Reactive plot-layout choice.
#' @param show_incident_fill Reactive incident-fill choice.
#' @param response_curves Reactive response-curve choices.
#'
#' @return Promotion and restore event reactives plus the history reactive.
#' @noRd
transmissionHistoryServer <- function(
  id,
  applied_snapshot,
  can_promote,
  can_download,
  active_state,
  clear_snapshot,
  mark_snapshot_archived,
  show_transmittance_panel,
  show_incident_fill,
  response_curves
) {
  reactive_arguments <- list(
    applied_snapshot = applied_snapshot,
    can_promote = can_promote,
    can_download = can_download,
    active_state = active_state,
    show_transmittance_panel = show_transmittance_panel,
    show_incident_fill = show_incident_fill,
    response_curves = response_curves
  )
  if (!all(vapply(reactive_arguments, shiny::is.reactive, logical(1)))) {
    stop("All transmission history inputs must be reactive.", call. = FALSE)
  }
  if (!is.function(clear_snapshot)) {
    stop("`clear_snapshot` must be a function.", call. = FALSE)
  }
  if (!is.function(mark_snapshot_archived)) {
    stop("`mark_snapshot_archived` must be a function.", call. = FALSE)
  }

  shiny::moduleServer(id, function(input, output, session) {
    history <- shiny::reactiveVal(NULL)
    promotion_event <- shiny::reactiveVal(NULL)
    restore_event <- shiny::reactiveVal(NULL)
    archive_node_id <- shiny::reactiveVal(NULL)
    prepared_export_bundle <- shiny::reactiveVal(NULL)
    action_sequence <- shiny::reactiveVal(0L)
    import_token <- shiny::reactiveVal(NULL)
    last_promote_click <- shiny::reactiveVal(-Inf)
    last_promote_keyboard <- shiny::reactiveVal(-Inf)
    last_restore_click <- shiny::reactiveVal(-Inf)
    last_restore_keyboard <- shiny::reactiveVal(-Inf)
    status <- shiny::reactiveVal(list(
      state = "neutral",
      message = transmission_text("history_initial")
    ))

    active_adapter <- shiny::reactive({
      current <- active_state()
      if (is.null(current)) {
        return(NULL)
      }
      as_transmission_active_spectrum(current)
    })

    shiny::observeEvent(
      active_adapter(),
      {
        current <- active_adapter()
        if (is.null(current)) {
          history(NULL)
          import_token(NULL)
          archive_node_id(NULL)
          clear_snapshot()
          status(list(
            state = "neutral",
            message = transmission_text("history_no_source")
          ))
          return()
        }

        if (identical(current$change_type, "import")) {
          token <- paste(
            current$revision,
            current$node_id,
            current$name,
            sep = "|"
          )
          if (!identical(import_token(), token)) {
            history(new_transmission_history(current))
            import_token(token)
            promotion_event(NULL)
            restore_event(NULL)
            archive_node_id(NULL)
            clear_snapshot()
            shiny::updateTextInput(session, "promotion_name", value = "")
            status(list(
              state = "current",
              message = transmission_text(
                "history_new_root",
                current$node_id
              )
            ))
          }
          return()
        }

        current_history <- history()
        if (
          !is.null(current_history) &&
            current$node_id %in% names(current_history$nodes)
        ) {
          history(
            transmission_history_restore(
              current_history,
              current$node_id
            )$history
          )
        }
      },
      ignoreInit = FALSE,
      ignoreNULL = FALSE,
      priority = 50
    )

    snapshot_token <- shiny::reactive({
      current <- applied_snapshot()
      if (is.null(current)) {
        return(NULL)
      }
      paste(current$apply_sequence, current$draft_revision, sep = "|")
    })

    shiny::observeEvent(
      snapshot_token(),
      {
        current <- applied_snapshot()
        if (is.null(current)) {
          return()
        }
        filter_name <- current$metadata$filter_name
        if (is.null(filter_name) || !nzchar(trimws(filter_name))) {
          filter_name <- transmission_text("transmission_filter")
        }
        shiny::updateTextInput(
          session,
          "promotion_name",
          value = paste0(
            current$incident_name,
            " \u00d7 ",
            trimws(filter_name)
          )
        )
      },
      ignoreInit = FALSE,
      ignoreNULL = TRUE
    )

    can_promote_current <- shiny::reactive({
      current_history <- history()
      current_active <- active_adapter()
      isTRUE(can_promote()) &&
        !is.null(applied_snapshot()) &&
        !is.null(current_history) &&
        !is.null(current_active) &&
        identical(current_history$active_node_id, current_active$node_id)
    })
    can_download_current <- shiny::reactive({
      isTRUE(can_download()) &&
        !is.null(applied_snapshot()) &&
        !is.null(history()) &&
        !is.null(active_adapter())
    })

    shiny::observe({
      shinyjs::toggleState("promote", can_promote_current())
    })

    activation_allowed <- function(
      trigger,
      last_click,
      last_keyboard
    ) {
      trigger <- match.arg(trigger, c("click", "keyboard"))
      now <- unname(proc.time()[["elapsed"]])
      cross_mode_window_seconds <- 0.5
      if (
        trigger == "click" &&
          now - last_keyboard() < cross_mode_window_seconds
      ) {
        return(FALSE)
      }
      if (
        trigger == "keyboard" &&
          now - last_click() < cross_mode_window_seconds
      ) {
        return(FALSE)
      }
      if (trigger == "click") {
        last_click(now)
      } else {
        last_keyboard(now)
      }
      TRUE
    }

    promote_current <- function(trigger = c("click", "keyboard")) {
      trigger <- match.arg(trigger)
      if (
        !activation_allowed(
          trigger,
          last_promote_click,
          last_promote_keyboard
        )
      ) {
        return(invisible(NULL))
      }
      if (!isTRUE(can_promote_current())) {
        status(list(
          state = "error",
          message = transmission_text("promotion_unavailable")
        ))
        return(invisible(NULL))
      }
      name <- input$promotion_name
      if (
        is.null(name) ||
          length(name) != 1L ||
          is.na(name) ||
          !nzchar(trimws(name))
      ) {
        status(list(
          state = "error",
          message = transmission_text("promotion_name_required")
        ))
        return(invisible(NULL))
      }

      promoted <- tryCatch(
        transmission_history_promote(
          history(),
          applied_snapshot(),
          trimws(name)
        ),
        error = function(error) error
      )
      if (inherits(promoted, "error")) {
        status(list(state = "error", message = conditionMessage(promoted)))
        return(invisible(NULL))
      }
      history(promoted$history)
      archive_node_id(promoted$node$node_id)
      next_sequence <- action_sequence() + 1L
      event <- new_transmission_activation_event(
        action_sequence = next_sequence,
        change_type = "promotion",
        node_id = promoted$node$node_id,
        parent_id = promoted$node$parent_id,
        spectrum = promoted$node$spectrum,
        name = promoted$node$name,
        provenance = promoted$node$provenance
      )
      action_sequence(next_sequence)
      promotion_event(event)
      mark_snapshot_archived()
      shiny::updateTextInput(session, "promotion_name", value = "")
      status(list(
        state = "current",
        message = transmission_text(
          "promoted_status",
          promoted$node$name,
          promoted$node$node_id
        )
      ))
      invisible(NULL)
    }

    shiny::observeEvent(input$promote, {
      promote_current("click")
    })

    shinyjs::onevent(
      event = "keydown",
      id = "promote",
      expr = function(event) {
        if (is_transmission_activation_key(event)) {
          promote_current("keyboard")
        }
      },
      properties = c("key", "code", "repeat", "which")
    )

    shiny::observe({
      current_history <- history()
      if (is.null(current_history)) {
        shiny::updateSelectInput(
          session,
          "restore_node",
          choices = character(),
          selected = character()
        )
        return(invisible(NULL))
      }
      choices <- transmission_history_choices(current_history)
      active_id <- current_history$active_node_id
      shiny::updateSelectInput(
        session,
        "restore_node",
        choices = choices,
        selected = active_id
      )
      invisible(NULL)
    })

    can_restore <- shiny::reactive({
      current_history <- history()
      !is.null(current_history) &&
        !is.null(input$restore_node) &&
        input$restore_node %in% names(current_history$nodes) &&
        !identical(input$restore_node, current_history$active_node_id)
    })
    shiny::observe({
      shinyjs::toggleState("restore", can_restore())
    })

    restore_current <- function(trigger = c("click", "keyboard")) {
      trigger <- match.arg(trigger)
      if (
        !activation_allowed(
          trigger,
          last_restore_click,
          last_restore_keyboard
        )
      ) {
        return(invisible(NULL))
      }
      if (!isTRUE(can_restore())) {
        return(invisible(NULL))
      }
      restored <- tryCatch(
        transmission_history_restore(history(), input$restore_node),
        error = function(error) error
      )
      if (inherits(restored, "error")) {
        status(list(state = "error", message = conditionMessage(restored)))
        return(invisible(NULL))
      }
      history(restored$history)
      next_sequence <- action_sequence() + 1L
      provenance <- utils::modifyList(
        restored$node$provenance,
        list(
          origin = restored$node$provenance$origin,
          restored_from_node = restored$node$node_id
        )
      )
      event <- new_transmission_activation_event(
        action_sequence = next_sequence,
        change_type = "restore",
        node_id = restored$node$node_id,
        parent_id = restored$node$parent_id,
        spectrum = restored$node$spectrum,
        name = restored$node$name,
        provenance = provenance
      )
      action_sequence(next_sequence)
      restore_event(event)
      clear_snapshot()
      shiny::updateTextInput(session, "promotion_name", value = "")
      status(list(
        state = "current",
        message = transmission_text(
          "restored_status",
          restored$node$node_id,
          restored$node$name
        )
      ))
      invisible(NULL)
    }

    shiny::observeEvent(input$restore, {
      restore_current("click")
    })

    shinyjs::onevent(
      event = "keydown",
      id = "restore",
      expr = function(event) {
        if (is_transmission_activation_key(event)) {
          restore_current("keyboard")
        }
      },
      properties = c("key", "code", "repeat", "which")
    )

    output$status <- shiny::renderUI({
      current <- status()
      htmltools::tags$div(
        class = paste(
          "transmission-history-status",
          paste0("history-", current$state)
        ),
        role = "status",
        `aria-live` = "polite",
        `aria-atomic` = "true",
        current$message
      )
    })

    output$history_table <- shiny::renderUI({
      current_history <- history()
      shiny::req(current_history)
      transmission_gt_html(transmission_history_gt(current_history))
    })

    archived_nodes <- shiny::reactive({
      current_history <- history()
      if (is.null(current_history)) {
        return(list())
      }
      Filter(
        function(node) {
          inherits(
            node$applied_snapshot,
            "transmission_applied_snapshot"
          )
        },
        current_history$nodes
      )
    })

    archived_snapshot <- shiny::reactive({
      nodes <- archived_nodes()
      selected <- archive_node_id()
      if (
        is.null(selected) ||
          !selected %in% names(nodes)
      ) {
        return(NULL)
      }
      nodes[[selected]]$applied_snapshot
    })

    shiny::observe({
      nodes <- archived_nodes()
      if (length(nodes) == 0L) {
        archive_node_id(NULL)
        return(invisible(NULL))
      }
      selected <- archive_node_id()
      if (is.null(selected) || !selected %in% names(nodes)) {
        sequences <- vapply(
          nodes,
          function(node) node$sequence_id,
          integer(1)
        )
        archive_node_id(names(nodes)[[which.max(sequences)]])
      }
      invisible(NULL)
    })

    shiny::observeEvent(
      input$archive_node,
      {
        nodes <- archived_nodes()
        if (
          !is.null(input$archive_node) &&
            input$archive_node %in% names(nodes)
        ) {
          archive_node_id(input$archive_node)
        }
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    output$archive_section <- shiny::renderUI({
      nodes <- archived_nodes()
      if (length(nodes) == 0L) {
        return(htmltools::tags$section(
          class = "transmission-archive-section is-empty",
          htmltools::h4(transmission_text("archive_heading")),
          htmltools::p(
            class = "text-muted",
            transmission_text("archive_empty")
          )
        ))
      }

      labels <- vapply(
        nodes,
        function(node) {
          paste0(node$sequence_id, ". ", node$name)
        },
        character(1)
      )
      choices <- stats::setNames(names(nodes), labels)
      selected <- archive_node_id()
      if (is.null(selected) || !selected %in% names(nodes)) {
        selected <- names(nodes)[[length(nodes)]]
      }

      htmltools::tags$section(
        class = "transmission-archive-section",
        `aria-labelledby` = session$ns("archive_heading"),
        htmltools::h4(
          id = session$ns("archive_heading"),
          transmission_text("archive_heading")
        ),
        htmltools::p(
          transmission_text("archive_intro")
        ),
        shiny::selectInput(
          session$ns("archive_node"),
          label = transmission_text("archive_select"),
          choices = choices,
          selected = selected
        ),
        shiny::uiOutput(session$ns("archived_outputs"))
      )
    })

    output$archived_outputs <- shiny::renderUI({
      current <- archived_snapshot()
      shiny::req(current)
      filter_name <- current$metadata$filter_name
      if (is.null(filter_name) || !nzchar(trimws(filter_name))) {
        filter_name <- transmission_text("transmission_filter")
      }

      htmltools::tags$div(
        class = paste(
          "transmission-applied-results transmission-analysis-package",
          "transmission-archived-results"
        ),
        `aria-label` = transmission_text("aria_archived_results"),
        shiny::uiOutput(session$ns("archived_metric_warnings")),
        shiny::uiOutput(session$ns("archived_plot_outputs")),
        transmission_tabset_panel(
          id = session$ns("archived_metric_table_tabs"),
          type = "tabs",
          shiny::tabPanel(
            title = transmission_text("result_tab_d65"),
            value = "d65",
            htmltools::tags$div(
              class = "transmission-gt-scroll",
              tabindex = "0",
              `aria-label` = transmission_text("aria_archived_d65_table"),
              transmission_gt_output(session$ns(
                "archived_d65_properties"
              ))
            )
          ),
          shiny::tabPanel(
            title = transmission_text("result_tab_light"),
            value = "light",
            htmltools::tags$div(
              class = "transmission-gt-scroll",
              tabindex = "0",
              `aria-label` = transmission_text(
                "aria_archived_absolute_table"
              ),
              transmission_gt_output(session$ns(
                "archived_absolute_metrics"
              ))
            )
          ),
          shiny::tabPanel(
            title = transmission_text("result_tab_balance"),
            value = "balance",
            htmltools::tags$div(
              class = "transmission-gt-scroll",
              tabindex = "0",
              `aria-label` = transmission_text("aria_archived_balance_table"),
              transmission_gt_output(session$ns(
                "archived_balance_metrics"
              ))
            )
          ),
          selected = "d65"
        )
      )
    })

    output$archived_plot_outputs <- shiny::renderUI({
      current <- archived_snapshot()
      shiny::req(current)
      include_filter <- isTRUE(show_transmittance_panel())
      panel_layout <- transmission_result_panel_layout(
        session$clientData$output_Plotbreite_width
      )
      htmltools::tags$div(
        class = paste(
          "transmission-result-plot-grid",
          if (include_filter) "has-filter-panel" else "single-panel"
        ),
        shiny::plotOutput(
          session$ns("archived_spectral_comparison"),
          height = if (include_filter && identical(panel_layout, "stack")) {
            "680px"
          } else {
            "430px"
          }
        )
      )
    })

    output$archived_spectral_comparison <- shiny::renderPlot(
      {
        current <- archived_snapshot()
        shiny::req(current)
        output_width_key <- paste0(
          "output_",
          session$ns("archived_spectral_comparison"),
          "_width"
        )
        archived_plot_width <- session$clientData[[output_width_key]]
        if (
          !is.numeric(archived_plot_width) ||
            length(archived_plot_width) != 1L ||
            !is.finite(archived_plot_width)
        ) {
          archived_plot_width <-
            session$clientData$output_Plotbreite_width
        }
        transmission_spectral_comparison_plot(
          current,
          show_transmittance_panel = isTRUE(show_transmittance_panel()),
          show_title = TRUE,
          incident_fill = isTRUE(show_incident_fill()),
          response_curves = response_curves(),
          panel_layout = transmission_result_panel_layout(
            session$clientData$output_Plotbreite_width
          ),
          title_wrap_width = transmission_archived_result_title_wrap_width(
            archived_plot_width
          ),
          title_word_wrap_width =
            transmission_result_title_word_wrap_width(
              archived_plot_width
            ),
          plot_margin_right =
            transmission_archived_result_title_right_margin(
              archived_plot_width
            ),
          adaptive_title_spacing = TRUE
        )
      },
      alt = transmission_text("alt_archived_spectral_comparison")
    )

    output$archived_d65_properties <- shiny::renderUI({
      current <- archived_snapshot()
      shiny::req(current)
      transmission_gt_html(transmission_d65_gt(current))
    })

    output$archived_absolute_metrics <- shiny::renderUI({
      current <- archived_snapshot()
      shiny::req(current)
      transmission_gt_html(transmission_absolute_gt(current))
    })

    output$archived_balance_metrics <- shiny::renderUI({
      current <- archived_snapshot()
      shiny::req(current)
      transmission_gt_html(transmission_balance_gt(current))
    })

    invisible(lapply(
      c(
        "archived_d65_properties",
        "archived_absolute_metrics",
        "archived_balance_metrics"
      ),
      function(output_id) {
        shiny::outputOptions(
          output,
          output_id,
          suspendWhenHidden = FALSE
        )
      }
    ))

    output$archived_metric_warnings <- shiny::renderUI({
      current <- archived_snapshot()
      shiny::req(current)
      transmission_metric_warning_ui(
        transmission_metric_warning_presentation(current)
      )
    })

    output$archive_download_controls <- shiny::renderUI({
      htmltools::tags$div(
        class = "transmission-download-grid",
        transmission_download_control(
          session$ns,
          "archive_download_completed",
          transmission_text("download_completed"),
          enabled = TRUE
        ),
        transmission_download_control(
          session$ns,
          "archive_download_comparison",
          transmission_text("download_comparison"),
          enabled = TRUE
        ),
        transmission_download_control(
          session$ns,
          "archive_download_plot",
          transmission_text("download_plot"),
          icon = "image",
          enabled = TRUE
        ),
        transmission_download_control(
          session$ns,
          "archive_download_d65",
          transmission_text("download_d65"),
          enabled = TRUE
        ),
        transmission_download_control(
          session$ns,
          "archive_download_metrics",
          transmission_text("download_metrics"),
          enabled = TRUE
        ),
        transmission_download_control(
          session$ns,
          "archive_download_history",
          transmission_text("download_history"),
          enabled = TRUE
        ),
        transmission_download_control(
          session$ns,
          "archive_download_audit",
          transmission_text("download_audit"),
          icon = "archive",
          enabled = TRUE
        )
      )
    })

    output$download_controls <- shiny::renderUI({
      enabled <- can_download_current()
      htmltools::tags$div(
        class = "transmission-download-grid",
        transmission_download_control(
          session$ns,
          "download_completed",
          transmission_text("download_completed"),
          enabled = enabled
        ),
        transmission_download_control(
          session$ns,
          "download_comparison",
          transmission_text("download_comparison"),
          enabled = enabled
        ),
        transmission_download_control(
          session$ns,
          "download_plot",
          transmission_text("download_plot"),
          icon = "image",
          enabled = enabled
        ),
        transmission_download_control(
          session$ns,
          "download_d65",
          transmission_text("download_d65"),
          enabled = enabled
        ),
        transmission_download_control(
          session$ns,
          "download_metrics",
          transmission_text("download_metrics"),
          enabled = enabled
        ),
        transmission_download_control(
          session$ns,
          "download_history",
          transmission_text("download_history"),
          enabled = enabled
        ),
        transmission_download_control(
          session$ns,
          "download_audit",
          transmission_text("download_audit"),
          icon = "archive",
          enabled = enabled
        )
      )
    })

    export_choices <- shiny::reactive({
      choices <- character()
      if (isTRUE(can_download_current())) {
        choices <- c(
          choices,
          stats::setNames("current", transmission_text("export_current"))
        )
      }
      nodes <- archived_nodes()
      if (length(nodes) > 0L) {
        labels <- vapply(
          nodes,
          function(node) {
            paste0(node$sequence_id, ". ", node$name)
          },
          character(1)
        )
        choices <- c(
          choices,
          stats::setNames(paste0("archive:", names(nodes)), labels)
        )
      }
      choices
    })

    export_snapshot <- shiny::reactive({
      selected <- input$export_result
      choices <- unname(export_choices())
      if (is.null(selected) || !selected %in% choices) {
        if (length(choices) == 0L) {
          return(NULL)
        }
        selected <- choices[[1L]]
      }
      if (identical(selected, "current")) {
        if (!isTRUE(can_download_current())) {
          return(NULL)
        }
        return(applied_snapshot())
      }
      node_id <- sub("^archive:", "", selected)
      nodes <- archived_nodes()
      if (!node_id %in% names(nodes)) {
        return(NULL)
      }
      nodes[[node_id]]$applied_snapshot
    })

    bundle_choice_labels <- function() {
      stats::setNames(
        transmission_export_content_ids(),
        vapply(
          transmission_export_content_ids(),
          function(id) transmission_text(paste0("export_bundle_", id)),
          character(1)
        )
      )
    }
    export_bundle_contents <- shiny::reactive({
      value <- input$export_bundle_contents
      intersect(
        as.character(value %||% character()),
        transmission_export_content_ids()
      )
    })
    valid_export_number <- function(value, fallback) {
      value <- suppressWarnings(as.numeric(value))
      if (length(value) == 1L && is.finite(value) && value > 0) {
        value
      } else {
        fallback
      }
    }
    export_plot_width <- shiny::reactive({
      valid_export_number(input$export_plot_width, 9)
    })
    export_plot_height <- shiny::reactive({
      valid_export_number(input$export_plot_height, 5.8)
    })
    export_font_size <- shiny::reactive({
      valid_export_number(input$export_font_size, 15)
    })
    export_scale_valid <- shiny::reactive({
      value <- trimws(input$export_scale_max %||% "")
      if (!nzchar(value)) return(TRUE)
      number <- suppressWarnings(as.numeric(value))
      length(number) == 1L && is.finite(number) && number > 0
    })
    export_scale_max <- shiny::reactive({
      if (!isTRUE(export_scale_valid())) return(NULL)
      value <- trimws(input$export_scale_max %||% "")
      if (!nzchar(value)) NULL else as.numeric(value)
    })
    export_plot_table_metric <- shiny::reactive({
      value <- input$export_plot_table_metric
      if (is.null(value) || !value %in% c("d65", "light", "balance")) {
        "d65"
      } else {
        value
      }
    })

    export_bundle_configuration <- shiny::reactive({
      snapshot <- export_snapshot()
      list(
        result = input$export_result %||% "",
        apply_sequence = snapshot$apply_sequence %||% NA_integer_,
        draft_revision = snapshot$draft_revision %||% NA_integer_,
        contents = sort(export_bundle_contents()),
        show_transmittance_panel = isTRUE(show_transmittance_panel()),
        incident_fill = isTRUE(show_incident_fill()),
        response_curves = sort(response_curves()),
        plot_width = export_plot_width(),
        plot_height = export_plot_height(),
        font_size = export_font_size(),
        max_irradiance = export_scale_max(),
        plot_table_metric = export_plot_table_metric()
      )
    })

    clear_prepared_export_bundle <- function() {
      prepared <- prepared_export_bundle()
      if (!is.null(prepared$file) && file.exists(prepared$file)) {
        unlink(prepared$file, force = TRUE)
      }
      prepared_export_bundle(NULL)
      invisible(NULL)
    }

    shiny::observeEvent(
      export_bundle_configuration(),
      clear_prepared_export_bundle(),
      ignoreInit = TRUE
    )

    session$onSessionEnded(function() {
      shiny::isolate(clear_prepared_export_bundle())
    })

    output$export_bundle_summary <- shiny::renderUI({
      htmltools::tags$div(
        class = "transmission-export-count",
        role = "status",
        `aria-live` = "polite",
        transmission_text(
          "export_bundle_summary",
          length(export_bundle_contents())
        )
      )
    })
    output$export_scale_feedback <- shiny::renderUI({
      if (isTRUE(export_scale_valid())) return(NULL)
      htmltools::tags$div(
        class = "transmission-export-scale-error",
        role = "alert",
        transmission_text("export_scale_invalid")
      )
    })
    output$export_bundle_control <- shiny::renderUI({
      enabled <- length(export_bundle_contents()) > 0L &&
        isTRUE(export_scale_valid())
      button_class <- paste(
        "btn btn-primary btn-lg transmission-download-control",
        "transmission-export-primary"
      )
      if (!isTRUE(enabled)) {
        return(htmltools::tags$button(
          type = "button",
          class = paste(button_class, "disabled"),
          disabled = NA,
          `aria-disabled` = "true",
          shiny::icon("archive"),
          transmission_text("download_bundle")
        ))
      }
      prepared <- prepared_export_bundle()
      current_configuration <- export_bundle_configuration()
      if (
        isTRUE(enabled) &&
          !is.null(prepared) &&
          identical(prepared$configuration, current_configuration) &&
          file.exists(prepared$file)
      ) {
        return(transmission_download_control(
          session$ns,
          "export_download_bundle",
          transmission_text("download_bundle_ready"),
          icon = "download",
          enabled = TRUE,
          button_class = button_class
        ))
      }
      shiny::actionButton(
        session$ns("export_prepare_bundle"),
        transmission_text("download_bundle"),
        icon = shiny::icon("archive"),
        class = paste(button_class, "transmission-guided-action")
      )
    })
    output$export_bundle_build_status <- shiny::renderUI({
      prepared <- prepared_export_bundle()
      if (
        is.null(prepared) ||
          !identical(
            prepared$configuration,
            export_bundle_configuration()
          ) ||
          !file.exists(prepared$file)
      ) {
        return(NULL)
      }
      htmltools::tags$div(
        class = "transmission-export-bundle-status",
        role = "status",
        `aria-live` = "polite",
        transmission_text("export_bundle_ready")
      )
    })
    invisible(lapply(
      c(
        "export_bundle_summary",
        "export_scale_feedback",
        "export_bundle_control",
        "export_bundle_build_status"
      ),
      function(output_id) {
        shiny::outputOptions(
          output,
          output_id,
          suspendWhenHidden = FALSE
        )
      }
    ))

    output$export_panel <- shiny::renderUI({
      choices <- export_choices()
      if (length(choices) == 0L) {
        return(htmltools::tags$div(
          class = "transmission-export-empty",
          role = "status",
          transmission_text("export_none")
        ))
      }
      selected <- input$export_result
      if (is.null(selected) || !selected %in% unname(choices)) {
        selected <- unname(choices)[[1L]]
      }
      htmltools::tagList(
        shiny::selectInput(
          session$ns("export_result"),
          label = transmission_text("export_result"),
          choices = choices,
          selected = selected
        ),
        htmltools::tags$section(
          class = "transmission-export-settings",
          htmltools::h4(transmission_text("export_plot_settings")),
          htmltools::p(transmission_text("export_plot_settings_intro")),
          htmltools::tags$div(
            class = "transmission-export-setting-grid",
            htmltools::tags$div(
              class = paste(
                "transmission-export-setting-row",
                "transmission-export-setting-row-primary"
              ),
              shiny::numericInput(
                session$ns("export_plot_width"),
                label = transmission_text("export_plot_width"),
                value = 9,
                min = 1,
                step = 0.5
              ),
              shiny::numericInput(
                session$ns("export_plot_height"),
                label = transmission_text("export_plot_height"),
                value = 5.8,
                min = 1,
                step = 0.5
              ),
              shiny::numericInput(
                session$ns("export_font_size"),
                label = transmission_text("export_font_size"),
                value = 15,
                min = 6,
                step = 1
              )
            ),
            htmltools::tags$div(
              class = paste(
                "transmission-export-setting-row",
                "transmission-export-setting-row-secondary"
              ),
              htmltools::tags$div(
                class = "transmission-export-setting-control",
                shiny::textInput(
                  session$ns("export_scale_max"),
                  label = transmission_text("export_scale_max"),
                  value = ""
                ),
                shiny::uiOutput(session$ns("export_scale_feedback"))
              ),
              htmltools::tags$div(
                class = "transmission-export-setting-control",
                shiny::selectInput(
                  session$ns("export_plot_table_metric"),
                  label = transmission_text("export_plot_table_metric"),
                  choices = stats::setNames(
                    c("d65", "light", "balance"),
                    c(
                      transmission_text("result_tab_d65"),
                      transmission_text("result_tab_light"),
                      transmission_text("result_tab_balance")
                    )
                  ),
                  selected = "d65"
                )
              )
            )
          )
        ),
        htmltools::tags$section(
          class = "transmission-export-bundle",
          htmltools::h4(transmission_text("export_bundle_heading")),
          htmltools::p(transmission_text("export_bundle_intro")),
          shiny::checkboxGroupInput(
            session$ns("export_bundle_contents"),
            label = NULL,
            choiceNames = names(bundle_choice_labels()),
            choiceValues = unname(bundle_choice_labels()),
            selected = transmission_export_content_ids()
          ),
          shiny::uiOutput(session$ns("export_bundle_summary")),
          shiny::uiOutput(session$ns("export_bundle_build_status")),
          shiny::uiOutput(session$ns("export_bundle_control"))
        ),
        htmltools::tags$details(
          class = "transmission-export-quick",
          htmltools::tags$summary(
            transmission_text("export_quick_heading")
          ),
          htmltools::tags$div(
            class = "transmission-export-groups",
            htmltools::tags$section(
              class = "transmission-export-group",
              htmltools::h4(transmission_text("export_figures")),
              htmltools::tags$div(
                class = "transmission-download-grid",
                transmission_download_control(
                  session$ns,
                  "export_download_plot",
                  transmission_text("download_result_plot"),
                  icon = "image",
                  enabled = TRUE
                ),
                transmission_download_control(
                  session$ns,
                  "export_download_filter_plot",
                  transmission_text("download_filter_plot"),
                  icon = "image",
                  enabled = TRUE
                ),
                transmission_download_control(
                  session$ns,
                  "export_download_plot_table",
                  transmission_text("download_plot_table"),
                  icon = "image",
                  enabled = TRUE
                )
              )
            ),
            htmltools::tags$section(
              class = "transmission-export-group",
              htmltools::h4(transmission_text("export_tables")),
              htmltools::tags$div(
                class = "transmission-download-grid",
                transmission_download_control(
                  session$ns,
                  "export_download_d65_png",
                  transmission_text("download_d65_table_png"),
                  icon = "image",
                  enabled = TRUE
                ),
                transmission_download_control(
                  session$ns,
                  "export_download_d65_csv",
                  transmission_text("download_d65"),
                  enabled = TRUE
                ),
                transmission_download_control(
                  session$ns,
                  "export_download_light_png",
                  transmission_text("download_light_table_png"),
                  icon = "image",
                  enabled = TRUE
                ),
                transmission_download_control(
                  session$ns,
                  "export_download_light_csv",
                  transmission_text("download_light_table_csv"),
                  enabled = TRUE
                ),
                transmission_download_control(
                  session$ns,
                  "export_download_balance_png",
                  transmission_text("download_balance_table_png"),
                  icon = "image",
                  enabled = TRUE
                ),
                transmission_download_control(
                  session$ns,
                  "export_download_balance_csv",
                  transmission_text("download_balance_table_csv"),
                  enabled = TRUE
                )
              )
            ),
            htmltools::tags$section(
              class = "transmission-export-group",
              htmltools::h4(transmission_text("export_data")),
              htmltools::tags$div(
                class = "transmission-download-grid",
                transmission_download_control(
                  session$ns,
                  "export_download_completed",
                  transmission_text("download_completed"),
                  enabled = TRUE
                ),
                transmission_download_control(
                  session$ns,
                  "export_download_comparison",
                  transmission_text("download_comparison"),
                  enabled = TRUE
                ),
                transmission_download_control(
                  session$ns,
                  "export_download_history",
                  transmission_text("download_history"),
                  enabled = TRUE
                ),
                transmission_download_control(
                  session$ns,
                  "export_download_audit",
                  transmission_text("download_audit"),
                  icon = "archive",
                  enabled = TRUE
                )
              )
            )
          )
        )
      )
    })

    download_filename <- function(suffix, extension = "csv") {
      current <- applied_snapshot()
      base <- if (is.null(current)) {
        "spectran-transmission"
      } else {
        transmission_filename_component(current$metadata$filter_name)
      }
      paste0(base, "-", suffix, ".", extension)
    }
    current_download <- function() {
      shiny::req(isTRUE(can_download_current()))
      current <- applied_snapshot()
      shiny::req(current)
      current
    }

    output$download_completed <- shiny::downloadHandler(
      filename = function() download_filename("completed-filter"),
      content = function(file) {
        write_transmission_csv(
          transmission_completed_filter_export(current_download()),
          file
        )
      }
    )
    output$download_comparison <- shiny::downloadHandler(
      filename = function() download_filename("spectral-comparison"),
      content = function(file) {
        write_transmission_csv(
          transmission_spectral_comparison_export(current_download()),
          file
        )
      }
    )
    output$download_plot <- shiny::downloadHandler(
      filename = function() download_filename("transmission-plot", "png"),
      content = function(file) {
        write_transmission_result_plot(
          snapshot = current_download(),
          file = file,
          show_transmittance_panel = isTRUE(show_transmittance_panel()),
          incident_fill = isTRUE(show_incident_fill()),
          response_curves = response_curves()
        )
      },
      contentType = "image/png"
    )
    output$download_d65 <- shiny::downloadHandler(
      filename = function() download_filename("d65-properties"),
      content = function(file) {
        write_transmission_csv(
          transmission_d65_export(current_download()),
          file
        )
      }
    )
    output$download_metrics <- shiny::downloadHandler(
      filename = function() download_filename("applied-metrics"),
      content = function(file) {
        write_transmission_csv(
          transmission_applied_metrics_export(current_download()),
          file
        )
      }
    )
    output$download_history <- shiny::downloadHandler(
      filename = function() download_filename("history"),
      content = function(file) {
        current_download()
        write_transmission_csv(transmission_history_table(history()), file)
      }
    )
    output$download_audit <- shiny::downloadHandler(
      filename = function() download_filename("audit", "zip"),
      content = function(file) {
        write_transmission_audit_zip(
          file = file,
          snapshot = current_download(),
          history = history(),
          active_state = active_adapter()
        )
      },
      contentType = "application/zip"
    )

    archive_download_filename <- function(suffix, extension = "csv") {
      current <- archived_snapshot()
      base <- if (is.null(current)) {
        "spectran-archived-transmission"
      } else {
        transmission_filename_component(current$metadata$filter_name)
      }
      node_component <- if (is.null(archive_node_id())) {
        "archived"
      } else {
        transmission_filename_component(archive_node_id())
      }
      paste0(base, "-", node_component, "-", suffix, ".", extension)
    }
    current_archive_download <- function() {
      current <- archived_snapshot()
      shiny::req(current)
      current
    }

    output$archive_download_completed <- shiny::downloadHandler(
      filename = function() archive_download_filename("completed-filter"),
      content = function(file) {
        write_transmission_csv(
          transmission_completed_filter_export(current_archive_download()),
          file
        )
      }
    )
    output$archive_download_comparison <- shiny::downloadHandler(
      filename = function() archive_download_filename("spectral-comparison"),
      content = function(file) {
        write_transmission_csv(
          transmission_spectral_comparison_export(
            current_archive_download()
          ),
          file
        )
      }
    )
    output$archive_download_plot <- shiny::downloadHandler(
      filename = function() {
        archive_download_filename("transmission-plot", "png")
      },
      content = function(file) {
        write_transmission_result_plot(
          snapshot = current_archive_download(),
          file = file,
          show_transmittance_panel = isTRUE(show_transmittance_panel()),
          incident_fill = isTRUE(show_incident_fill()),
          response_curves = response_curves()
        )
      },
      contentType = "image/png"
    )
    output$archive_download_d65 <- shiny::downloadHandler(
      filename = function() archive_download_filename("d65-properties"),
      content = function(file) {
        write_transmission_csv(
          transmission_d65_export(current_archive_download()),
          file
        )
      }
    )
    output$archive_download_metrics <- shiny::downloadHandler(
      filename = function() archive_download_filename("applied-metrics"),
      content = function(file) {
        write_transmission_csv(
          transmission_applied_metrics_export(current_archive_download()),
          file
        )
      }
    )
    output$archive_download_history <- shiny::downloadHandler(
      filename = function() archive_download_filename("history"),
      content = function(file) {
        current_archive_download()
        write_transmission_csv(transmission_history_table(history()), file)
      }
    )
    output$archive_download_audit <- shiny::downloadHandler(
      filename = function() archive_download_filename("audit", "zip"),
      content = function(file) {
        write_transmission_audit_zip(
          file = file,
          snapshot = current_archive_download(),
          history = history(),
          active_state = active_adapter()
        )
      },
      contentType = "application/zip"
    )

    export_download_filename <- function(suffix, extension = "csv") {
      current <- export_snapshot()
      base <- if (is.null(current)) {
        "spectran-transmission"
      } else {
        transmission_filename_component(current$metadata$filter_name)
      }
      selected <- input$export_result
      node <- if (!is.null(selected) && startsWith(selected, "archive:")) {
        paste0(
          "-",
          transmission_filename_component(sub("^archive:", "", selected))
        )
      } else {
        ""
      }
      paste0(base, node, "-", suffix, ".", extension)
    }
    current_export_download <- function() {
      current <- export_snapshot()
      shiny::req(current)
      current
    }

    output$export_download_plot <- shiny::downloadHandler(
      filename = function() export_download_filename("result-plot", "png"),
      content = function(file) {
        write_transmission_result_plot(
          snapshot = current_export_download(),
          file = file,
          show_transmittance_panel = isTRUE(show_transmittance_panel()),
          incident_fill = isTRUE(show_incident_fill()),
          response_curves = response_curves(),
          width = export_plot_width(),
          height = export_plot_height(),
          font_size = export_font_size(),
          max_irradiance = export_scale_max()
        )
      },
      contentType = "image/png"
    )
    output$export_download_filter_plot <- shiny::downloadHandler(
      filename = function() {
        export_download_filename("transmittance-spectrum", "png")
      },
      content = function(file) {
        write_transmission_filter_plot(
          snapshot = current_export_download(),
          file = file,
          width = export_plot_width(),
          height = export_plot_height(),
          font_size = export_font_size()
        )
      },
      contentType = "image/png"
    )
    output$export_download_plot_table <- shiny::downloadHandler(
      filename = function() {
        export_download_filename(
          paste0("result-with-", export_plot_table_metric(), "-table"),
          "png"
        )
      },
      content = function(file) {
        write_transmission_plot_table_png(
          snapshot = current_export_download(),
          file = file,
          metric_group = export_plot_table_metric(),
          show_transmittance_panel = isTRUE(show_transmittance_panel()),
          incident_fill = isTRUE(show_incident_fill()),
          response_curves = response_curves(),
          width = export_plot_width(),
          height = export_plot_height(),
          font_size = export_font_size(),
          max_irradiance = export_scale_max()
        )
      },
      contentType = "image/png"
    )
    output$export_download_d65_png <- shiny::downloadHandler(
      filename = function() {
        export_download_filename("d65-properties-table", "png")
      },
      content = function(file) {
        write_transmission_gt_png(
          transmission_d65_gt(current_export_download()),
          file
        )
      },
      contentType = "image/png"
    )
    output$export_download_light_png <- shiny::downloadHandler(
      filename = function() {
        export_download_filename("light-values-table", "png")
      },
      content = function(file) {
        write_transmission_gt_png(
          transmission_absolute_gt(current_export_download()),
          file
        )
      },
      contentType = "image/png"
    )
    output$export_download_balance_png <- shiny::downloadHandler(
      filename = function() {
        export_download_filename("spectral-balance-table", "png")
      },
      content = function(file) {
        write_transmission_gt_png(
          transmission_balance_gt(current_export_download()),
          file
        )
      },
      contentType = "image/png"
    )
    output$export_download_completed <- shiny::downloadHandler(
      filename = function() export_download_filename("completed-filter"),
      content = function(file) {
        write_transmission_csv(
          transmission_completed_filter_export(current_export_download()),
          file
        )
      }
    )
    output$export_download_comparison <- shiny::downloadHandler(
      filename = function() export_download_filename("spectral-comparison"),
      content = function(file) {
        write_transmission_csv(
          transmission_spectral_comparison_export(current_export_download()),
          file
        )
      }
    )
    output$export_download_d65_csv <- shiny::downloadHandler(
      filename = function() export_download_filename("d65-properties"),
      content = function(file) {
        write_transmission_csv(
          transmission_d65_export(current_export_download()),
          file
        )
      }
    )
    output$export_download_light_csv <- shiny::downloadHandler(
      filename = function() export_download_filename("light-values"),
      content = function(file) {
        write_transmission_csv(
          transmission_light_metrics_export(current_export_download()),
          file
        )
      }
    )
    output$export_download_balance_csv <- shiny::downloadHandler(
      filename = function() export_download_filename("spectral-balance"),
      content = function(file) {
        write_transmission_csv(
          transmission_balance_metrics_export(current_export_download()),
          file
        )
      }
    )
    output$export_download_history <- shiny::downloadHandler(
      filename = function() export_download_filename("history"),
      content = function(file) {
        current_export_download()
        write_transmission_csv(transmission_history_table(history()), file)
      }
    )
    output$export_download_audit <- shiny::downloadHandler(
      filename = function() export_download_filename("audit", "zip"),
      content = function(file) {
        write_transmission_audit_zip(
          file = file,
          snapshot = current_export_download(),
          history = history(),
          active_state = active_adapter()
        )
      },
      contentType = "application/zip"
    )
    shiny::observeEvent(input$export_prepare_bundle, {
      shiny::req(length(export_bundle_contents()) > 0L)
      shiny::req(isTRUE(export_scale_valid()))
      configuration <- shiny::isolate(export_bundle_configuration())
      selected_contents <- configuration$contents
      export_snapshot <- shiny::isolate(current_export_download())
      export_history <- shiny::isolate(history())
      export_active_state <- shiny::isolate(active_adapter())
      bundle_file <- tempfile(
        pattern = "spectran-transmission-export-",
        fileext = ".zip"
      )
      clear_prepared_export_bundle()
      shiny::withProgress(
        message = transmission_text("export_bundle_progress_heading"),
        detail = transmission_text(
          "export_bundle_progress_detail",
          length(selected_contents)
        ),
        value = 0,
        {
          shiny::setProgress(value = 0.05)
          write_transmission_export_bundle(
            file = bundle_file,
            snapshot = export_snapshot,
            history = export_history,
            active_state = export_active_state,
            show_transmittance_panel = configuration$show_transmittance_panel,
            incident_fill = configuration$incident_fill,
            response_curves = configuration$response_curves,
            contents = selected_contents,
            plot_width = configuration$plot_width,
            plot_height = configuration$plot_height,
            font_size = configuration$font_size,
            max_irradiance = configuration$max_irradiance,
            plot_table_metric = configuration$plot_table_metric
          )
          shiny::setProgress(value = 1)
        }
      )
      prepared_export_bundle(list(
        file = bundle_file,
        configuration = configuration
      ))
    })

    output$export_download_bundle <- shiny::downloadHandler(
      filename = function() export_download_filename("export-bundle", "zip"),
      content = function(file) {
        prepared <- prepared_export_bundle()
        shiny::req(!is.null(prepared))
        shiny::req(identical(
          prepared$configuration,
          export_bundle_configuration()
        ))
        shiny::req(file.exists(prepared$file))
        copied <- file.copy(prepared$file, file, overwrite = TRUE)
        if (!isTRUE(copied)) {
          stop("The prepared transmission export bundle could not be copied.")
        }
      },
      contentType = "application/zip"
    )

    # The user-facing controls are native buttons that activate hidden Shiny
    # download outputs. Keep those output bindings live so Shiny supplies each
    # target URL even though the target link itself is visually hidden.
    download_output_ids <- c(
      "download_completed",
      "download_comparison",
      "download_plot",
      "download_d65",
      "download_metrics",
      "download_history",
      "download_audit",
      "archive_download_completed",
      "archive_download_comparison",
      "archive_download_plot",
      "archive_download_d65",
      "archive_download_metrics",
      "archive_download_history",
      "archive_download_audit",
      "export_download_plot",
      "export_download_filter_plot",
      "export_download_plot_table",
      "export_download_d65_png",
      "export_download_light_png",
      "export_download_balance_png",
      "export_download_completed",
      "export_download_comparison",
      "export_download_d65_csv",
      "export_download_light_csv",
      "export_download_balance_csv",
      "export_download_history",
      "export_download_audit",
      "export_download_bundle"
    )
    invisible(lapply(download_output_ids, function(output_id) {
      shiny::outputOptions(
        output,
        output_id,
        suspendWhenHidden = FALSE
      )
    }))

    list(
      promotion_event = shiny::reactive(promotion_event()),
      restore_event = shiny::reactive(restore_event()),
      history = shiny::reactive(history()),
      archived_snapshot = archived_snapshot,
      archive_node_id = shiny::reactive(archive_node_id()),
      can_promote = can_promote_current,
      can_download = can_download_current,
      action_sequence = shiny::reactive(action_sequence())
    )
  })
}
