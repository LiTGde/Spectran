# Promotion, restore, history, and downloads -----------------------------

#' UI for promoting a receiver spectrum
#'
#' @param id Shiny module identifier.
#'
#' @return Shiny UI tags.
#' @noRd
transmissionHistoryControlsUI <- function(id, compact = FALSE, workspace = FALSE) {
  ns <- shiny::NS(id)
  promotion_controls <- htmltools::tagList(
    if (isTRUE(workspace)) shiny::checkboxInput(ns("include_incident"),
      material_workspace_text("include_incident"), FALSE),
    shiny::textInput(
      ns("promotion_name"),
      label = if (isTRUE(workspace)) material_workspace_text("next_source_name") else transmission_text("promotion_name"),
      value = "",
      placeholder = transmission_text("promotion_placeholder"),
      updateOn = "blur"
    ),
    if (isTRUE(workspace)) shiny::checkboxInput(ns("rescale"), material_workspace_text("rescale"), FALSE),
    shiny::conditionalPanel(
      condition = if (isTRUE(workspace)) sprintf("input['%s'] === true", ns("rescale")) else "true",
    if (isTRUE(workspace)) material_light_level_ui(ns),
    htmltools::tags$div(
      class = "transmission-field-label",
      htmltools::tags$label(
        if (isTRUE(workspace)) material_workspace_text("target_level") else material_text("target_lux"),
        `for` = ns("promotion_lux")
      ),
      transmission_info_tooltip(
        ns,
        "promotion_lux_info",
        if (isTRUE(workspace)) material_workspace_text("light_level") else material_text("target_lux"),
        if (isTRUE(workspace)) material_workspace_text("rescale_help") else material_text("target_help")
      )
    ),
    shiny::numericInput(
      ns("promotion_lux"),
      label = NULL,
      value = NA_real_,
      min = 0
    )),
    shiny::uiOutput(ns("promotion_scenario")),
    htmltools::div(class = "material-save-actions",
    if (isTRUE(workspace)) shiny::actionButton(ns("save_result"),
      material_workspace_text("save_only"), icon = shiny::icon("floppy-disk"), class = "btn-primary"),
    shiny::actionButton(
      ns("promote"),
      label = if (isTRUE(workspace)) material_workspace_text("use_output") else transmission_text("promote_button"),
      icon = shiny::icon("level-up"),
      class = "btn-primary"
    ))
  )
  controls <- if (isTRUE(compact)) {
    htmltools::tags$div(
      class = "transmission-history-control-group",
      promotion_controls
    )
  } else {
    shiny::fluidRow(
      shiny::column(
        width = 12,
        class = "col-lg-6 transmission-history-column",
        promotion_controls
      )
    )
  }

  htmltools::tags$div(
    class = "transmission-history-controls",
    `aria-labelledby` = ns("heading"),
    htmltools::h3(id = ns("heading"), if (isTRUE(workspace)) material_workspace_text("next") else transmission_text("promote_heading")),
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
transmissionHistoryDetailsUI <- function(id, workspace = FALSE, help_ui = NULL) {
  ns <- shiny::NS(id)
  transmission_text <- if (isTRUE(workspace)) material_workspace_history_text else transmission_text
  combined <- htmltools::tagList(
    htmltools::h4(if (isTRUE(workspace)) material_workspace_text("history_combined") else material_text("cumulative")),
    htmltools::p(if (isTRUE(workspace)) material_workspace_text("history_combined_help") else material_text("cumulative_help")),
    if (isTRUE(workspace)) shiny::uiOutput(ns("path_plot_ui")),
    shiny::uiOutput(ns("cumulative_summary")))
  htmltools::tags$div(
    class = "transmission-history-details",
    htmltools::h4(transmission_text("history_tree")),
    htmltools::p(if (isTRUE(workspace)) material_workspace_text("history_help") else
      material_text("history_actions_help")),
    help_ui,
    shiny::uiOutput(ns("history_status")),
    htmltools::tags$div(
      class = "transmission-gt-scroll",
      tabindex = "0",
      `aria-label` = transmission_text("aria_history_table"),
      transmission_gt_output(ns("history_table"))
    ),
    shiny::uiOutput(ns("selected_node")),
    if (isTRUE(workspace)) htmltools::div(class = "material-path-views",
      shiny::tabsetPanel(id = ns("path_view"),
        shiny::tabPanel(material_workspace_text("history_saved"), value = "saved",
          shiny::uiOutput(ns("archive_section"))),
        shiny::tabPanel(material_workspace_text("history_combined"), value = "combined", combined))) else
      htmltools::tagList(combined, shiny::uiOutput(ns("archive_section")))
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
  response_curves,
  workspace = FALSE
) {
  transmission_text <- if (isTRUE(workspace)) material_workspace_history_text else transmission_text
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
    transmission_info_tooltip_server("promotion_lux_info")
    history <- shiny::reactiveVal(NULL)
    promotion_event <- shiny::reactiveVal(NULL)
    saved_event <- shiny::reactiveVal(NULL)
    restore_event <- shiny::reactiveVal(NULL)
    archive_node_id <- shiny::reactiveVal(NULL)
    prepared_export_bundle <- shiny::reactiveVal(NULL)
    action_sequence <- shiny::reactiveVal(0L)
    import_token <- shiny::reactiveVal(NULL)
    last_promoted_snapshot <- shiny::reactiveVal(NULL)
    last_promote_click <- shiny::reactiveVal(-Inf)
    last_promote_keyboard <- shiny::reactiveVal(-Inf)
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
            saved_event(NULL)
            last_promoted_snapshot(NULL)
            restore_event(NULL)
            archive_node_id(current$node_id)
            clear_snapshot()
            shiny::updateTextInput(session, "promotion_name", value = "")
            status(list(
              state = "current",
              message = transmission_text(
                "history_new_root",
                transmission_history_node_label(history(), current$node_id)
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

    output$promotion_scenario <- shiny::renderUI({
      current <- applied_snapshot()
      if (is.null(current)) return(NULL)
      htmltools::tagList(
        if (isTRUE(workspace)) htmltools::p(class = "material-promotion-default",
          material_workspace_text("preserve_lux", material_workspace_metric(material_photopic_lux(current$transmitted_spectrum)))),
        htmltools::tags$p(material_text(paste0(material_mode(current), "_model"))))
    })
    cumulative <- shiny::reactive({
      current <- history()
      shiny::req(current)
      selected <- archive_node_id()
      if (is.null(selected) || !selected %in% names(current$nodes))
        selected <- current$active_node_id
      calculate_material_cumulative(current, selected)
    })
    output$selected_node <- shiny::renderUI({
      current <- history()
      selected <- archive_node_id()
      shiny::req(current, selected, selected %in% names(current$nodes))
      htmltools::tags$p(
        class = "transmission-history-selection",
        role = "status",
        `aria-live` = "polite",
        htmltools::tags$strong(paste0(
          material_text("history_selected"),
          ": ",
          transmission_history_node_label(current, selected),
          " \u00b7 ",
          current$nodes[[selected]]$name
        ))
      )
    })
    output$cumulative_summary <- shiny::renderUI({
      if (material_cumulative_has_rescaling(cumulative())) {
        return(htmltools::tags$p(
          class = "transmission-preview-note",
          role = "status",
          if (isTRUE(workspace)) material_workspace_text("history_adjusted") else
            material_text("cumulative_rescaled")
        ))
      }
      htmltools::tagList(
        htmltools::tags$div(
          class = "transmission-gt-scroll",
          transmission_gt_output(session$ns("cumulative_material"))
        ),
        shiny::downloadButton(
          session$ns("cumulative_csv"),
          material_text("cumulative_download")
        )
      )
    })
    output$cumulative_material <- shiny::renderUI({
      shiny::req(!material_cumulative_has_rescaling(cumulative()))
      transmission_gt_html(material_cumulative_gt(
        cumulative()$material_metrics,
        material_text("cumulative_material")
      ))
    })
    output$cumulative_csv <- shiny::downloadHandler(
      filename = function() "Spectran-cumulative-material-effect.csv",
      content = function(file) {
        shiny::req(!material_cumulative_has_rescaling(cumulative()))
        write_transmission_csv(material_cumulative_export(cumulative()), file)
      }
    )

    selected_path_node <- shiny::reactive({
      current <- history()
      shiny::req(current)
      selected <- archive_node_id()
      if (is.null(selected) || !selected %in% names(current$nodes)) selected <- current$active_node_id
      selected
    })
    output$path_plot_ui <- shiny::renderUI({
      current <- history()
      shiny::req(current)
      path <- material_history_path(current, selected_path_node())
      if (length(path) < 2L)
        return(htmltools::p(material_workspace_text("path_empty")))
      htmltools::tagList(
        htmltools::h4(material_workspace_text("path_plot")),
        htmltools::p(class = "material-path-plot-note", material_workspace_text("path_plot_help")),
        if (length(path) > 4L)
          htmltools::p(class = "material-path-plot-note", material_workspace_text("path_plot_readability")),
        if (material_cumulative_has_rescaling(cumulative()))
          htmltools::p(class = "material-source-warning", material_workspace_text("path_plot_adjusted")),
        shiny::plotOutput(session$ns("path_plot"), height = "auto"),
        htmltools::div(class = "material-path-downloads",
          shiny::downloadButton(session$ns("path_plot_png"), material_workspace_text("path_plot_download")),
          shiny::downloadButton(session$ns("path_spectra_csv"), material_workspace_text("path_data_download"))))
    })
    path_plot_width <- shiny::reactive(session$clientData[[paste0("output_", session$ns("path_plot"), "_width")]] %||% 800)
    output$path_plot <- shiny::renderPlot({
      material_path_plot(history(), selected_path_node(),
        font_size = if (path_plot_width() < 500) 10 else 12,
        label_width = material_path_label_width(path_plot_width()), width_px = path_plot_width())
    }, height = function() {
      nodes <- history()$nodes[material_history_path(history(), selected_path_node())]
      wrap <- max(10L, material_path_label_width(path_plot_width()) - 8L)
      330 + 18 * sum(vapply(nodes, function(x) 2 + ceiling(nchar(x$name) / wrap), numeric(1)))
    }, alt = function() material_workspace_text("path_plot_help"), res = 96)
    output$path_plot_png <- shiny::downloadHandler(
      filename = function() paste0("Spectran-light-path-", selected_path_node(), ".png"),
      content = function(file) write_material_path_plot(history(), selected_path_node(), file))
    output$path_spectra_csv <- shiny::downloadHandler(
      filename = function() paste0("Spectran-light-path-", selected_path_node(), ".csv"),
      content = function(file) write_transmission_csv(material_path_spectra(history(), selected_path_node()), file))

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
        shiny::updateNumericInput(
          session,
          "promotion_lux",
          value = material_light_level(current$transmitted_spectrum,
            if (isTRUE(workspace)) input$level_metric %||% "photopic" else "photopic")
        )
        if (isTRUE(workspace)) shiny::updateCheckboxInput(session, "rescale", value = FALSE)
        filter_name <- current$metadata$filter_name
        if (is.null(filter_name) || !nzchar(trimws(filter_name))) {
          filter_name <- transmission_text("transmission_filter")
        }
        shiny::updateTextInput(
          session,
          "promotion_name",
          value = if (isTRUE(workspace) && !isTRUE(input$include_incident)) material_workspace_text("after_material", trimws(filter_name)) else paste0(
            current$incident_name,
            " \u00d7 ",
            trimws(filter_name)
          )
        )
      },
      ignoreInit = FALSE,
      ignoreNULL = TRUE
    )

    shiny::observeEvent(input$include_incident, {
      current <- applied_snapshot()
      shiny::req(isTRUE(workspace), current)
      name <- if (isTRUE(input$include_incident)) paste(current$incident_name, "\u00d7", current$metadata$filter_name) else
        material_workspace_text("after_material", current$metadata$filter_name)
      shiny::updateTextInput(session, "promotion_name", value = name)
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$level_metric, {
      current <- applied_snapshot()
      shiny::req(isTRUE(workspace), current)
      shiny::updateNumericInput(session, "promotion_lux",
        value = material_light_level(current$transmitted_spectrum, input$level_metric))
    }, ignoreInit = TRUE)

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
      if (isTRUE(workspace)) shinyjs::toggleState("save_result", can_promote_current())
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

    promote_current <- function(trigger = c("click", "keyboard"), activate = TRUE) {
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
        # A native click can arrive after the keyboard activation has already
        # promoted this snapshot. Preserve its success status without adding
        # a second node or showing a spurious unavailable error.
        if (
          !is.null(snapshot_token()) &&
            identical(snapshot_token(), last_promoted_snapshot())
        )
          return(invisible(NULL))
        status(list(
          state = "error",
          message = if (isTRUE(workspace)) material_workspace_text("save_unavailable") else transmission_text("promotion_unavailable")
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
          message = if (isTRUE(workspace)) material_workspace_text("save_name_required") else transmission_text("promotion_name_required")
        ))
        return(invisible(NULL))
      }

      promoted <- tryCatch(
        transmission_history_promote(
          history(),
          applied_snapshot(),
          trimws(name),
          target_lux = if (isTRUE(workspace) && !isTRUE(input$rescale)) NULL else if (isTRUE(workspace))
            material_photopic_lux(material_scale_light(applied_snapshot()$transmitted_spectrum,
              input$promotion_lux, input$level_metric %||% "photopic")) else input$promotion_lux,
          activate = activate
        ),
        error = function(error) error
      )
      if (inherits(promoted, "error")) {
        status(list(state = "error", message = conditionMessage(promoted)))
        return(invisible(NULL))
      }
      last_promoted_snapshot(snapshot_token())
      history(promoted$history)
      archive_node_id(promoted$node$node_id)
      if (!isTRUE(activate)) {
        mark_snapshot_archived()
        saved_event(list(node_id = promoted$node$node_id, token = snapshot_token()))
        status(list(state = "current", message = material_workspace_text("history_saved_status",
          promoted$node$name, transmission_history_node_label(history(), promoted$node$node_id))))
        return(invisible(NULL))
      }
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
          transmission_history_node_label(history(), promoted$node$node_id)
        )
      ))
      invisible(NULL)
    }

    shiny::observeEvent(input$promote, {
      promote_current("click")
    })
    if (isTRUE(workspace)) shiny::observeEvent(input$save_result, {
      promote_current("click", activate = FALSE)
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

    restore_node <- function(node_id) {
      current <- history()
      if (identical(node_id, current$active_node_id)) return(invisible(NULL))
      restored <- tryCatch(
        transmission_history_restore(current, node_id),
        error = function(error) error
      )
      if (inherits(restored, "error")) {
        status(list(state = "error", message = conditionMessage(restored)))
        return(invisible(NULL))
      }
      history(restored$history)
      archive_node_id(node_id)
      next_sequence <- action_sequence() + 1L
      provenance <- utils::modifyList(
        restored$node$provenance,
        list(restored_from_node = node_id)
      )
      event <- new_transmission_activation_event(
        action_sequence = next_sequence,
        change_type = "restore",
        node_id = node_id,
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
          transmission_history_node_label(history(), node_id),
          restored$node$name
        )
      ))
      invisible(NULL)
    }

    shiny::observeEvent(
      input$node_action,
      {
        action <- input$node_action
        current <- history()
        scalar_text <- function(x)
          is.character(x) && length(x) == 1L && !is.na(x)
        if (
          is.null(current) ||
            !is.list(action) ||
            !scalar_text(action$node) ||
            !scalar_text(action$action) ||
            !action$node %in% names(current$nodes) ||
            !action$action %in% c("show", "restore")
        )
          return(invisible(NULL))
        if (action$action == "show") {
          if (identical(action$node, archive_node_id())) return(invisible(NULL))
          archive_node_id(action$node)
        } else {
          if (identical(action$node, current$active_node_id))
            return(invisible(NULL))
          restore_node(action$node)
        }
        # The activated button becomes disabled after the table is redrawn.
        # Keep keyboard focus in its row on the short node label instead.
        focus_id <- session$ns(paste0(
          "node-label-",
          current$nodes[[action$node]]$sequence_id
        ))
        session$onFlushed(
          function() {
            shinyjs::runjs(paste0(
              "requestAnimationFrame(function(){requestAnimationFrame(function(){",
              "var node=document.getElementById(",
              encodeString(focus_id, quote = '"'),
              ");if(node)node.focus({preventScroll:true});});});"
            ))
          },
          once = TRUE
        )
      },
      ignoreInit = TRUE
    )

    status_ui <- shiny::reactive({
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
    output$status <- shiny::renderUI({
      if (isTRUE(workspace) && identical(status()$state, "current")) return(NULL)
      status_ui()
    })
    output$history_status <- shiny::renderUI(status_ui())

    output$history_table <- shiny::renderUI({
      current_history <- history()
      shiny::req(current_history)
      transmission_gt_html(transmission_history_gt(
        current_history,
        ns = session$ns,
        selected_node = archive_node_id(), workspace = workspace
      ))
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

    output$archive_section <- shiny::renderUI({
      current <- archived_snapshot()
      htmltools::tags$section(
        class = paste(
          "transmission-archive-section",
          if (is.null(current)) "is-empty"
        ),
        `aria-labelledby` = session$ns("archive_heading"),
        htmltools::h4(
          id = session$ns("archive_heading"),
          transmission_text("archive_heading")
        ),
        if (is.null(current)) {
          htmltools::p(class = "text-muted", if (isTRUE(workspace))
            material_workspace_text("history_source_help") else material_text("archive_source"))
        } else {
          htmltools::tagList(
            htmltools::p(transmission_text("archive_intro")),
            shiny::uiOutput(session$ns("archived_outputs"))
          )
        }
      )
    })

    output$archived_outputs <- shiny::renderUI({
      current <- archived_snapshot()
      shiny::req(current)
      transmission_text <- material_labeler(material_mode(current))
      filter_name <- current$metadata$filter_name
      if (is.null(filter_name) || !nzchar(trimws(filter_name))) {
        filter_name <- transmission_text("transmission_filter")
      }

      htmltools::tags$div(
        class = paste(
          "transmission-applied-results transmission-analysis-package",
          "transmission-archived-results"
        ),
        `aria-label` = if (isTRUE(workspace)) material_workspace_text("history_saved") else
          transmission_text("aria_archived_results"),
        shiny::uiOutput(session$ns("archived_metric_warnings")),
        shiny::uiOutput(session$ns("archived_plot_outputs")),
        if (material_mode(current) == "reflection")
          material_colour_preview_ui(current$filter),
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
          title_word_wrap_width = transmission_result_title_word_wrap_width(
            archived_plot_width
          ),
          plot_margin_right = transmission_archived_result_title_right_margin(
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
      transmission_text <- material_labeler(material_mode(archived_snapshot()))
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
      transmission_text <- material_labeler(material_mode(applied_snapshot()))
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
          stats::setNames("current", if (isTRUE(workspace))
            material_workspace_text("current_result", applied_snapshot()$metadata$filter_name) else
            transmission_text("export_current"))
        )
      }
      nodes <- archived_nodes()
      if (length(nodes) > 0L) {
        labels <- vapply(
          nodes,
          function(node) {
            paste0("N", node$sequence_id, " \u00b7 ", node$name)
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

    export_path_node <- shiny::reactive({
      choices <- unname(export_choices())
      selected <- input$export_result
      if (is.null(selected) || !selected %in% choices) selected <- if (length(choices)) choices[[1L]] else ""
      if (!startsWith(selected, "archive:")) return(NULL)
      node_id <- sub("^archive:", "", selected)
      if (!node_id %in% names(archived_nodes())) return(NULL)
      node_id
    })
    output$export_path_plot <- shiny::downloadHandler(
      filename = function() paste0("Spectran-light-path-", export_path_node(), ".png"),
      content = function(file) {
        shiny::req(export_path_node())
        write_material_path_plot(history(), export_path_node(), file)
      }, contentType = "image/png")

    bundle_choice_labels <- function() {
      transmission_text <- material_labeler(material_mode(export_snapshot()))
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
      transmission_text <- material_labeler(material_mode(export_snapshot()))
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
      panels <- htmltools::tagList(
        shiny::selectInput(
          session$ns("export_result"),
          label = transmission_text("export_result"),
          choices = choices,
          selected = selected
        ),
        (if (isTRUE(workspace)) htmltools::tags$details else htmltools::tags$section)(
          class = "transmission-export-settings",
          (if (isTRUE(workspace)) htmltools::tags$summary else htmltools::h4)(transmission_text("export_plot_settings")),
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
        (if (isTRUE(workspace)) htmltools::tags$details else htmltools::tags$section)(
          class = "transmission-export-bundle",
          (if (isTRUE(workspace)) htmltools::tags$summary else htmltools::h4)(transmission_text("export_bundle_heading")),
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
        (if (isTRUE(workspace)) htmltools::tags$div else htmltools::tags$details)(
          class = "transmission-export-quick",
          (if (isTRUE(workspace)) htmltools::h4 else htmltools::tags$summary)(
            transmission_text("export_quick_heading")
          ),
          htmltools::tags$div(
            class = "transmission-export-groups",
            (if (isTRUE(workspace)) htmltools::tags$details else htmltools::tags$section)(
              class = "transmission-export-group",
              open = if (isTRUE(workspace)) NA else NULL,
              (if (isTRUE(workspace)) htmltools::tags$summary else htmltools::h4)(transmission_text("export_figures")),
              if (isTRUE(workspace)) htmltools::tagList(
                transmission_download_control(session$ns, "export_path_plot",
                  material_workspace_text("path_plot_download"), icon = "image",
                  enabled = !is.null(export_path_node())),
                if (is.null(export_path_node())) htmltools::p(class = "help-block", material_workspace_text("path_export_hint"))),
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
            (if (isTRUE(workspace)) htmltools::tags$details else htmltools::tags$section)(
              class = "transmission-export-group",
              (if (isTRUE(workspace)) htmltools::tags$summary else htmltools::h4)(transmission_text("export_tables")),
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
            (if (isTRUE(workspace)) htmltools::tags$details else htmltools::tags$section)(
              class = "transmission-export-group",
              (if (isTRUE(workspace)) htmltools::tags$summary else htmltools::h4)(transmission_text("export_data")),
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
      if (isTRUE(workspace)) {
        return(htmltools::div(class = "material-export-workspace",
          panels[[1L]], panels[[4L]], panels[[2L]], panels[[3L]]))
      }
      panels
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
      filename = function() download_filename("material-coefficients"),
      content = function(file) {
        write_transmission_csv(
          material_coefficient_comparison(current_download()),
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
      filename = function() archive_download_filename("material-coefficients"),
      content = function(file) {
        write_transmission_csv(
          material_coefficient_comparison(current_archive_download()),
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
        export_download_filename(
          material_coefficient_filename(export_snapshot()),
          "png"
        )
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
        export_download_filename("material-coefficients-table", "png")
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
      filename = function() export_download_filename("material-coefficients"),
      content = function(file) {
        write_transmission_csv(
          material_coefficient_comparison(current_export_download()),
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
      "export_download_bundle",
      "path_plot_png", "path_spectra_csv", "export_path_plot"
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
      saved_event = shiny::reactive(saved_event()),
      restore_node = restore_node,
      history = shiny::reactive(history()),
      archived_snapshot = archived_snapshot,
      archive_node_id = shiny::reactive(archive_node_id()),
      can_promote = can_promote_current,
      can_download = can_download_current,
      action_sequence = shiny::reactive(action_sequence())
    )
  })
}
