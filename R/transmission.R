# Transmission import and preview module -----------------------------------

#' Tooltip definitions for transmission controls
#'
#' @return A named list of input labels, accessible trigger labels, and content.
#' @noRd
transmission_tooltip_specs <- function() {
  list(
    filter_file = list(
      label = transmission_text("csv_label"),
      trigger_label = transmission_text("csv_tooltip_label"),
      content = transmission_text("csv_tooltip")
    ),
    filter_name = list(
      label = transmission_text("filter_name"),
      trigger_label = transmission_text("filter_name_tooltip_label"),
      content = transmission_text("filter_name_tooltip")
    ),
    scale = list(
      label = transmission_text("scale"),
      trigger_label = transmission_text("scale_tooltip_label"),
      content = transmission_text("scale_tooltip")
    ),
    transmittance_type = list(
      label = transmission_text("type"),
      trigger_label = transmission_text("type_tooltip_label"),
      content = transmission_text("type_tooltip")
    ),
    scattering = list(
      label = transmission_text("scattering"),
      trigger_label = transmission_text("scattering_tooltip_label"),
      content = transmission_text("scattering_tooltip")
    ),
    measurement_geometry = list(
      label = transmission_text("geometry"),
      trigger_label = transmission_text("geometry_tooltip_label"),
      content = transmission_text("geometry_tooltip")
    ),
    measurement_angle = list(
      label = transmission_text("angle"),
      trigger_label = transmission_text("angle_tooltip_label"),
      content = transmission_text("angle_tooltip")
    )
  )
}

#' Passive-filter model limitation
#'
#' @return A character string for visible and audit-facing guidance.
#' @noRd
transmission_passive_filter_limitation <- function() {
  transmission_text("type_tooltip")
}

#' Information tooltip compatible with Spectran's Bootstrap 3 shell
#'
#' @param ns Shiny namespace function.
#' @param tooltip_id Identifier within the module.
#' @param label Accessible name for the information trigger.
#' @param content Tooltip content.
#'
#' @return HTML tags.
#' @noRd
transmission_info_tooltip <- function(ns, tooltip_id, label, content) {
  content_id <- ns(paste0(tooltip_id, "_content"))
  htmltools::tags$span(
    id = ns(paste0(tooltip_id, "_wrapper")),
    class = "transmission-info-tooltip",
    htmltools::tags$button(
      id = ns(tooltip_id),
      type = "button",
      class = "transmission-info-tooltip-trigger",
      `aria-label` = label,
      `aria-describedby` = content_id,
      shiny::icon("circle-info", `aria-hidden` = "true")
    ),
    htmltools::tags$span(
      id = content_id,
      class = "transmission-info-tooltip-content",
      role = "tooltip",
      content
    )
  )
}

#' Field label with a compact information tooltip
#'
#' @param ns Shiny namespace function.
#' @param input_id Identifier of the associated input.
#' @param tooltip_key Name from `transmission_tooltip_specs()`.
#'
#' @return HTML tags.
#' @noRd
transmission_field_label <- function(ns, input_id, tooltip_key = input_id) {
  spec <- transmission_tooltip_specs()[[tooltip_key]]
  if (is.null(spec)) {
    stop("Unknown transmission tooltip identifier.", call. = FALSE)
  }

  htmltools::tags$div(
    class = "transmission-field-label",
    htmltools::tags$label(`for` = ns(input_id), spec$label),
    transmission_info_tooltip(
      ns = ns,
      tooltip_id = paste0(tooltip_key, "_info"),
      label = spec$trigger_label,
      content = spec$content
    )
  )
}

#' Recognize Escape for dismissing a tooltip
#'
#' @param event Browser keyboard event properties supplied by `shinyjs`.
#'
#' @return A single logical value.
#' @noRd
is_transmission_escape_key <- function(event) {
  if (!is.list(event) || isTRUE(event[["repeat"]])) {
    return(FALSE)
  }
  key <- if (is.null(event$key)) "" else as.character(event$key[[1]])
  code <- if (is.null(event$code)) "" else as.character(event$code[[1]])
  which <- if (is.null(event$which)) NA_integer_ else
    suppressWarnings(as.integer(event$which[[1]]))
  identical(key, "Escape") ||
    identical(code, "Escape") ||
    identical(which, 27L)
}

#' Register dismiss and re-arm behavior for an information tooltip
#'
#' @param tooltip_id Identifier within the current Shiny module.
#'
#' @return `NULL`, invisibly.
#' @noRd
transmission_info_tooltip_server <- function(tooltip_id) {
  wrapper_id <- paste0(tooltip_id, "_wrapper")

  shinyjs::onevent(
    event = "keydown",
    id = tooltip_id,
    expr = function(event) {
      if (is_transmission_escape_key(event)) {
        shinyjs::addClass(id = wrapper_id, class = "is-dismissed")
      }
    },
    properties = c("key", "code", "repeat", "which")
  )
  shinyjs::onevent(
    event = "blur",
    id = tooltip_id,
    expr = {
      shinyjs::removeClass(id = wrapper_id, class = "is-dismissed")
    }
  )
  shinyjs::onevent(
    event = "click",
    id = tooltip_id,
    expr = {
      shinyjs::removeClass(id = wrapper_id, class = "is-dismissed")
    }
  )

  invisible(NULL)
}

#' Recognize a non-repeated keyboard activation
#'
#' @param event Browser keyboard event properties supplied by `shinyjs`.
#'
#' @return A single logical value.
#' @noRd
is_transmission_activation_key <- function(event) {
  if (!is.list(event) || isTRUE(event[["repeat"]])) {
    return(FALSE)
  }
  key <- if (is.null(event$key)) "" else as.character(event$key[[1]])
  code <- if (is.null(event$code)) "" else as.character(event$code[[1]])
  which <- if (is.null(event$which)) NA_integer_ else
    suppressWarnings(as.integer(event$which[[1]]))
  key %in%
    c("Enter", " ", "Spacebar") ||
    code %in% c("Enter", "Space") ||
    which %in% c(13L, 32L)
}

#' Physical transmittance scale with an exact zero baseline
#'
#' @return A ggplot2 continuous-position scale.
#' @noRd
transmission_fraction_y_scale <- function() {
  ggplot2::scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2),
    labels = scales::label_number(accuracy = 0.1),
    expand = ggplot2::expansion(mult = c(0, 0))
  )
}

#' Translated labels for the shared transmission CSV core
#'
#' @return A label list accepted by the shared CSV module.
#' @noRd
transmission_csv_settings_labels <- function() {
  labels <- spectral_csv_settings_labels(transmission_text("csv_value"))
  labels$heading <- transmission_text("csv_heading")
  labels$skip <- transmission_text("csv_skip")
  labels$wavelength <- transmission_text("csv_wavelength")
  labels$value <- transmission_text("csv_value")
  labels$separator <- transmission_text("csv_separator")
  labels$whitespace <- transmission_text("csv_whitespace")
  labels$decimal <- transmission_text("csv_decimal")
  labels$decimal_choices <- c(
    stats::setNames(".", transmission_text("csv_point")),
    stats::setNames(",", transmission_text("csv_comma"))
  )
  labels$header <- transmission_text("csv_header")
  labels$reset <- transmission_text("csv_reset")
  labels$invalid_columns <- transmission_text("csv_invalid_columns")
  labels
}

#' Input controls for selecting a transmission spectrum
#'
#' @param ns Shiny namespace function.
#' @param csv_labels Shared CSV-settings labels.
#' @param default_source Initial source selection.
#' @param heading Whether to show the wizard-step heading.
#' @param csv_settings_open Whether advanced CSV settings start expanded.
#'
#' @return Shiny UI tags.
#' @noRd
transmission_source_inputs_ui <- function(
  ns,
  csv_labels,
  default_source,
  heading = TRUE,
  csv_settings_open = TRUE
) {
  htmltools::tagList(
    if (isTRUE(heading)) htmltools::h3(transmission_text("choose_heading")),
    htmltools::p(
      class = "transmission-section-intro",
      transmission_text("choose_intro")
    ),
    shiny::radioButtons(
      ns("input_source"),
      label = transmission_text("source_label"),
      choices = c(
        stats::setNames("catalogue", transmission_text("source_catalogue")),
        stats::setNames("upload", transmission_text("source_upload"))
      ),
      selected = default_source,
      inline = TRUE
    ),
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] === 'catalogue'", ns("input_source")),
      shiny::selectInput(
        ns("catalogue_collection"),
        label = transmission_text("catalogue_collection"),
        choices = c(
          stats::setNames("featured", transmission_text("catalogue_featured")),
          stats::setNames("all", transmission_text("catalogue_all")),
          stats::setNames(
            "facade_windows",
            transmission_text("catalogue_facade")
          ),
          stats::setNames(
            "spitschan2019",
            transmission_text("catalogue_spitschan")
          )
        ),
        selected = "featured"
      ),
      shiny::selectInput(
        ns("catalogue_category"),
        label = transmission_text("catalogue_category"),
        choices = stats::setNames(
          "all",
          transmission_text("catalogue_all_categories")
        ),
        selected = "all"
      ),
      shiny::selectizeInput(
        ns("catalogue_filter"),
        label = transmission_text("catalogue_filter"),
        choices = character(),
        options = list(
          placeholder = transmission_text("catalogue_placeholder")
        )
      ),
      shiny::uiOutput(ns("catalogue_info"))
    ),
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] === 'upload'", ns("input_source")),
      transmission_field_label(ns, "filter_file"),
      shiny::fileInput(
        ns("filter_file"),
        label = NULL,
        accept = c(".csv", "text/csv", "text/plain"),
        width = "100%",
        buttonLabel = transmission_text("file_browse"),
        placeholder = transmission_text("file_none")
      ),
      shiny::uiOutput(ns("file_transport_status")),
      shiny::downloadButton(
        ns("download_template"),
        label = transmission_text("template_download"),
        class = "transmission-template-download"
      ),
      htmltools::tags$details(
        class = "transmission-csv-settings",
        open = if (isTRUE(csv_settings_open)) NA else NULL,
        htmltools::tags$summary(transmission_text("csv_settings")),
        spectral_csv_settingsUI(ns("csv"), labels = csv_labels)
      )
    )
  )
}

#' Input controls describing transmission and measurement conditions
#'
#' @param ns Shiny namespace function.
#' @param heading Whether to show the wizard-step heading.
#'
#' @return Shiny UI tags.
#' @noRd
transmission_metadata_inputs_ui <- function(ns, heading = TRUE) {
  htmltools::tagList(
    if (isTRUE(heading))
      htmltools::h3(transmission_text("measurement_heading")),
    if (isTRUE(heading)) {
      htmltools::p(
        class = "transmission-section-intro",
        transmission_text("measurement_intro")
      )
    },
    transmission_field_label(ns, "filter_name"),
    shiny::textInput(
      ns("filter_name"),
      label = NULL,
      value = "Uploaded filter"
    ),
    transmission_field_label(ns, "scale"),
    shiny::selectInput(
      ns("scale"),
      label = NULL,
      choices = c(
        stats::setNames("", transmission_text("scale_choose")),
        stats::setNames("fraction", transmission_text("scale_fraction")),
        stats::setNames("percent", transmission_text("scale_percent"))
      ),
      selected = "fraction"
    ),
    transmission_field_label(ns, "transmittance_type"),
    shiny::selectInput(
      ns("transmittance_type"),
      label = NULL,
      choices = c(
        stats::setNames("", transmission_text("type_choose")),
        stats::setNames("total", transmission_text("type_total")),
        stats::setNames("internal", transmission_text("type_internal")),
        stats::setNames("unknown", transmission_text("type_unknown"))
      ),
      selected = "total"
    ),
    shiny::uiOutput(ns("type_acknowledgement")),
    transmission_field_label(ns, "scattering"),
    shiny::selectInput(
      ns("scattering"),
      label = NULL,
      choices = c(
        stats::setNames("no", transmission_text("no")),
        stats::setNames("yes", transmission_text("yes")),
        stats::setNames("unknown", transmission_text("type_unknown"))
      ),
      selected = "no"
    ),
    transmission_field_label(ns, "measurement_geometry"),
    shiny::textInput(
      ns("measurement_geometry"),
      label = NULL,
      value = "",
      placeholder = transmission_text("geometry_placeholder")
    ),
    transmission_field_label(ns, "measurement_angle"),
    shiny::textInput(
      ns("measurement_angle"),
      label = NULL,
      value = "",
      placeholder = transmission_text("angle_placeholder")
    ),
    shiny::uiOutput(ns("scattering_acknowledgement"))
  )
}

#' Input controls for completing the calculation grid
#'
#' @param ns Shiny namespace function.
#' @param heading Whether to show the wizard-step heading.
#'
#' @return Shiny UI tags.
#' @noRd
transmission_normalization_inputs_ui <- function(ns, heading = TRUE) {
  htmltools::tagList(
    if (isTRUE(heading))
      htmltools::h3(transmission_text("normalization_heading")),
    if (isTRUE(heading)) {
      htmltools::p(
        class = "transmission-section-intro",
        transmission_text("normalization_intro")
      )
    },
    shiny::uiOutput(ns("coverage_controls"))
  )
}

#' Namespaced output identifier for one repeated live preview
#'
#' @param base Base output identifier.
#' @param context Preview context, or an empty string for the review layout.
#'
#' @return A single identifier.
#' @noRd
transmission_preview_output_id <- function(base, context = "") {
  if (!nzchar(context)) base else paste(base, context, sep = "_")
}

#' Live normalization preview used beside every input tab
#'
#' @param ns Shiny namespace function.
#' @param context Unique tab context for repeated output identifiers.
#'
#' @return Shiny UI tags.
#' @noRd
transmission_preview_ui <- function(ns, context = "") {
  htmltools::tagList(
    htmltools::h3(transmission_text("preview_heading")),
    shiny::uiOutput(ns(transmission_preview_output_id(
      "preview_outputs",
      context
    )))
  )
}

#' Analysis-style tab body with a live normalization preview
#'
#' @param ns Shiny namespace function.
#' @param controls Input controls for the active tab.
#' @param context Unique preview context.
#' @param footer Guided navigation controls rendered below both columns.
#' @param readiness Whether to render the readiness panel below this tab.
#'
#' @return Shiny UI tags.
#' @noRd
transmission_input_tab_ui <- function(
  ns,
  controls,
  context,
  footer = NULL,
  readiness = FALSE
) {
  htmltools::tags$div(
    class = "transmission-tab-pane",
    shiny::fluidRow(
      shiny::column(
        width = 12,
        class = "col-lg-5 transmission-form-column",
        controls
      ),
      shiny::column(
        width = 12,
        class = paste(
          "col-lg-7 transmission-preview-column",
          "transmission-tab-preview"
        ),
        transmission_preview_ui(ns, context = context)
      )
    ),
    if (isTRUE(readiness)) shiny::uiOutput(ns("readiness")),
    footer
  )
}

#' Build a live construction plot from any available preparation state
#'
#' @param preparation A transmission preparation or `NULL` before parsing.
#'
#' @return A ggplot object with a fixed physical transmittance domain.
#' @noRd
transmission_construction_plot <- function(preparation = NULL) {
  status_colours <- c(
    supplied = "#1b1b1b",
    interpolated = "#0072b2",
    acknowledged_large_gap = "#d55e00",
    large_gap_unacknowledged = "#cc79a7",
    assumed_lower_tail = "#009e73",
    assumed_upper_tail = "#56b4e9",
    carried_lower_tail = "#007f5f",
    carried_upper_tail = "#2b8cbe",
    missing_lower_tail = "#8c510a",
    missing_upper_tail = "#bf812d",
    outside_range = "#777777"
  )
  status_shapes <- c(
    supplied = 16,
    interpolated = 17,
    acknowledged_large_gap = 15,
    large_gap_unacknowledged = 4,
    assumed_lower_tail = 18,
    assumed_upper_tail = 8,
    carried_lower_tail = 15,
    carried_upper_tail = 17,
    missing_lower_tail = 1,
    missing_upper_tail = 2,
    outside_range = 3
  )
  plot_labels <- transmission_status_label(names(status_colours))
  plot_labels <- gsub(" \\(", "\n(", plot_labels)
  names(plot_labels) <- names(status_colours)

  frame <- tibble::tibble(
    wavelength_nm = c(380, 780),
    transmittance = c(0, 1)
  )
  plot <- ggplot2::ggplot() +
    ggplot2::geom_blank(
      data = frame,
      ggplot2::aes(
        x = .data$wavelength_nm,
        y = .data$transmittance
      )
    ) +
    ggplot2::annotate(
      "rect",
      xmin = 380,
      xmax = 780,
      ymin = -Inf,
      ymax = Inf,
      fill = "grey95"
    ) +
    ggplot2::geom_vline(
      xintercept = c(380, 780),
      linetype = "dashed",
      colour = "grey45"
    )

  if (
    is.null(preparation) ||
      !inherits(preparation, "transmission_preparation") ||
      is.null(preparation$normalized)
  ) {
    return(
      plot +
        ggplot2::annotate(
          "text",
          x = 580,
          y = 0.5,
          label = transmission_text("preview_no_curve"),
          colour = "grey35"
        ) +
        ggplot2::labs(
          x = transmission_text("plot_wavelength"),
          y = transmission_text("plot_transmittance")
        ) +
        ggplot2::scale_x_continuous(
          limits = c(380, 780),
          breaks = c(380, 480, 580, 680, 780),
          expand = ggplot2::expansion(mult = c(0.02, 0.02))
        ) +
        transmission_fraction_y_scale() +
        transmission_plot_theme()
    )
  }

  normalized <- preparation$normalized
  completed <- preparation$completed
  if (is.null(completed)) {
    line_data <- normalized[
      is.finite(normalized$wavelength_nm) &
        is.finite(normalized$transmittance) &
        normalized$transmittance >= 0 &
        normalized$transmittance <= 1,
      c("wavelength_nm", "transmittance"),
      drop = FALSE
    ]
    display_points <- line_data
    matched <- match(
      display_points$wavelength_nm,
      normalized$wavelength_nm
    )
    display_points$status <- ifelse(
      normalized$within_calculation_range[matched],
      "supplied",
      "outside_range"
    )
  } else {
    line_data <- completed[, c("wavelength_nm", "transmittance"), drop = FALSE]
    outside_points <- normalized[
      !normalized$within_calculation_range,
      c("wavelength_nm", "transmittance"),
      drop = FALSE
    ]
    if (nrow(outside_points) > 0L) {
      outside_points$status <- "outside_range"
    }
    display_points <- completed[,
      c("wavelength_nm", "transmittance", "status"),
      drop = FALSE
    ]
    if (nrow(outside_points) > 0L) {
      display_points <- rbind(display_points, outside_points)
    }
  }
  display_points$status <- factor(
    display_points$status,
    levels = names(status_colours)
  )

  plot <- plot +
    ggridges::geom_ridgeline_gradient(
      data = line_data,
      ggplot2::aes(
        x = .data$wavelength_nm,
        y = 0,
        height = .data$transmittance,
        fill = .data$wavelength_nm
      ),
      inherit.aes = FALSE,
      scale = 1,
      colour = NA,
      alpha = 0.35,
      na.rm = TRUE
    ) +
    ggplot2::scale_fill_gradientn(
      colours = transmission_spectral_palette(),
      guide = "none"
    ) +
    ggplot2::geom_line(
      data = line_data,
      ggplot2::aes(
        x = .data$wavelength_nm,
        y = .data$transmittance,
        group = 1
      ),
      colour = "grey35",
      linewidth = 0.9,
      na.rm = TRUE
    )

  has_status_points <- nrow(display_points) > 0L &&
    any(!is.na(display_points$status))
  if (has_status_points) {
    plot <- plot +
      ggplot2::geom_point(
        data = display_points,
        ggplot2::aes(
          x = .data$wavelength_nm,
          y = .data$transmittance,
          colour = .data$status,
          shape = .data$status
        ),
        size = 1.45,
        stroke = 0.8,
        na.rm = TRUE
      )
  }

  invalid_physical <- sum(
    !is.finite(normalized$transmittance) |
      normalized$transmittance < 0 |
      normalized$transmittance > 1
  )
  if (invalid_physical > 0L) {
    plot <- plot +
      ggplot2::annotate(
        "label",
        x = 580,
        y = 0.06,
        label = paste0(
          if (invalid_physical == 1L) {
            transmission_text("plot_invalid_one")
          } else {
            transmission_text("plot_invalid_many", invalid_physical)
          }
        ),
        size = 3,
        colour = "#8a1c1c",
        fill = "#fff2f2"
      )
  }

  plot <- plot +
    ggplot2::labs(
      x = transmission_text("plot_wavelength"),
      y = transmission_text("plot_transmittance")
    )
  if (has_status_points) {
    plot <- plot +
      ggplot2::labs(
        colour = transmission_text("plot_construction_legend"),
        shape = transmission_text("plot_construction_legend")
      ) +
      ggplot2::scale_colour_manual(
        values = status_colours,
        breaks = names(status_colours),
        labels = plot_labels,
        drop = TRUE,
        na.translate = FALSE
      ) +
      ggplot2::scale_shape_manual(
        values = status_shapes,
        breaks = names(status_shapes),
        labels = plot_labels,
        drop = TRUE,
        na.translate = FALSE
      ) +
      ggplot2::guides(
        colour = ggplot2::guide_legend(
          ncol = 2,
          byrow = TRUE,
          title.position = "top",
          title.hjust = 0
        ),
        shape = ggplot2::guide_legend(
          ncol = 2,
          byrow = TRUE,
          title.position = "top",
          title.hjust = 0
        )
      )
  }

  plot +
    ggplot2::scale_x_continuous(
      breaks = c(380, 480, 580, 680, 780),
      expand = ggplot2::expansion(mult = c(0.02, 0.06))
    ) +
    transmission_fraction_y_scale() +
    transmission_plot_theme(font_size = 13) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "vertical",
      legend.justification = "left",
      legend.box = "vertical",
      legend.box.just = "left",
      legend.text = ggplot2::element_text(size = 9.5),
      legend.title = ggplot2::element_text(size = 10, face = "bold")
    )
}

#' Transmission-spectrum import and normalization UI
#'
#' @param id Shiny module identifier.
#' @param default_source Initial source chooser value. Use `upload` for the
#'   isolated review app and `catalogue` for the integrated page.
#'
#' @return Shiny UI tags.
#' @noRd
transmissionUI <- function(
  id,
  default_source = c("upload", "catalogue"),
  layout = c("review", "tabs")
) {
  default_source <- match.arg(default_source)
  layout <- match.arg(layout)
  ns <- shiny::NS(id)
  csv_labels <- transmission_csv_settings_labels()

  htmltools::tags$div(
    class = "spectran-transmission-module",
    htmltools::tags$style(htmltools::HTML(
      paste(
        ".spectran-transmission-module {",
        "  max-width: 100%; overflow-x: clip; padding: 18px;",
        "  border-radius: 4px; background: #fff;",
        "  box-shadow: 0 1px 1px rgb(0 0 0 / 10%);",
        "}",
        ".spectran-transmission-module * { box-sizing: border-box; }",
        ".spectran-transmission-module .btn-primary {",
        "  color: #111; background: #f8e350; border-color: #8d7d00;",
        "}",
        ".spectran-transmission-module .btn-primary:hover,",
        ".spectran-transmission-module .btn-primary:active {",
        "  color: #111; background: #ecd538; border-color: #6f6300;",
        "}",
        ".spectran-transmission-module .transmission-readiness {",
        "  margin: 0 0 16px; padding: 12px 14px; border: 1px solid #bbb;",
        "  border-left-width: 5px; border-radius: 4px; background: #f7f7f7;",
        "}",
        ".spectran-transmission-module .transmission-readiness.is-ready {",
        "  border-left-color: #2e7d32; background: #f1f8f1;",
        "}",
        ".spectran-transmission-module .transmission-readiness.not-ready {",
        "  border-left-color: #b26a00; background: #fff8e8;",
        "}",
        ".spectran-transmission-module .transmission-readiness ul {",
        "  margin: 8px 0 0; padding-left: 20px;",
        "}",
        ".spectran-transmission-module .transmission-readiness-section {",
        "  margin-top: 10px;",
        "}",
        ".spectran-transmission-module .transmission-readiness-heading {",
        "  display: block; margin-bottom: 2px;",
        "}",
        ".spectran-transmission-module .transmission-readiness-notes {",
        "  padding-top: 8px; border-top: 1px solid #ccc;",
        "}",
        ".spectran-transmission-module .transmission-template-download {",
        "  max-width: 100%; height: auto; min-height: 34px;",
        "  white-space: normal; line-height: 1.25; text-align: left;",
        "  overflow-wrap: anywhere;",
        "}",
        ".spectran-transmission-module .transmission-catalogue-info {",
        "  margin: 10px 0 14px; padding: 10px 12px;",
        "  border-left: 4px solid #2874a6; background: #f2f7fb;",
        "  overflow-wrap: anywhere;",
        "}",
        ".spectran-transmission-module .transmission-catalogue-info p {",
        "  margin: 5px 0 0;",
        "}",
        ".spectran-transmission-module .transmission-help {",
        "  color: #4b4b4b; margin-top: -6px; margin-bottom: 12px;",
        "}",
        ".spectran-transmission-module .transmission-field-label {",
        "  display: flex; align-items: center; gap: 0.35em;",
        "  position: relative; margin-bottom: 5px; font-weight: 700;",
        "}",
        ".spectran-transmission-module .transmission-field-label label {",
        "  margin: 0;",
        "}",
        ".spectran-transmission-module .transmission-info-tooltip {",
        "  position: static; display: inline-flex; align-items: center;",
        "}",
        ".spectran-transmission-module .transmission-info-tooltip-trigger {",
        "  display: inline-flex; align-items: center; justify-content: center;",
        "  width: 1.65em; height: 1.65em; padding: 0; border: 0;",
        "  border-radius: 50%; background: transparent; color: #24527a;",
        "  font-size: 0.9em; line-height: 1;",
        "}",
        ".spectran-transmission-module .transmission-info-tooltip-trigger:hover {",
        "  background: #e8f1f8; color: #183b59;",
        "}",
        ".spectran-transmission-module .transmission-info-tooltip-trigger:focus,",
        ".spectran-transmission-module .transmission-info-tooltip-trigger:focus-visible {",
        "  outline: 3px solid #005fcc; outline-offset: 2px;",
        "  background: #e8f1f8; color: #183b59;",
        "}",
        ".spectran-transmission-module .transmission-info-tooltip-content {",
        "  position: absolute; z-index: 1050; top: 100%;",
        "  left: 0; width: min(22rem, calc(100vw - 30px));",
        "  padding: 9px 11px; border-radius: 4px;",
        "  background: #222; color: #fff; font-size: 13px;",
        "  font-weight: 400; line-height: 1.4; text-align: left;",
        "  overflow-wrap: anywhere; opacity: 0; pointer-events: none;",
        "  clip: rect(0 0 0 0); clip-path: inset(50%);",
        "  transform: none;",
        "}",
        ".spectran-transmission-module .transmission-info-tooltip:hover",
        "  .transmission-info-tooltip-content,",
        ".spectran-transmission-module .transmission-info-tooltip-trigger:focus",
        "  + .transmission-info-tooltip-content,",
        ".spectran-transmission-module .transmission-info-tooltip-trigger:focus-visible",
        "  + .transmission-info-tooltip-content {",
        "  opacity: 1; clip: auto; clip-path: none; pointer-events: auto;",
        "}",
        ".spectran-transmission-module .transmission-info-tooltip.is-dismissed",
        "  .transmission-info-tooltip-content {",
        "  opacity: 0; clip: rect(0 0 0 0); clip-path: inset(50%);",
        "  pointer-events: none;",
        "}",
        ".spectran-transmission-module .transmission-apply-section {",
        "  margin-top: 24px;",
        "}",
        ".spectran-transmission-module .transmission-source-summary,",
        ".spectran-transmission-module .transmission-apply-status,",
        ".spectran-transmission-module .transmission-applied-note {",
        "  margin: 12px 0; padding: 10px 12px; border-left: 4px solid #777;",
        "  background: #f5f5f5;",
        "}",
        ".spectran-transmission-module .transmission-apply-status.is-current {",
        "  border-left-color: #2e7d32; background: #f1f8f1;",
        "}",
        ".spectran-transmission-module .transmission-apply-status.is-archived {",
        "  border-left-color: #2874a6; background: #f2f7fb;",
        "}",
        ".spectran-transmission-module .transmission-apply-status.is-stale {",
        "  border-left-color: #b26a00; background: #fff8e8;",
        "}",
        ".spectran-transmission-module .transmission-apply-status.apply-error,",
        ".spectran-transmission-module .source-unavailable {",
        "  border-left-color: #b71c1c; background: #fff2f2;",
        "}",
        ".spectran-transmission-module .transmission-apply-section .action-button:focus,",
        ".spectran-transmission-module .transmission-apply-section .action-button:focus-visible {",
        "  outline: 3px solid #005fcc; outline-offset: 3px;",
        "  box-shadow: 0 0 0 2px #fff;",
        "}",
        ".spectran-transmission-module .transmission-applied-results {",
        "  margin-top: 18px;",
        "}",
        ".spectran-transmission-module .transmission-applied-results.is-stale {",
        "  opacity: 0.52;",
        "}",
        ".spectran-transmission-module .transmission-applied-results.is-archived {",
        "  opacity: 1;",
        "}",
        ".spectran-transmission-module .transmission-history-section {",
        "  margin-top: 24px;",
        "}",
        ".spectran-transmission-module .transmission-archive-section {",
        "  margin: 20px 0; padding: 16px; border: 1px solid #c9d7e2;",
        "  border-radius: 4px; background: #f7fbfe;",
        "}",
        ".spectran-transmission-module .transmission-archive-section.is-empty {",
        "  background: #f7f7f7; border-color: #ddd;",
        "}",
        ".spectran-transmission-module .transmission-archive-section h4 {",
        "  margin-top: 0;",
        "}",
        ".spectran-transmission-module .transmission-archived-results {",
        "  margin-top: 14px; padding-top: 12px; border-top: 1px solid #c9d7e2;",
        "}",
        ".spectran-transmission-module .transmission-history-status {",
        "  margin: 12px 0; padding: 10px 12px; border-left: 4px solid #777;",
        "  background: #f5f5f5;",
        "}",
        ".spectran-transmission-module .transmission-history-status.history-current {",
        "  border-left-color: #2e7d32; background: #f1f8f1;",
        "}",
        ".spectran-transmission-module .transmission-history-status.history-error {",
        "  border-left-color: #b71c1c; background: #fff2f2;",
        "}",
        ".spectran-transmission-module .transmission-download-grid {",
        "  display: flex; flex-wrap: wrap; gap: 8px; align-items: stretch;",
        "  box-sizing: border-box; max-width: 100%; padding: 7px;",
        "}",
        ".spectran-transmission-module .transmission-download-control {",
        "  white-space: normal; text-align: left;",
        "}",
        ".spectran-transmission-module .transmission-download-wrapper {",
        "  display: inline-flex; max-width: 100%;",
        "}",
        paste0(
          ".spectran-transmission-module .transmission-export-bundle ",
          ".transmission-download-wrapper {"
        ),
        "  display: flex; width: 100%;",
        "}",
        ".spectran-transmission-module .transmission-download-target {",
        "  display: none !important;",
        "}",
        ".spectran-transmission-module .transmission-download-control.disabled {",
        "  pointer-events: none; opacity: 0.5;",
        "}",
        ".transmission-source-import {",
        "  max-width: 100%; white-space: normal; text-align: left;",
        "}",
        ".transmission-import-confirmation {",
        "  max-width: 48rem; margin: 12px 0; overflow-wrap: anywhere;",
        "}",
        ".transmission-import-confirmation h4 { margin-top: 0; }",
        ".transmission-import-confirmation-actions {",
        "  display: flex; flex-wrap: wrap; gap: 8px; margin-top: 12px;",
        "}",
        ".transmission-source-import-feedback {",
        "  max-width: 48rem; margin: 10px 0; padding: 9px 11px;",
        "  border-left: 4px solid #777; background: #f5f5f5;",
        "}",
        ".transmission-source-import-feedback.import-warning {",
        "  border-left-color: #b26a00; background: #fff8e8;",
        "}",
        ".transmission-source-import-feedback.import-current {",
        "  border-left-color: #2e7d32; background: #f1f8f1;",
        "}",
        ".transmission-source-import:focus,",
        ".transmission-source-import:focus-visible,",
        ".transmission-import-confirmation .action-button:focus,",
        ".transmission-import-confirmation .action-button:focus-visible {",
        "  outline: 3px solid #005fcc; outline-offset: 3px;",
        "  box-shadow: 0 0 0 2px #fff;",
        "}",
        ".spectran-transmission-module .transmission-history-section .action-button:focus,",
        ".spectran-transmission-module .transmission-history-section .action-button:focus-visible,",
        ".spectran-transmission-module .transmission-download-control:focus,",
        ".spectran-transmission-module .transmission-download-control:focus-visible {",
        "  outline: 3px solid #005fcc; outline-offset: 3px;",
        "  box-shadow: 0 0 0 2px #fff;",
        "}",
        ".spectran-transmission-module .transmission-metric-warnings {",
        "  margin: 12px 0; padding: 10px 12px; border-left: 4px solid #b26a00;",
        "  background: #fff8e8;",
        "}",
        ".spectran-transmission-module .transmission-metric-warnings p {",
        "  margin: 6px 0 8px;",
        "}",
        ".spectran-transmission-module .transmission-warning-details {",
        "  max-width: 100%; overflow-wrap: anywhere;",
        "}",
        ".spectran-transmission-module .transmission-warning-details summary {",
        "  cursor: pointer; font-weight: 600;",
        "}",
        ".spectran-transmission-module .transmission-warning-group-heading {",
        "  display: block; margin-top: 10px;",
        "}",
        ".spectran-transmission-module .transmission-warning-details ul {",
        "  margin: 4px 0 8px; padding-left: 20px;",
        "}",
        ".spectran-transmission-module .transmission-table-scroll {",
        "  display: block; max-width: 100%; width: 100%; overflow-x: auto;",
        "  box-sizing: border-box; padding: 3px;",
        "  -webkit-overflow-scrolling: touch;",
        "}",
        ".spectran-transmission-module .transmission-table-scroll:focus,",
        ".spectran-transmission-module .transmission-table-scroll:focus-visible {",
        "  outline: none; box-shadow: inset 0 0 0 3px #005fcc;",
        "}",
        ".spectran-transmission-module .transmission-table-scroll table {",
        "  width: max-content; min-width: 100%; margin-bottom: 4px;",
        "}",
        ".spectran-transmission-module .transmission-table-scroll th {",
        "  white-space: normal; min-width: 7.5em; vertical-align: bottom;",
        "}",
        ".spectran-transmission-module .transmission-table-scroll td {",
        "  white-space: nowrap;",
        "}",
        ".spectran-transmission-module .transmission-gt-scroll {",
        "  display: block; max-width: 100%; width: 100%; overflow-x: auto;",
        "  box-sizing: border-box; padding: 4px;",
        "  -webkit-overflow-scrolling: touch;",
        "}",
        ".spectran-transmission-module .transmission-gt-scroll:focus,",
        ".spectran-transmission-module .transmission-gt-scroll:focus-visible {",
        "  outline: none; box-shadow: inset 0 0 0 3px #005fcc;",
        "}",
        ".spectran-transmission-module .transmission-gt-scroll .gt_table {",
        "  max-width: 100%;",
        "}",
        ".spectran-transmission-module .transmission-preview-note {",
        "  padding: 10px 12px; border-left: 4px solid #777; background: #f5f5f5;",
        "}",
        ".spectran-transmission-module .transmission-tabs > .tabbable > .nav-tabs {",
        "  display: flex; gap: 4px; margin-bottom: 0; padding: 3px 4px 0;",
        "  border-bottom: 1px solid #d5dce2; overflow-x: auto; overflow-y: hidden;",
        "}",
        ".spectran-transmission-module .transmission-tabs > .tabbable > .nav-tabs > li {",
        "  flex: 0 0 auto; margin-bottom: -1px;",
        "}",
        ".spectran-transmission-module .transmission-tabs > .tabbable > .nav-tabs > li > a {",
        "  color: #333; font-weight: 600; padding: 10px 15px;",
        "}",
        ".spectran-transmission-module .transmission-tabs > .tabbable > .nav-tabs > li.active {",
        "  border-top: 0;",
        "}",
        ".spectran-transmission-module .transmission-tabs > .tabbable > .nav-tabs > li.active > a {",
        "  color: #111; background: #fff;",
        "}",
        ".spectran-transmission-module .transmission-tabs > .tabbable > .nav-tabs > li > a:focus,",
        ".spectran-transmission-module .transmission-tabs > .tabbable > .nav-tabs > li > a:focus-visible {",
        "  outline: 3px solid #005fcc; outline-offset: -2px;",
        "}",
        ".spectran-transmission-module .transmission-tab-pane {",
        "  padding: 18px 4px 4px;",
        "}",
        ".spectran-transmission-module .transmission-tab-preview {",
        "  position: sticky; top: 12px; align-self: flex-start;",
        "}",
        ".spectran-transmission-module .transmission-preview-details {",
        "  margin-top: 10px; border-top: 1px solid #ddd; padding-top: 8px;",
        "}",
        ".spectran-transmission-module .transmission-preview-details > summary {",
        "  cursor: pointer; font-weight: 600; color: #8a5b00;",
        "}",
        ".spectran-transmission-module .transmission-preview-details-body {",
        "  margin-top: 12px;",
        "}",
        ".spectran-transmission-module .transmission-result-options {",
        "  display: block; margin: 0; padding: 10px 12px 8px;",
        "  border: 0; background: #f4f8fb;",
        "}",
        ".spectran-transmission-module .transmission-result-options h4,",
        ".spectran-transmission-module .transmission-result-options .form-group {",
        "  margin: 0;",
        "}",
        ".spectran-transmission-module .transmission-result-options h4 {",
        "  margin-bottom: 6px; font-weight: 700;",
        "}",
        ".spectran-transmission-module .transmission-spectrum-options .control-label {",
        "  display: block; margin: 0 0 4px; font-weight: 600;",
        "}",
        ".spectran-transmission-module .transmission-spectrum-options .shiny-options-group,",
        ".spectran-transmission-module .transmission-specific-plot-options {",
        "  display: flex; flex-wrap: wrap; align-items: center; gap: 4px 18px;",
        "}",
        ".spectran-transmission-module .transmission-spectrum-options .checkbox-inline,",
        ".spectran-transmission-module .transmission-specific-plot-options .checkbox {",
        "  margin: 0; padding-top: 3px; padding-bottom: 3px;",
        "}",
        ".spectran-transmission-module .transmission-specific-plot-options {",
        "  margin-top: 4px; padding-top: 5px; border-top: 1px solid #d5dce2;",
        "}",
        ".spectran-transmission-module .transmission-result-table-column .nav-tabs {",
        "  margin-bottom: 8px;",
        "}",
        ".spectran-transmission-module .transmission-result-heading {",
        "  margin: 12px 0 10px; overflow-wrap: anywhere;",
        "}",
        ".spectran-transmission-module .transmission-result-plot-grid {",
        "  display: grid; grid-template-columns: minmax(0, 1fr);",
        "  gap: 0; min-width: 0; width: 100%; padding: 0 8px;",
        "  background: #f4f8fb;",
        "}",
        ".spectran-transmission-module .transmission-result-plot-grid .shiny-plot-output {",
        "  min-width: 0; width: 100% !important; max-width: 100%;",
        "}",
        ".spectran-transmission-module .transmission-csv-settings {",
        "  margin-top: 8px;",
        "}",
        ".spectran-transmission-module .transmission-csv-settings > summary {",
        "  cursor: pointer; font-weight: 600; color: #8a5b00;",
        "}",
        ".spectran-transmission-module .transmission-apply-controls {",
        "  margin-top: 0; padding: 12px 14px; background: #fafafa;",
        "  border-left: 4px solid #f8e350;",
        "}",
        ".spectran-transmission-module .transmission-apply-controls h3 {",
        "  margin-top: 0;",
        "}",
        ".spectran-transmission-module .transmission-history-controls .btn-primary {",
        "  max-width: 100%; white-space: normal; overflow-wrap: anywhere;",
        "}",
        ".spectran-transmission-module .progress-bar {",
        "  background-color: #777; color: transparent; font-size: 0;",
        "}",
        ".spectran-transmission-module .progress-bar::after {",
        paste0(
          "  content: '",
          transmission_text("file_receiving"),
          "'; color: #fff; font-size: 12px;"
        ),
        "}",
        ".spectran-transmission-module .progress-bar[style*='width: 100%']::after {",
        paste0(
          "  content: '",
          transmission_text("file_received"),
          "';"
        ),
        "}",
        "@media (max-width: 1399px) {",
        "  .spectran-transmission-module .transmission-result-plot-column,",
        "  .spectran-transmission-module .transmission-result-table-column {",
        "    float: none; width: 100%; max-width: 100%;",
        "  }",
        "}",
        "@media (max-width: 1199px) {",
        "  .spectran-transmission-module .transmission-form-column,",
        "  .spectran-transmission-module .transmission-preview-column,",
        "  .spectran-transmission-module .transmission-table-column,",
        "  .spectran-transmission-module .transmission-metric-column,",
        "  .spectran-transmission-module .transmission-history-column {",
        "    float: none; width: 100%; max-width: 100%;",
        "  }",
        "  .spectran-transmission-module .transmission-preview-column {",
        "    margin-top: 12px;",
        "  }",
        "  .spectran-transmission-module .transmission-tab-preview {",
        "    position: static;",
        "  }",
        "}",
        "@media (max-width: 479px) {",
        "  .spectran-transmission-module { padding: 10px; }",
        "  .transmission-import-confirmation-actions .action-button {",
        "    width: 100%; white-space: normal;",
        "  }",
        "  .spectran-transmission-module .transmission-template-download {",
        "    display: flex; width: 100%; align-items: flex-start;",
        "    justify-content: flex-start; gap: 0.35em;",
        "  }",
        "}",
        sep = "\n"
      )
    )),
    shinyjs::useShinyjs(),
    if (identical(layout, "review")) {
      htmltools::tagList(
        shiny::uiOutput(ns("readiness")),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            class = "col-lg-5 transmission-form-column",
            htmltools::h3("Transmission spectrum"),
            transmission_source_inputs_ui(
              ns,
              csv_labels,
              default_source,
              heading = FALSE
            ),
            htmltools::hr(),
            transmission_metadata_inputs_ui(ns, heading = FALSE),
            transmission_normalization_inputs_ui(ns, heading = FALSE)
          ),
          shiny::column(
            width = 12,
            class = "col-lg-7 transmission-preview-column",
            transmission_preview_ui(ns)
          )
        ),
        transmissionApplyUI(ns("apply")),
        transmissionHistoryUI(ns("history"))
      )
    } else {
      htmltools::tagList(
        htmltools::tags$div(
          class = "transmission-tabs",
          `aria-label` = transmission_text("workflow_label"),
          transmission_tabset_panel(
            id = ns("section"),
            type = "tabs",
            shiny::tabPanel(
              title = transmission_text("tab_spectrum"),
              value = "spectrum",
              transmission_input_tab_ui(
                ns,
                transmission_source_inputs_ui(
                  ns,
                  csv_labels,
                  default_source,
                  heading = TRUE,
                  csv_settings_open = FALSE
                ),
                context = "spectrum",
                readiness = TRUE,
                footer = shiny::uiOutput(ns("navigation_spectrum"))
              )
            ),
            shiny::tabPanel(
              title = transmission_text("tab_measurement"),
              value = "measurement",
              transmission_input_tab_ui(
                ns,
                transmission_metadata_inputs_ui(ns, heading = TRUE),
                context = "measurement",
                footer = shiny::uiOutput(ns("navigation_measurement"))
              )
            ),
            shiny::tabPanel(
              title = transmission_text("tab_normalization"),
              value = "normalization",
              transmission_input_tab_ui(
                ns,
                transmission_normalization_inputs_ui(ns, heading = TRUE),
                context = "normalization",
                footer = shiny::uiOutput(ns("navigation_normalization"))
              )
            ),
            shiny::tabPanel(
              title = transmission_text("tab_results"),
              value = "results",
              htmltools::tags$div(
                class = "transmission-tab-pane transmission-results-tab",
                transmissionApplyControlsUI(ns("apply")),
                transmissionApplyResultsUI(ns("apply")),
                shiny::uiOutput(ns("navigation_results"))
              )
            ),
            shiny::tabPanel(
              title = transmission_text("tab_history"),
              value = "history",
              htmltools::tags$div(
                class = "transmission-tab-pane transmission-history-tab",
                transmissionHistoryControlsUI(ns("history")),
                transmissionHistoryDetailsUI(ns("history")),
                shiny::uiOutput(ns("navigation_history"))
              )
            ),
            shiny::tabPanel(
              title = transmission_text("tab_export"),
              value = "export",
              htmltools::tags$div(
                class = "transmission-tab-pane transmission-export-tab",
                transmissionHistoryExportUI(ns("history")),
                shiny::uiOutput(ns("navigation_export"))
              )
            ),
            selected = "spectrum"
          )
        ),
        htmltools::tags$script(htmltools::HTML(
          transmission_tab_observer_script(ns("section"))
        ))
      )
    }
  )
}

#' Whether a preparation contains a curve that can be previewed
#'
#' @param preparation Transmission preparation or `NULL`.
#'
#' @return A single logical value.
#' @noRd
transmission_has_parseable_curve <- function(preparation) {
  !is.null(preparation) &&
    !is.null(preparation$normalized) &&
    nrow(preparation$normalized) > 0L
}

#' Group incomplete-draft messages by the tab where they can be resolved
#'
#' @param upload Parsed upload record with optional `error`.
#' @param preparation Transmission preparation or `NULL`.
#' @param metadata_requirements Localized metadata requirements.
#'
#' @return A named list for Spectrum, Details, and Normalization.
#' @noRd
transmission_requirement_groups <- function(
  upload,
  preparation,
  metadata_requirements = character()
) {
  groups <- list(
    spectrum = character(),
    measurement = character(),
    normalization = character()
  )

  upload_error <- if (is.list(upload)) upload$error else NULL
  if (!is.null(upload_error) && length(upload_error) > 0L) {
    groups$spectrum <- transmission_localize_diagnostics(upload_error)
  } else if (is.null(preparation)) {
    groups$spectrum <- transmission_text("require_source")
  } else {
    errors <- preparation$diagnostics$errors %||% character()
    details_error <- grepl(
      paste(
        "after scaling",
        "Choose whether transmission is a fraction or percent",
        sep = "|"
      ),
      errors,
      fixed = FALSE
    )
    groups$spectrum <- transmission_localize_diagnostics(
      errors[!details_error]
    )
    groups$measurement <- transmission_localize_diagnostics(
      errors[details_error]
    )
    groups$normalization <- transmission_localize_diagnostics(
      preparation$diagnostics$requirements %||% character()
    )
  }

  groups$measurement <- c(groups$measurement, metadata_requirements)
  lapply(groups, function(messages) {
    unique(messages[!is.na(messages) & nzchar(messages)])
  })
}

#' Guided navigation row for a Transmission tab
#'
#' @param ns Module namespace function.
#' @param back_id Optional Back action identifier.
#' @param back_label Back action label.
#' @param forward_id Optional forward action identifier.
#' @param forward_label Forward action label.
#' @param forward_icon Forward action icon.
#'
#' @return Shiny UI tags.
#' @noRd
transmission_guided_navigation <- function(
  ns,
  back_id = NULL,
  back_label = transmission_text("back"),
  forward_id = NULL,
  forward_label = transmission_text("continue"),
  forward_icon = "arrow-right"
) {
  htmltools::tags$div(
    class = "transmission-guided-navigation",
    if (is.null(back_id)) {
      htmltools::tags$span(class = "transmission-navigation-spacer")
    } else {
      shiny::actionButton(
        ns(back_id),
        label = back_label,
        icon = shiny::icon("arrow-left"),
        class = "btn-default transmission-guided-action"
      )
    },
    if (!is.null(forward_id)) {
      shiny::actionButton(
        ns(forward_id),
        label = forward_label,
        icon = shiny::icon(forward_icon),
        class = "btn-primary transmission-guided-action"
      )
    }
  )
}

#' Choose the Transmission section after an active-source state change
#'
#' @param active_state Active-spectrum adapter or `NULL`.
#'
#' @return `"spectrum"` for imports, otherwise `NULL`.
#' @noRd
transmission_tabs_reset_section <- function(active_state) {
  if (!is.null(active_state) && identical(active_state$change_type, "import")) {
    return("spectrum")
  }
  NULL
}

# Compatibility alias retained for internal milestone fixtures and audit logs.
transmission_wizard_reset_step <- transmission_tabs_reset_section

#' Transmission-spectrum import and normalization server
#'
#' @param id Shiny module identifier.
#' @param fixture_data Optional reactive containing canonical fixture data with
#'   `source_row`, `wavelength_nm`, and `value`. Intended for the isolated app.
#' @param incident_spectrum Optional reactive active Spectran spectrum.
#' @param incident_name Optional reactive active-spectrum name.
#' @param active_state Optional reactive active-spectrum adapter. When supplied,
#'   it is the authoritative source for session history and activation events.
#'
#' @return A named list of reactives: `preparation`, `metadata`, `diagnostics`,
#'   and `ready`.
#' @noRd
transmissionServer <- function(
  id,
  fixture_data = NULL,
  incident_spectrum = NULL,
  incident_name = NULL,
  active_state = NULL
) {
  if (!is.null(fixture_data)) {
    stopifnot(shiny::is.reactive(fixture_data))
  }
  if (!is.null(incident_spectrum)) {
    stopifnot(shiny::is.reactive(incident_spectrum))
  }
  if (!is.null(incident_name)) {
    stopifnot(shiny::is.reactive(incident_name))
  }
  if (!is.null(active_state)) {
    stopifnot(shiny::is.reactive(active_state))
  }
  history_enabled <- !is.null(active_state)

  shiny::moduleServer(id, function(input, output, session) {
    if (is.null(incident_spectrum)) {
      incident_spectrum <- shiny::reactive(NULL)
    }
    if (is.null(incident_name)) {
      incident_name <- shiny::reactive(transmission_text("active_source"))
    }
    if (is.null(active_state)) {
      active_state <- shiny::reactive(NULL)
    }
    current_input_source <- function() {
      source <- input$input_source
      if (is.null(source) || !source %in% c("upload", "catalogue")) {
        return("upload")
      }
      source
    }
    for (tooltip_id in paste0(
      names(transmission_tooltip_specs()),
      "_info"
    )) {
      transmission_info_tooltip_server(tooltip_id)
    }

    csv_labels <- transmission_csv_settings_labels()
    csv_settings <- spectral_csv_settingsServer(
      "csv",
      data = shiny::reactive(NULL),
      labels = csv_labels
    )

    source_revision <- shiny::reactiveVal(0L)
    decision_revision <- shiny::reactiveValues()
    transmittance_type_epoch <- shiny::reactiveVal(0L)
    type_ack_epoch <- shiny::reactiveVal(NULL)
    previous_transmittance_type <- shiny::reactiveVal(NULL)
    curve_decision_key <- shiny::reactiveValues(
      lower_tail = NULL,
      upper_tail = NULL,
      large_gap_ack = NULL
    )
    active_curve_key <- shiny::reactiveVal(NULL)

    suggested_upload_name <- function() {
      file <- shiny::isolate(input$filter_file)
      if (is.null(file) || is.null(file$name) || !nzchar(file$name)) {
        return(transmission_text("uploaded_filter"))
      }
      tools::file_path_sans_ext(basename(file$name))
    }

    reset_filter_decisions <- function(
      suggested_name,
      scale = "fraction",
      transmittance_type = "total"
    ) {
      source_revision(source_revision() + 1L)
      shiny::updateTextInput(
        session,
        "filter_name",
        value = suggested_name
      )
      shiny::updateSelectInput(session, "scale", selected = scale)
      shiny::updateSelectInput(
        session,
        "transmittance_type",
        selected = transmittance_type
      )
      shiny::updateCheckboxInput(session, "type_ack", value = FALSE)
      shiny::updateSelectInput(session, "scattering", selected = "no")
      shiny::updateTextInput(session, "measurement_geometry", value = "")
      shiny::updateTextInput(session, "measurement_angle", value = "")
      shiny::updateCheckboxInput(session, "scattering_ack", value = FALSE)
      shiny::updateSelectInput(session, "lower_tail", selected = "")
      shiny::updateSelectInput(session, "upper_tail", selected = "")
      shiny::updateCheckboxInput(session, "large_gap_ack", value = FALSE)

      curve_decision_key$lower_tail <- NULL
      curve_decision_key$upper_tail <- NULL
      curve_decision_key$large_gap_ack <- NULL
    }

    catalogue_records <- transmission_catalogue_records_data()

    collection_records <- shiny::reactive({
      collection <- input$catalogue_collection
      if (is.null(collection) || !nzchar(collection)) {
        collection <- "featured"
      }
      filter_transmission_catalogue(
        records = catalogue_records,
        collection = collection
      )
    })

    shiny::observe({
      records <- collection_records()
      categories <- unique(records[, c(
        "category_id",
        "category_en",
        "category_de"
      )])
      category_label <- if (
        identical(transmission_language_setting(), "Deutsch")
      ) {
        categories$category_de
      } else {
        categories$category_en
      }
      categories <- categories[order(category_label), , drop = FALSE]
      category_label <- category_label[order(category_label)]
      choices <- c(
        stats::setNames(
          "all",
          transmission_text("catalogue_all_categories")
        ),
        stats::setNames(categories$category_id, category_label)
      )
      selected <- shiny::isolate(input$catalogue_category)
      if (is.null(selected) || !selected %in% unname(choices)) {
        selected <- "all"
      }
      shiny::updateSelectInput(
        session,
        "catalogue_category",
        choices = choices,
        selected = selected
      )
    })

    filtered_catalogue_records <- shiny::reactive({
      collection <- input$catalogue_collection
      if (is.null(collection) || !nzchar(collection)) {
        collection <- "featured"
      }
      category <- input$catalogue_category
      if (is.null(category) || !nzchar(category)) {
        category <- "all"
      }
      filter_transmission_catalogue(
        records = catalogue_records,
        collection = collection,
        category = category
      )
    })

    shiny::observe({
      records <- filtered_catalogue_records()
      choices <- transmission_catalogue_choices(
        records,
        language = transmission_language_setting()
      )
      selected <- shiny::isolate(input$catalogue_filter)
      if (
        is.null(selected) ||
          !nzchar(selected) ||
          !selected %in% unname(choices)
      ) {
        selected <- if (length(choices) > 0L) unname(choices[[1L]]) else ""
      }
      shiny::updateSelectizeInput(
        session,
        "catalogue_filter",
        choices = choices,
        selected = selected,
        server = TRUE
      )
    })

    selected_catalogue <- shiny::reactive({
      selected <- input$catalogue_filter
      if (is.null(selected) || !nzchar(selected)) {
        return(NULL)
      }
      tryCatch(
        transmission_catalogue_record(selected),
        error = function(error) NULL
      )
    })

    output$catalogue_info <- shiny::renderUI({
      selected <- selected_catalogue()
      if (is.null(selected)) {
        return(htmltools::tags$p(
          class = "transmission-preview-note",
          transmission_text("catalogue_choose")
        ))
      }
      record <- selected$record
      category <- if (identical(transmission_language_setting(), "Deutsch")) {
        record$category_de
      } else {
        record$category_en
      }
      coverage <- paste0(
        format_transmission_number(record$wavelength_min_nm),
        "\u2013",
        format_transmission_number(record$wavelength_max_nm),
        " nm"
      )
      source_version <- record$source_version[[1L]]
      source_commit <- record$source_commit[[1L]]
      has_source_commit <- !is.na(source_commit) && nzchar(source_commit)
      measurement_geometry <- transmission_catalogue_localized_value(
        record,
        "measurement_geometry"
      )
      thickness_mm <- record$thickness_mm[[1L]]
      source_points <- record$source_points[[1L]]
      source_description <- transmission_catalogue_localized_value(
        record,
        "source_description"
      )
      transformation <- transmission_catalogue_localized_value(
        record,
        "transformation"
      )
      source_tail_treatment <- transmission_catalogue_localized_value(
        record,
        "source_tail_treatment"
      )
      licence <- transmission_catalogue_localized_value(record, "licence")
      type_label <- switch(
        record$transmittance_type[[1L]],
        total = transmission_text("type_total"),
        internal = transmission_text("type_internal"),
        transmission_text("type_unknown")
      )
      htmltools::tags$aside(
        class = "transmission-catalogue-info",
        `aria-label` = transmission_text("aria_catalogue_information"),
        htmltools::tags$strong(record$display_name),
        htmltools::tags$p(transmission_text(
          "catalogue_coverage",
          category,
          coverage
        )),
        if (
          !is.na(source_tail_treatment) &&
            nzchar(source_tail_treatment)
        ) {
          htmltools::tags$p(source_tail_treatment)
        },
        htmltools::tags$p(
          htmltools::tags$strong(
            paste0(transmission_text("source"), ": ")
          ),
          htmltools::tags$a(
            href = record$source_url,
            target = "_blank",
            rel = "noopener noreferrer",
            record$source_reference
          )
        ),
        htmltools::tags$p(
          htmltools::tags$strong(
            paste0(
              transmission_text(
                if (has_source_commit) {
                  "catalogue_revision"
                } else {
                  "catalogue_version"
                }
              ),
              ": "
            )
          ),
          if (has_source_commit) source_commit else source_version
        ),
        htmltools::tags$p(
          htmltools::tags$strong(
            paste0(transmission_text("citation"), ": ")
          ),
          record$citation
        ),
        htmltools::tags$p(
          htmltools::tags$strong(
            paste0(transmission_text("licence"), ": ")
          ),
          licence
        ),
        htmltools::tags$details(
          class = "transmission-catalogue-details",
          htmltools::tags$summary(
            transmission_text("catalogue_measurement_details")
          ),
          htmltools::tags$dl(
            if (!is.na(source_description) && nzchar(source_description)) {
              htmltools::tagList(
                htmltools::tags$dt(
                  transmission_text("catalogue_description")
                ),
                htmltools::tags$dd(source_description)
              )
            },
            if (
              !is.na(measurement_geometry) &&
                nzchar(measurement_geometry)
            ) {
              htmltools::tagList(
                htmltools::tags$dt(
                  transmission_text("catalogue_geometry")
                ),
                htmltools::tags$dd(measurement_geometry)
              )
            },
            if (is.finite(thickness_mm)) {
              htmltools::tagList(
                htmltools::tags$dt(
                  transmission_text("catalogue_thickness")
                ),
                htmltools::tags$dd(paste0(
                  format_transmission_number(thickness_mm),
                  " mm"
                ))
              )
            },
            htmltools::tags$dt(transmission_text("catalogue_type")),
            htmltools::tags$dd(type_label),
            htmltools::tags$dt(
              transmission_text("catalogue_source_points")
            ),
            htmltools::tags$dd(format(source_points, trim = TRUE)),
            htmltools::tags$dt(
              transmission_text("catalogue_transformation")
            ),
            htmltools::tags$dd(transformation)
          )
        )
      )
    })

    if (!is.null(fixture_data)) {
      shiny::observeEvent(
        fixture_data(),
        {
          fixture <- fixture_data()
          fixture_name <- attr(fixture, "filter_name", exact = TRUE)
          if (is.null(fixture_name) || !nzchar(fixture_name)) {
            fixture_name <- suggested_upload_name()
          }
          reset_filter_decisions(fixture_name)
        },
        ignoreInit = FALSE,
        ignoreNULL = FALSE,
        priority = 100
      )
    }

    shiny::observeEvent(
      input$filter_file,
      {
        fixture_is_active <- !is.null(fixture_data) &&
          !is.null(shiny::isolate(fixture_data()))
        if (
          !fixture_is_active &&
            identical(shiny::isolate(current_input_source()), "upload")
        ) {
          reset_filter_decisions(suggested_upload_name())
        }
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE,
      priority = 100
    )

    shiny::observeEvent(
      input$catalogue_filter,
      {
        if (!identical(shiny::isolate(current_input_source()), "catalogue")) {
          return(invisible(NULL))
        }
        selected <- shiny::isolate(selected_catalogue())
        if (is.null(selected)) {
          return(invisible(NULL))
        }
        record <- selected$record
        reset_filter_decisions(
          suggested_name = record$display_name,
          scale = record$scale,
          transmittance_type = record$transmittance_type
        )
        invisible(NULL)
      },
      ignoreInit = FALSE,
      ignoreNULL = TRUE,
      priority = 100
    )

    shiny::observeEvent(
      input$input_source,
      {
        if (identical(current_input_source(), "catalogue")) {
          selected <- shiny::isolate(selected_catalogue())
          if (!is.null(selected)) {
            record <- selected$record
            reset_filter_decisions(
              suggested_name = record$display_name,
              scale = record$scale,
              transmittance_type = record$transmittance_type
            )
          }
        } else if (identical(current_input_source(), "upload")) {
          reset_filter_decisions(suggested_upload_name())
        }
      },
      ignoreInit = TRUE,
      priority = 90
    )

    record_decision_revision <- function(name) {
      decision_revision[[name]] <- source_revision()
    }

    shiny::observeEvent(
      input$filter_name,
      {
        record_decision_revision("filter_name")
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$scale,
      {
        record_decision_revision("scale")
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$transmittance_type,
      {
        current_type <- input$transmittance_type
        previous_type <- previous_transmittance_type()
        previous_transmittance_type(current_type)

        if (
          !is.null(previous_type) &&
            !identical(previous_type, current_type)
        ) {
          transmittance_type_epoch(transmittance_type_epoch() + 1L)
          type_ack_epoch(NULL)
          decision_revision[["type_ack"]] <- NULL
          shiny::updateCheckboxInput(session, "type_ack", value = FALSE)
        }
        record_decision_revision("transmittance_type")
      },
      ignoreInit = FALSE,
      priority = 20
    )
    shiny::observeEvent(
      input$type_ack,
      {
        record_decision_revision("type_ack")
        current_type <- input$transmittance_type
        if (
          isTRUE(input$type_ack) &&
            current_type %in% c("internal", "unknown")
        ) {
          type_ack_epoch(transmittance_type_epoch())
        } else {
          type_ack_epoch(NULL)
        }
      },
      ignoreInit = FALSE,
      priority = 10
    )
    shiny::observeEvent(
      input$scattering,
      {
        record_decision_revision("scattering")
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$measurement_geometry,
      {
        record_decision_revision("measurement_geometry")
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$measurement_angle,
      {
        record_decision_revision("measurement_angle")
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$scattering_ack,
      {
        record_decision_revision("scattering_ack")
      },
      ignoreInit = TRUE
    )

    current_decision <- function(name, default = NULL) {
      if (identical(decision_revision[[name]], source_revision())) {
        input[[name]]
      } else {
        default
      }
    }

    qualified_type_acknowledged <- function() {
      current_type <- current_decision("transmittance_type", "total")
      isTRUE(current_decision("type_ack", FALSE)) &&
        current_type %in% c("internal", "unknown") &&
        identical(type_ack_epoch(), transmittance_type_epoch())
    }

    uploaded_data <- shiny::reactive({
      if (!is.null(fixture_data)) {
        fixture <- fixture_data()
        if (!is.null(fixture)) {
          fixture_name <- attr(fixture, "filter_name", exact = TRUE)
          if (is.null(fixture_name) || !nzchar(fixture_name)) {
            fixture_name <- "Development transmission fixture"
          }
          selected <- tibble::as_tibble(fixture)
          return(list(
            data = selected,
            source = "fixture",
            error = NULL,
            record = new_transmission_input_record(
              record_type = "development_fixture",
              record_name = fixture_name,
              parsed_values = selected,
              citation = "Synthetic Spectran development fixture",
              license = "MIT"
            )
          ))
        }
      }

      if (identical(current_input_source(), "catalogue")) {
        selected <- selected_catalogue()
        if (is.null(selected)) {
          return(list(
            data = NULL,
            source = "catalogue",
            error = NULL,
            record = NULL
          ))
        }
        catalogue_record <- selected$record
        selected_values <- tibble::tibble(
          source_row = seq_len(nrow(selected$curve)),
          wavelength_nm = selected$curve$wavelength_nm,
          value = selected$curve$transmittance
        )
        return(list(
          data = selected_values,
          source = "catalogue",
          error = NULL,
          record = new_transmission_input_record(
            record_type = "catalogue",
            record_name = catalogue_record$display_name,
            parsed_values = selected_values,
            citation = catalogue_record$citation,
            license = catalogue_record$licence,
            source_url = catalogue_record$source_url,
            catalogue_id = catalogue_record$catalogue_id,
            catalogue = catalogue_record$catalogue,
            category = catalogue_record$category_en,
            source_version = catalogue_record$source_version,
            source_commit = catalogue_record$source_commit,
            source_file = catalogue_record$source_file,
            source_record = catalogue_record$source_record,
            source_reference = catalogue_record$source_reference,
            transformation = catalogue_record$transformation,
            source_tail_treatment = catalogue_record$source_tail_treatment
          )
        ))
      }

      if (is.null(input$filter_file)) {
        return(list(
          data = NULL,
          source = NULL,
          error = NULL,
          record = NULL
        ))
      }

      raw_bytes <- tryCatch(
        {
          size <- file.info(input$filter_file$datapath)$size
          if (length(size) != 1L || !is.finite(size) || size < 0) {
            stop("The received upload size could not be verified.")
          }
          readBin(
            input$filter_file$datapath,
            what = "raw",
            n = as.integer(size)
          )
        },
        error = function(error) error
      )
      if (inherits(raw_bytes, "error")) {
        return(list(
          data = NULL,
          source = "upload",
          error = paste(
            "The original upload could not be preserved for audit.",
            conditionMessage(raw_bytes)
          ),
          record = NULL
        ))
      }

      parsed <- tryCatch(
        read_spectral_csv(input$filter_file$datapath, csv_settings()),
        error = function(error) error
      )
      if (inherits(parsed, "error")) {
        return(list(
          data = NULL,
          source = "upload",
          error = conditionMessage(parsed),
          record = NULL
        ))
      }

      selected <- tryCatch(
        spectral_csv_preview_data(parsed, csv_settings()),
        error = function(error) error
      )
      if (inherits(selected, "error")) {
        return(list(
          data = NULL,
          source = "upload",
          error = conditionMessage(selected),
          record = NULL
        ))
      }

      media_type <- input$filter_file$type
      if (is.null(media_type) || !nzchar(media_type)) {
        media_type <- "application/octet-stream"
      }
      list(
        data = selected,
        source = "upload",
        error = NULL,
        record = new_transmission_input_record(
          record_type = "upload",
          record_name = basename(input$filter_file$name),
          media_type = media_type,
          raw_bytes = raw_bytes,
          parsed_values = selected,
          citation = "User-supplied transmission spectrum",
          license = paste(
            "Rights and reuse terms remain with the user or the original",
            "data source"
          )
        )
      )
    })

    output$file_transport_status <- shiny::renderUI({
      upload <- uploaded_data()
      if (!identical(upload$source, "upload")) {
        return(NULL)
      }
      htmltools::tags$p(
        class = "text-muted",
        role = "status",
        "File received. Parsing and validation results are listed in the ",
        "readiness panel above."
      )
    })

    curve_key <- shiny::reactive({
      upload <- uploaded_data()
      if (is.null(upload$data)) {
        return(paste0("unavailable:", upload$error))
      }
      wavelength <- upload$data$wavelength_nm
      paste0(
        length(wavelength),
        ":",
        paste(
          vapply(
            c(wavelength, upload$data$value),
            format_transmission_number,
            character(1)
          ),
          collapse = "|"
        )
      )
    })

    shiny::observeEvent(
      curve_key(),
      {
        new_key <- curve_key()
        previous_key <- active_curve_key()
        active_curve_key(new_key)

        if (!is.null(previous_key) && !identical(previous_key, new_key)) {
          shiny::updateSelectInput(session, "lower_tail", selected = "")
          shiny::updateSelectInput(session, "upper_tail", selected = "")
          shiny::updateCheckboxInput(
            session,
            "large_gap_ack",
            value = FALSE
          )
          curve_decision_key$lower_tail <- NULL
          curve_decision_key$upper_tail <- NULL
          curve_decision_key$large_gap_ack <- NULL
        }
      },
      ignoreInit = FALSE,
      priority = 50
    )

    shiny::observeEvent(
      input$lower_tail,
      {
        curve_decision_key$lower_tail <- curve_key()
      },
      ignoreInit = FALSE
    )
    shiny::observeEvent(
      input$upper_tail,
      {
        curve_decision_key$upper_tail <- curve_key()
      },
      ignoreInit = FALSE
    )
    shiny::observeEvent(
      input$large_gap_ack,
      {
        curve_decision_key$large_gap_ack <- curve_key()
      },
      ignoreInit = FALSE
    )

    current_curve_decision <- function(name, default = NULL) {
      if (identical(curve_decision_key[[name]], curve_key())) {
        input[[name]]
      } else {
        default
      }
    }

    preparation <- shiny::reactive({
      upload <- uploaded_data()
      if (is.null(upload$data)) {
        return(NULL)
      }

      prepare_transmission_curve(
        data = upload$data,
        scale = current_decision("scale", "fraction"),
        lower_tail = current_curve_decision("lower_tail"),
        upper_tail = current_curve_decision("upper_tail"),
        acknowledge_large_gaps = isTRUE(current_curve_decision(
          "large_gap_ack",
          FALSE
        ))
      )
    })

    metadata <- shiny::reactive({
      upload <- uploaded_data()
      current_preparation <- preparation()
      preparation_diagnostics <- if (is.null(current_preparation)) {
        list()
      } else {
        current_preparation$diagnostics
      }
      gap_labels <- function(gaps) {
        if (!is.data.frame(gaps) || nrow(gaps) == 0L) {
          return("")
        }
        paste0(
          gaps$from_nm,
          "\u2013",
          gaps$to_nm,
          " nm",
          collapse = " | "
        )
      }
      list(
        filter_name = current_decision("filter_name", ""),
        scale = current_decision("scale", "fraction"),
        transmittance_type = current_decision("transmittance_type", "total"),
        filter_model_scope = "passive_non_fluorescent",
        filter_model_limitation = transmission_passive_filter_limitation(),
        qualified_type_acknowledged = qualified_type_acknowledged(),
        scattering = current_decision("scattering", "no"),
        measurement_geometry = trimws(current_decision(
          "measurement_geometry",
          ""
        )),
        measurement_angle = trimws(current_decision(
          "measurement_angle",
          ""
        )),
        scattering_acknowledged = isTRUE(current_decision(
          "scattering_ack",
          FALSE
        )),
        input_record = upload$record,
        parsed_values = if (is.null(current_preparation)) {
          NULL
        } else {
          current_preparation$original
        },
        normalization_decisions = list(
          sorted_input = isTRUE(preparation_diagnostics$was_sorted),
          outside_calculation_range_samples = preparation_diagnostics$outside_count,
          lower_tail = preparation_diagnostics$lower_tail,
          upper_tail = preparation_diagnostics$upper_tail,
          internal_large_gaps = gap_labels(
            preparation_diagnostics$large_gaps
          ),
          external_intervals = gap_labels(
            preparation_diagnostics$external_intervals
          ),
          large_gaps_acknowledged = isTRUE(
            preparation_diagnostics$acknowledge_large_gaps
          ),
          transmittance_type = current_decision(
            "transmittance_type",
            "total"
          ),
          qualified_type_acknowledged = qualified_type_acknowledged(),
          scattering = current_decision("scattering", "no"),
          measurement_geometry = trimws(current_decision(
            "measurement_geometry",
            ""
          )),
          measurement_angle = trimws(current_decision(
            "measurement_angle",
            ""
          )),
          scattering_acknowledged = isTRUE(current_decision(
            "scattering_ack",
            FALSE
          ))
        ),
        normalization_warnings = preparation_diagnostics$warnings
      )
    })

    metadata_requirements <- shiny::reactive({
      current <- metadata()
      requirements <- character()

      if (
        is.null(current$filter_name) || !nzchar(trimws(current$filter_name))
      ) {
        requirements <- c(requirements, transmission_text("require_name"))
      }
      if (
        is.null(current$transmittance_type) ||
          !current$transmittance_type %in% c("total", "internal", "unknown")
      ) {
        requirements <- c(requirements, transmission_text("require_type"))
      }
      if (
        current$transmittance_type %in%
          c("internal", "unknown") &&
          !current$qualified_type_acknowledged
      ) {
        requirements <- c(
          requirements,
          transmission_text("require_type_ack")
        )
      }
      if (identical(current$scattering, "yes")) {
        if (!nzchar(current$measurement_geometry)) {
          requirements <- c(
            requirements,
            transmission_text("require_geometry")
          )
        }
        if (!current$scattering_acknowledged) {
          requirements <- c(
            requirements,
            transmission_text("require_scattering_ack")
          )
        }
      }

      unique(requirements)
    })

    diagnostics <- shiny::reactive({
      upload <- uploaded_data()
      current <- preparation()

      if (!is.null(upload$error)) {
        return(list(
          ready = FALSE,
          errors = transmission_localize_diagnostics(upload$error),
          requirements = metadata_requirements(),
          warnings = character()
        ))
      }
      if (is.null(current)) {
        return(list(
          ready = FALSE,
          errors = character(),
          requirements = c(
            transmission_text("require_source"),
            metadata_requirements()
          ),
          warnings = character()
        ))
      }

      list(
        ready = isTRUE(current$diagnostics$ready) &&
          length(metadata_requirements()) == 0L,
        errors = transmission_localize_diagnostics(
          current$diagnostics$errors
        ),
        requirements = transmission_localize_diagnostics(unique(c(
          current$diagnostics$requirements,
          metadata_requirements()
        ))),
        warnings = transmission_localize_diagnostics(
          current$diagnostics$warnings
        )
      )
    })

    output$type_acknowledgement <- shiny::renderUI({
      current_type <- metadata()$transmittance_type
      if (!current_type %in% c("internal", "unknown")) {
        return(NULL)
      }
      shiny::checkboxInput(
        session$ns("type_ack"),
        label = transmission_text("type_ack"),
        value = isTRUE(current_decision("type_ack", FALSE))
      )
    })

    output$scattering_acknowledgement <- shiny::renderUI({
      if (!identical(metadata()$scattering, "yes")) {
        return(NULL)
      }
      shiny::checkboxInput(
        session$ns("scattering_ack"),
        label = transmission_text("scattering_ack"),
        value = isTRUE(current_decision("scattering_ack", FALSE))
      )
    })

    output$coverage_controls <- shiny::renderUI({
      current <- preparation()
      if (is.null(current) || is.null(current$normalized)) {
        return(NULL)
      }
      diagnostic <- current$diagnostics
      controls <- list()

      if (isTRUE(diagnostic$lower_missing)) {
        affected <- format_transmission_range(diagnostic$lower_tail_grid)
        first_sample <- min(current$normalized$wavelength_nm)
        controls <- c(
          controls,
          list(
            shiny::selectInput(
              session$ns("lower_tail"),
              label = transmission_text("tail_lower_label", affected),
              choices = c(
                stats::setNames("", transmission_text("tail_choose")),
                stats::setNames("zero", transmission_text("tail_opaque")),
                stats::setNames(
                  "one",
                  transmission_text("tail_transparent")
                ),
                stats::setNames(
                  "carry",
                  transmission_text("tail_carry_first")
                )
              ),
              selected = current_curve_decision("lower_tail", "")
            ),
            shiny::helpText(transmission_text(
              "tail_lower_help",
              affected,
              format_transmission_number(first_sample)
            ))
          )
        )
      }
      if (isTRUE(diagnostic$upper_missing)) {
        affected <- format_transmission_range(diagnostic$upper_tail_grid)
        last_sample <- max(current$normalized$wavelength_nm)
        controls <- c(
          controls,
          list(
            shiny::selectInput(
              session$ns("upper_tail"),
              label = transmission_text("tail_upper_label", affected),
              choices = c(
                stats::setNames("", transmission_text("tail_choose")),
                stats::setNames("zero", transmission_text("tail_opaque")),
                stats::setNames(
                  "one",
                  transmission_text("tail_transparent")
                ),
                stats::setNames(
                  "carry",
                  transmission_text("tail_carry_last")
                )
              ),
              selected = current_curve_decision("upper_tail", "")
            ),
            shiny::helpText(transmission_text(
              "tail_upper_help",
              affected,
              format_transmission_number(last_sample)
            ))
          )
        )
      }
      if (!is.null(diagnostic$large_gaps) && nrow(diagnostic$large_gaps) > 0L) {
        gap_text <- paste0(
          vapply(
            diagnostic$large_gaps$from_nm,
            format_transmission_number,
            character(1)
          ),
          "\u2013",
          vapply(
            diagnostic$large_gaps$to_nm,
            format_transmission_number,
            character(1)
          ),
          " nm",
          collapse = ", "
        )
        controls <- c(
          controls,
          list(
            htmltools::tags$p(
              class = "text-warning",
              transmission_text("large_gap_label", gap_text)
            ),
            shiny::checkboxInput(
              session$ns("large_gap_ack"),
              label = transmission_text("large_gap_ack"),
              value = isTRUE(current_curve_decision(
                "large_gap_ack",
                FALSE
              ))
            )
          )
        )
      }

      if (length(controls) == 0L) {
        return(htmltools::tags$div(
          class = "transmission-normalization-complete",
          role = "status",
          shiny::icon("check-circle"),
          htmltools::tags$div(
            htmltools::tags$strong(
              transmission_text("coverage_complete")
            ),
            htmltools::tags$p(
              transmission_text("coverage_complete_help")
            )
          )
        ))
      }
      do.call(htmltools::tagList, controls)
    })

    output$readiness <- shiny::renderUI({
      current <- diagnostics()
      action_items <- list()
      note_items <- list()

      if (length(current$errors) > 0L) {
        action_items <- c(
          action_items,
          lapply(current$errors, function(message) {
            htmltools::tags$li(class = "text-danger", message)
          })
        )
      }
      if (length(current$requirements) > 0L) {
        action_items <- c(
          action_items,
          lapply(current$requirements, function(message) {
            htmltools::tags$li(class = "text-warning", message)
          })
        )
      }
      if (length(current$warnings) > 0L) {
        note_items <- lapply(current$warnings, function(message) {
          htmltools::tags$li(message)
        })
      }

      action_count <- length(current$errors) + length(current$requirements)
      note_count <- length(current$warnings)

      status <- if (isTRUE(current$ready)) {
        htmltools::tags$strong(
          class = "text-success",
          transmission_text("ready")
        )
      } else {
        htmltools::tags$strong(
          class = "text-warning",
          paste0(
            if (action_count == 1L) {
              transmission_text("not_ready_one")
            } else {
              transmission_text("not_ready_many", action_count)
            },
            if (note_count > 0L) {
              paste0(
                "; ",
                if (note_count == 1L) {
                  transmission_text("note_one")
                } else {
                  transmission_text("note_many", note_count)
                }
              )
            } else {
              ""
            },
            "."
          )
        )
      }

      htmltools::tags$div(
        class = paste(
          "transmission-readiness",
          if (isTRUE(current$ready)) "is-ready" else "not-ready"
        ),
        role = "status",
        `aria-live` = "polite",
        `aria-atomic` = "false",
        status,
        if (length(action_items) > 0L) {
          htmltools::tags$div(
            class = paste(
              "transmission-readiness-section",
              "transmission-readiness-actions"
            ),
            htmltools::tags$strong(
              class = "transmission-readiness-heading",
              transmission_text("readiness_actions")
            ),
            htmltools::tags$ul(action_items)
          )
        },
        if (length(note_items) > 0L) {
          htmltools::tags$div(
            class = paste(
              "transmission-readiness-section",
              "transmission-readiness-notes"
            ),
            htmltools::tags$strong(
              class = "transmission-readiness-heading",
              transmission_text("readiness_notes", note_count)
            ),
            htmltools::tags$ul(note_items)
          )
        }
      )
    })

    bind_preview_outputs <- function(context = "") {
      force(context)
      preview_id <- function(base) {
        transmission_preview_output_id(base, context)
      }

      output[[preview_id("preview_outputs")]] <- shiny::renderUI({
        current <- preparation()
        if (is.null(current) || is.null(current$normalized)) {
          return(htmltools::tags$div(
            class = "transmission-preview-note transmission-no-curve-note",
            role = "status",
            transmission_text("preview_no_data")
          ))
        }
        htmltools::tagList(
          shiny::plotOutput(
            session$ns(preview_id("construction_plot")),
            height = "340px"
          ),
          htmltools::tags$details(
            class = "transmission-preview-details",
            htmltools::tags$summary(transmission_text("preview_details")),
            htmltools::tags$div(
              class = "transmission-preview-details-body",
              htmltools::tags$div(
                class = "transmission-preview-toolbar",
                shiny::downloadButton(
                  session$ns(preview_id("download_plot")),
                  label = transmission_text("download_preview_plot"),
                  icon = shiny::icon("image")
                )
              ),
              if (is.null(current$completed)) {
                htmltools::tags$div(
                  class = "transmission-preview-note",
                  role = "status",
                  transmission_text("preview_incomplete")
                )
              },
              htmltools::h4(transmission_text("parsed_rows")),
              shiny::uiOutput(session$ns(preview_id("input_preview_caption"))),
              htmltools::tags$div(
                class = "transmission-gt-scroll",
                tabindex = "0",
                `aria-label` = transmission_text("aria_parsed_table"),
                transmission_gt_output(session$ns(preview_id(
                  "input_preview"
                )))
              ),
              if (!is.null(current$completed)) {
                htmltools::tagList(
                  htmltools::h4(transmission_text("completed_status")),
                  shiny::uiOutput(session$ns(preview_id(
                    "status_summary_caption"
                  ))),
                  htmltools::tags$div(
                    class = "transmission-gt-scroll",
                    tabindex = "0",
                    `aria-label` = transmission_text("aria_status_table"),
                    transmission_gt_output(session$ns(preview_id(
                      "status_summary"
                    )))
                  )
                )
              }
            )
          )
        )
      })

      output[[preview_id("construction_plot")]] <- shiny::renderPlot(
        {
          current <- preparation()
          transmission_construction_plot(current)
        },
        alt = transmission_text("alt_construction_plot")
      )

      output[[preview_id("input_preview_caption")]] <- shiny::renderUI({
        current <- preparation()
        shiny::req(current, current$normalized)
        total <- nrow(current$normalized)
        inside <- sum(current$normalized$within_calculation_range)
        outside <- total - inside
        scale_label <- if (identical(metadata()$scale, "percent")) {
          transmission_text("scale_percent_shown")
        } else {
          transmission_text("scale_fraction_shown")
        }
        htmltools::tags$p(
          htmltools::tags$strong(
            transmission_text("parsed_caption", min(10L, total), total)
          ),
          htmltools::tags$br(),
          transmission_text("parsed_counts", inside, outside, scale_label)
        )
      })

      output[[preview_id("input_preview")]] <- shiny::renderUI({
        current <- preparation()
        shiny::req(current, current$normalized)
        transmission_gt_html(transmission_input_preview_gt(current))
      })

      output[[preview_id("status_summary_caption")]] <- shiny::renderUI({
        current <- preparation()
        shiny::req(current, current$completed)
        htmltools::tags$p(
          transmission_text("completed_caption", nrow(current$completed))
        )
      })

      output[[preview_id("status_summary")]] <- shiny::renderUI({
        current <- preparation()
        shiny::req(current, current$completed)
        transmission_gt_html(transmission_status_summary_gt(current))
      })

      output[[preview_id("download_plot")]] <- shiny::downloadHandler(
        filename = function() {
          filter_name <- metadata()$filter_name
          paste0(
            transmission_filename_component(filter_name),
            "-transmittance-plot.png"
          )
        },
        content = function(file) {
          current <- preparation()
          shiny::req(current, current$normalized)
          plot <- transmission_construction_plot(current) +
            transmission_plot_footnote(font_size = 13)
          ggplot2::ggsave(
            filename = file,
            plot = plot,
            width = 9,
            height = 5.5,
            units = "in",
            dpi = 300,
            bg = "white"
          )
        },
        contentType = "image/png"
      )

      invisible(NULL)
    }

    invisible(lapply(
      c("", "spectrum", "measurement", "normalization"),
      bind_preview_outputs
    ))

    output$download_template <- shiny::downloadHandler(
      filename = function() {
        "Spectran-100-percent-flat-transmittance-template.csv"
      },
      content = function(file) {
        utils::write.csv(
          transmission_template_data(),
          file = file,
          row.names = FALSE
        )
      }
    )

    draft_state <- shiny::reactive({
      list(
        preparation = preparation(),
        metadata = list(
          transmittance_type = current_decision(
            "transmittance_type",
            "total"
          ),
          filter_model_scope = "passive_non_fluorescent",
          qualified_type_acknowledged = qualified_type_acknowledged(),
          scattering = current_decision("scattering", "no"),
          measurement_geometry = trimws(current_decision(
            "measurement_geometry",
            ""
          )),
          measurement_angle = trimws(current_decision(
            "measurement_angle",
            ""
          )),
          scattering_acknowledged = isTRUE(current_decision(
            "scattering_ack",
            FALSE
          ))
        ),
        incident_spectrum = incident_spectrum(),
        incident_name = incident_name()
      )
    })

    applied <- transmissionApplyServer(
      "apply",
      preparation = preparation,
      metadata = metadata,
      ready = shiny::reactive(isTRUE(diagnostics()$ready)),
      incident_spectrum = incident_spectrum,
      incident_name = incident_name,
      draft_state = draft_state
    )

    history_module <- transmissionHistoryServer(
      "history",
      applied_snapshot = applied$snapshot,
      can_promote = applied$can_promote,
      can_download = applied$can_download,
      active_state = active_state,
      clear_snapshot = applied$clear_snapshot,
      mark_snapshot_archived = applied$mark_snapshot_archived,
      show_transmittance_panel = applied$show_transmittance_panel,
      show_incident_fill = applied$show_incident_fill,
      response_curves = applied$response_curves
    )

    missing_groups <- shiny::reactive({
      transmission_requirement_groups(
        upload = uploaded_data(),
        preparation = preparation(),
        metadata_requirements = metadata_requirements()
      )
    })

    go_to_section <- function(section) {
      shiny::updateTabsetPanel(
        session,
        "section",
        selected = section
      )
      transmission_scroll_tab_into_view(
        session$ns("section"),
        section
      )
      invisible(NULL)
    }

    session$onFlushed(
      function() {
        shinyjs::runjs(transmission_guided_keyboard_script())
        shinyjs::runjs(spectran_checkbox_keyboard_script())
      },
      once = TRUE
    )

    navigation_last_activation <- new.env(parent = emptyenv())
    navigation_activation_allowed <- function(
      action_id,
      trigger = c("click", "keyboard")
    ) {
      trigger <- match.arg(trigger)
      other_trigger <- if (identical(trigger, "click")) {
        "keyboard"
      } else {
        "click"
      }
      now <- unname(proc.time()[["elapsed"]])
      other_key <- paste(action_id, other_trigger, sep = "::")
      other_time <- navigation_last_activation[[other_key]]
      if (
        !is.null(other_time) &&
          now - other_time < 0.5
      ) {
        return(FALSE)
      }
      navigation_last_activation[[paste(
        action_id,
        trigger,
        sep = "::"
      )]] <- now
      TRUE
    }

    missing_group_specs <- list(
      spectrum = list(
        heading = "missing_group_spectrum",
        action = "missing_go_spectrum",
        label = "go_to_spectrum"
      ),
      measurement = list(
        heading = "missing_group_details",
        action = "missing_go_measurement",
        label = "go_to_details"
      ),
      normalization = list(
        heading = "missing_group_normalization",
        action = "missing_go_normalization",
        label = "go_to_normalization"
      )
    )

    open_missing_section <- function(section) {
      shiny::removeModal()
      go_to_section(section)
    }

    activate_missing_section <- function(
      section,
      trigger = c("click", "keyboard")
    ) {
      trigger <- match.arg(trigger)
      action_id <- paste0("missing_go_", section)
      if (!navigation_activation_allowed(action_id, trigger)) {
        return(invisible(NULL))
      }
      open_missing_section(section)
    }

    show_missing_modal <- function() {
      groups <- missing_groups()
      sections <- lapply(names(missing_group_specs), function(group_name) {
        messages <- groups[[group_name]]
        if (length(messages) == 0L) {
          return(NULL)
        }
        spec <- missing_group_specs[[group_name]]
        htmltools::tags$section(
          class = "transmission-missing-group",
          htmltools::h4(transmission_text(spec$heading)),
          htmltools::tags$ul(lapply(messages, htmltools::tags$li)),
          shiny::actionButton(
            session$ns(spec$action),
            label = transmission_text(spec$label),
            icon = shiny::icon("arrow-right"),
            class = "btn-primary transmission-guided-action"
          )
        )
      })
      sections <- Filter(Negate(is.null), sections)
      if (length(sections) == 0L) {
        return(invisible(FALSE))
      }
      shiny::showModal(shiny::modalDialog(
        title = transmission_text("missing_modal_title"),
        htmltools::tags$p(transmission_text("missing_modal_intro")),
        do.call(htmltools::tagList, sections),
        footer = shiny::modalButton(transmission_text("modal_close")),
        easyClose = TRUE,
        size = "m"
      ))
      invisible(TRUE)
    }
    shiny::observeEvent(input$missing_go_spectrum, {
      activate_missing_section("spectrum", "click")
    })
    shiny::observeEvent(input$missing_go_measurement, {
      activate_missing_section("measurement", "click")
    })
    shiny::observeEvent(input$missing_go_normalization, {
      activate_missing_section("normalization", "click")
    })

    next_required_section <- function(from = "spectrum") {
      groups <- missing_groups()
      order <- switch(
        from,
        spectrum = c("spectrum", "measurement", "normalization"),
        measurement = c("spectrum", "measurement", "normalization"),
        normalization = c("spectrum", "measurement", "normalization"),
        c("spectrum", "measurement", "normalization")
      )
      for (section in order) {
        if (length(groups[[section]]) > 0L) {
          return(section)
        }
      }
      NULL
    }

    details_are_relevant <- function() {
      current <- metadata()
      !identical(current$scale, "fraction") ||
        !identical(current$transmittance_type, "total") ||
        identical(current$scattering, "yes") ||
        nzchar(current$measurement_geometry %||% "") ||
        nzchar(current$measurement_angle %||% "")
    }

    normalization_is_relevant <- function() {
      current <- preparation()
      if (is.null(current)) {
        return(FALSE)
      }
      diagnostics <- current$diagnostics
      isTRUE(diagnostics$lower_missing) ||
        isTRUE(diagnostics$upper_missing) ||
        (is.data.frame(diagnostics$large_gaps) &&
          nrow(diagnostics$large_gaps) > 0L)
    }

    previous_input_section <- function() {
      if (normalization_is_relevant()) {
        return("normalization")
      }
      if (details_are_relevant()) {
        return("measurement")
      }
      "spectrum"
    }

    apply_and_show_results <- function(trigger = c("click", "keyboard")) {
      trigger <- match.arg(trigger)
      if (isTRUE(applied$apply_current_draft(trigger))) {
        go_to_section("results")
      }
      invisible(NULL)
    }

    activate_spectrum_forward <- function(
      trigger = c("click", "keyboard")
    ) {
      trigger <- match.arg(trigger)
      if (!navigation_activation_allowed("spectrum_forward", trigger)) {
        return(invisible(NULL))
      }
      next_section <- next_required_section("spectrum")
      if (is.null(next_section)) {
        apply_and_show_results(trigger)
      } else if (identical(next_section, "spectrum")) {
        show_missing_modal()
      } else {
        go_to_section(next_section)
      }
      invisible(NULL)
    }

    activate_measurement_back <- function(
      trigger = c("click", "keyboard")
    ) {
      trigger <- match.arg(trigger)
      if (!navigation_activation_allowed("measurement_back", trigger)) {
        return(invisible(NULL))
      }
      go_to_section("spectrum")
    }

    activate_measurement_forward <- function(
      trigger = c("click", "keyboard")
    ) {
      trigger <- match.arg(trigger)
      if (!navigation_activation_allowed("measurement_forward", trigger)) {
        return(invisible(NULL))
      }
      next_section <- next_required_section("measurement")
      if (is.null(next_section)) {
        apply_and_show_results(trigger)
      } else if (identical(next_section, "normalization")) {
        go_to_section("normalization")
      } else {
        show_missing_modal()
      }
      invisible(NULL)
    }

    activate_normalization_back <- function(
      trigger = c("click", "keyboard")
    ) {
      trigger <- match.arg(trigger)
      if (!navigation_activation_allowed("normalization_back", trigger)) {
        return(invisible(NULL))
      }
      go_to_section(if (details_are_relevant()) "measurement" else "spectrum")
    }

    activate_normalization_forward <- function(
      trigger = c("click", "keyboard")
    ) {
      trigger <- match.arg(trigger)
      if (!navigation_activation_allowed("normalization_forward", trigger)) {
        return(invisible(NULL))
      }
      if (is.null(next_required_section("normalization"))) {
        apply_and_show_results(trigger)
      } else {
        show_missing_modal()
      }
      invisible(NULL)
    }

    activate_results_back <- function(trigger = c("click", "keyboard")) {
      trigger <- match.arg(trigger)
      if (!navigation_activation_allowed("results_back", trigger)) {
        return(invisible(NULL))
      }
      go_to_section(previous_input_section())
    }

    activate_results_forward <- function(
      trigger = c("click", "keyboard")
    ) {
      trigger <- match.arg(trigger)
      if (!navigation_activation_allowed("results_forward", trigger)) {
        return(invisible(NULL))
      }
      go_to_section("history")
    }

    activate_history_back <- function(trigger = c("click", "keyboard")) {
      trigger <- match.arg(trigger)
      if (!navigation_activation_allowed("history_back", trigger)) {
        return(invisible(NULL))
      }
      go_to_section("results")
    }

    activate_history_forward <- function(
      trigger = c("click", "keyboard")
    ) {
      trigger <- match.arg(trigger)
      if (!navigation_activation_allowed("history_forward", trigger)) {
        return(invisible(NULL))
      }
      go_to_section("export")
    }

    activate_export_back <- function(trigger = c("click", "keyboard")) {
      trigger <- match.arg(trigger)
      if (!navigation_activation_allowed("export_back", trigger)) {
        return(invisible(NULL))
      }
      go_to_section("history")
    }

    navigation_label <- function(from) {
      next_section <- next_required_section(from)
      if (identical(next_section, "spectrum")) {
        return(transmission_text("review_requirements"))
      }
      if (identical(next_section, "measurement")) {
        if (identical(from, "spectrum")) {
          return(transmission_text("continue_details"))
        }
        return(transmission_text("complete_details"))
      }
      if (identical(next_section, "normalization")) {
        return(transmission_text("continue_normalization"))
      }
      transmission_text("apply_and_results")
    }

    output$navigation_spectrum <- shiny::renderUI({
      ui <- transmission_guided_navigation(
        session$ns,
        forward_id = "spectrum_forward",
        forward_label = navigation_label("spectrum"),
        forward_icon = if (is.null(next_required_section("spectrum"))) {
          "play"
        } else {
          "arrow-right"
        }
      )
      ui
    })
    output$navigation_measurement <- shiny::renderUI({
      ui <- transmission_guided_navigation(
        session$ns,
        back_id = "measurement_back",
        forward_id = "measurement_forward",
        forward_label = navigation_label("measurement"),
        forward_icon = if (is.null(next_required_section("measurement"))) {
          "play"
        } else {
          "arrow-right"
        }
      )
      ui
    })
    output$navigation_normalization <- shiny::renderUI({
      ui <- transmission_guided_navigation(
        session$ns,
        back_id = "normalization_back",
        forward_id = "normalization_forward",
        forward_label = if (is.null(next_required_section("normalization"))) {
          transmission_text("apply_and_results")
        } else {
          transmission_text("review_requirements")
        },
        forward_icon = if (is.null(next_required_section("normalization"))) {
          "play"
        } else {
          "exclamation-circle"
        }
      )
      ui
    })
    output$navigation_results <- shiny::renderUI({
      has_snapshot <- !is.null(applied$snapshot())
      ui <- transmission_guided_navigation(
        session$ns,
        back_id = "results_back",
        forward_id = if (!has_snapshot) {
          NULL
        } else {
          "results_forward"
        },
        forward_label = transmission_text("continue_history")
      )
      ui
    })
    output$navigation_history <- shiny::renderUI({
      ui <- transmission_guided_navigation(
        session$ns,
        back_id = "history_back",
        forward_id = "history_forward",
        forward_label = transmission_text("continue_export")
      )
      ui
    })
    output$navigation_export <- shiny::renderUI({
      ui <- transmission_guided_navigation(
        session$ns,
        back_id = "export_back"
      )
      ui
    })

    shiny::observeEvent(input$spectrum_forward, {
      activate_spectrum_forward("click")
    })
    shiny::observeEvent(input$measurement_back, {
      activate_measurement_back("click")
    })
    shiny::observeEvent(input$measurement_forward, {
      activate_measurement_forward("click")
    })
    shiny::observeEvent(input$normalization_back, {
      activate_normalization_back("click")
    })
    shiny::observeEvent(input$normalization_forward, {
      activate_normalization_forward("click")
    })
    shiny::observeEvent(input$results_back, {
      activate_results_back("click")
    })
    shiny::observeEvent(input$results_forward, {
      activate_results_forward("click")
    })
    shiny::observeEvent(input$history_back, {
      activate_history_back("click")
    })
    shiny::observeEvent(input$history_forward, {
      activate_history_forward("click")
    })
    shiny::observeEvent(input$export_back, {
      activate_export_back("click")
    })
    missing_modal_sequence <- shiny::reactiveVal(0L)
    shiny::observeEvent(
      applied$not_ready_attempt(),
      {
        if (applied$not_ready_attempt() < 1L) {
          return(invisible(NULL))
        }
        if (!isTRUE(applied$apply_ready())) {
          if (isTRUE(show_missing_modal())) {
            missing_modal_sequence(missing_modal_sequence() + 1L)
          }
        }
      },
      ignoreInit = FALSE,
      priority = -100
    )

    section_choices <- c(
      "spectrum",
      "measurement",
      "normalization",
      "results",
      "history",
      "export"
    )
    active_section <- shiny::reactive({
      section <- input$section
      if (is.null(section) || !section %in% section_choices) {
        return("spectrum")
      }
      section
    })

    active_revision <- shiny::reactive({
      current <- active_state()
      if (is.null(current)) {
        return(0L)
      }
      current$revision
    })
    shiny::observeEvent(
      active_revision(),
      {
        current <- active_state()
        reset_section <- transmission_tabs_reset_section(current)
        if (!is.null(reset_section)) {
          go_to_section(reset_section)
        }
      },
      ignoreInit = TRUE
    )

    list(
      preparation = preparation,
      metadata = metadata,
      diagnostics = diagnostics,
      ready = shiny::reactive(isTRUE(diagnostics()$ready)),
      applied_snapshot = applied$snapshot,
      draft_revision = applied$draft_revision,
      apply_sequence = applied$apply_sequence,
      applied_stale = applied$stale,
      apply_ready = applied$apply_ready,
      can_promote = if (history_enabled) {
        history_module$can_promote
      } else {
        applied$can_promote
      },
      can_download = if (history_enabled) {
        history_module$can_download
      } else {
        applied$can_download
      },
      promotion_event = history_module$promotion_event,
      restore_event = history_module$restore_event,
      history = history_module$history,
      archived_snapshot = history_module$archived_snapshot,
      archive_node_id = history_module$archive_node_id,
      active_section = active_section,
      missing_modal_sequence = shiny::reactive(missing_modal_sequence()),
      history_action_sequence = history_module$action_sequence
    )
  })
}

#' Run the isolated Milestone 3 transmission showcase
#'
#' @return A Shiny application object.
#' @noRd
transmissionApp <- function() {
  build_id <- "M3-R4.6.1-preview-2026-08-13.2"
  ui <- shiny::fluidPage(
    shiny::titlePanel(
      paste("Spectran Transmission: Milestone 3 review build", build_id)
    ),
    shiny::wellPanel(
      htmltools::tags$p(
        htmltools::tags$strong("Review build: "),
        build_id
      ),
      shiny::selectInput(
        "source_fixture",
        label = "Development source fixture to import",
        choices = c(
          "CIE D65 at 250 lx" = "d65",
          "Equal-energy source at 250 lx" = "equal_energy",
          "Zero spectrum (undefined-denominator test)" = "zero"
        ),
        selected = "d65"
      ),
      shiny::actionButton(
        "import_source",
        label = "Import selected source as new history root",
        icon = shiny::icon("upload"),
        class = "transmission-source-import"
      ),
      shiny::uiOutput("active_source_status"),
      transmissionSourceImportUI("source_import_reset"),
      shiny::selectInput(
        "fixture",
        label = "Development filter fixture",
        choices = c(
          "None: use file upload" = "none",
          "Neutral 50% (complete)" = "neutral",
          "Selective short-wavelength filter" = "selective",
          "Partial coverage" = "partial",
          "Large internal gap" = "large_gap",
          "Invalid values and duplicate wavelength" = "invalid"
        ),
        selected = "selective"
      ),
      htmltools::p(
        "Fixtures, the source selector, and this import button are ",
        "development-only. A root-only history resets directly. If promoted ",
        "work exists, import requires confirmation before it clears the tree ",
        "and creates a new visible root. Promotion and restore use the same ",
        "event boundary planned for the integrated app."
      ),
      htmltools::p(
        htmltools::tags$strong("Deferred production layout: "),
        "the final integrated page will become a guided wizard with a live ",
        "normalization preview beside every active step."
      )
    ),
    transmissionUI("transmission"),
    htmltools::hr(),
    htmltools::h3("Development status"),
    shiny::verbatimTextOutput("development_status")
  )

  server <- function(input, output, session) {
    source_name_from_id <- function(source_id) {
      switch(
        source_id,
        d65 = "CIE D65 at 250 lx",
        equal_energy = "Equal-energy source at 250 lx",
        zero = "Zero spectrum",
        "Development source"
      )
    }

    active_state <- shiny::reactiveVal(new_transmission_active_spectrum(
      spectrum = transmission_source_fixture("d65"),
      name = source_name_from_id("d65"),
      origin = "Development fixture",
      revision = 1L,
      change_type = "import",
      node_id = "node-1"
    ))

    perform_source_import <- function(request) {
      current <- active_state()
      active_state(new_transmission_active_spectrum(
        spectrum = transmission_source_fixture(request$source_id),
        name = request$source_name,
        origin = "Development fixture",
        revision = current$revision + 1L,
        change_type = "import",
        node_id = "node-1"
      ))
      invisible(NULL)
    }

    fixture_data <- shiny::reactive({
      if (identical(input$fixture, "none")) {
        return(NULL)
      }
      transmission_fixture(input$fixture)
    })

    incident_spectrum <- shiny::reactive(active_state()$spectrum)
    incident_name <- shiny::reactive(active_state()$name)

    transmission <- transmissionServer(
      "transmission",
      fixture_data = fixture_data,
      incident_spectrum = incident_spectrum,
      incident_name = incident_name,
      active_state = shiny::reactive(active_state())
    )

    source_import_reset <- transmissionSourceImportServer(
      "source_import_reset",
      history = transmission$history,
      perform_import = perform_source_import,
      return_focus_id = "import_source"
    )

    source_import_request <- function() {
      selected_id <- input$source_fixture
      list(
        source_id = selected_id,
        source_name = source_name_from_id(selected_id)
      )
    }

    shiny::observeEvent(input$import_source, {
      source_import_reset$request(source_import_request(), "click")
    })

    shinyjs::onevent(
      event = "keydown",
      id = "import_source",
      expr = function(event) {
        if (is_transmission_activation_key(event)) {
          source_import_reset$request(
            source_import_request(),
            "keyboard"
          )
        }
      },
      properties = c("key", "code", "repeat", "which")
    )

    last_activation_sequence <- shiny::reactiveVal(0L)
    activate_event <- function(event) {
      if (
        is.null(event) ||
          event$action_sequence <= last_activation_sequence()
      ) {
        return(invisible(NULL))
      }
      current <- active_state()
      active_state(transmission_active_spectrum_from_event(
        event,
        revision = current$revision + 1L
      ))
      last_activation_sequence(event$action_sequence)
      invisible(NULL)
    }
    shiny::observeEvent(
      transmission$promotion_event(),
      activate_event(transmission$promotion_event()),
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )
    shiny::observeEvent(
      transmission$restore_event(),
      activate_event(transmission$restore_event()),
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    output$active_source_status <- shiny::renderUI({
      current <- active_state()
      htmltools::tags$p(
        class = "transmission-development-active-source",
        role = "status",
        htmltools::tags$strong("Current active source: "),
        paste0(
          current$name,
          " (",
          current$change_type,
          ", ",
          current$node_id,
          ", revision ",
          current$revision,
          ")"
        )
      )
    })

    output$development_status <- shiny::renderPrint({
      current <- transmission$preparation()
      current_history <- transmission$history()
      list(
        build_id = build_id,
        active_state = active_state()[
          c("name", "origin", "revision", "change_type", "node_id")
        ],
        ready = transmission$ready(),
        supplied_rows = if (is.null(current)) 0L else nrow(current$original),
        completed_rows = if (is.null(current) || is.null(current$completed)) {
          0L
        } else {
          nrow(current$completed)
        },
        diagnostics = transmission$diagnostics(),
        draft_revision = transmission$draft_revision(),
        apply_ready = transmission$apply_ready(),
        apply_sequence = transmission$apply_sequence(),
        applied = !is.null(transmission$applied_snapshot()),
        stale = transmission$applied_stale(),
        promotion_enabled = transmission$can_promote(),
        downloads_enabled = transmission$can_download(),
        history = if (is.null(current_history)) {
          NULL
        } else {
          transmission_history_table(current_history)
        },
        history_action_sequence = transmission$history_action_sequence()
      )
    })
  }

  shiny::shinyApp(ui, server)
}
