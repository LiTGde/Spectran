# Transmission Apply workflow --------------------------------------------

#' UI for applying a completed filter to the active light source
#'
#' @param id Shiny module identifier.
#'
#' @return Shiny UI tags.
#' @noRd
transmissionApplyControlsUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("apply_controls"))
}

#' UI for the provisional applied-result outputs
#'
#' @param id Shiny module identifier.
#'
#' @return Shiny UI tags.
#' @noRd
transmissionApplyResultsUI <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tags$section(
    class = "transmission-apply-results-section",
    shiny::uiOutput(ns("applied_outputs"))
  )
}

#' Complete UI for applying a completed filter
#'
#' @param id Shiny module identifier.
#'
#' @return Shiny UI tags.
#' @noRd
transmissionApplyUI <- function(id) {
  htmltools::tagList(
    htmltools::hr(),
    transmissionApplyControlsUI(id),
    transmissionApplyResultsUI(id)
  )
}

#' Format a finite metric value for compact tables
#'
#' @param value Numeric value.
#' @param defined Whether the value is defined.
#' @param digits Significant digits.
#'
#' @return Display text.
#' @noRd
format_transmission_metric <- function(value, defined = TRUE, digits = 3L) {
  if (!isTRUE(defined) || length(value) != 1L || !is.finite(value)) {
    return(transmission_text("undefined"))
  }
  if (value == 0) {
    return("0")
  }
  magnitude <- abs(value)
  if (magnitude >= 1e5 || magnitude < 1e-3) {
    return(formatC(value, format = "e", digits = max(1L, digits - 1L)))
  }
  format(signif(value, digits), trim = TRUE, scientific = FALSE)
}

#' Format a fractional metric as a percentage
#'
#' @param value Fractional value.
#' @param defined Whether the value is defined.
#'
#' @return Display text.
#' @noRd
format_transmission_percent <- function(value, defined = TRUE) {
  if (!isTRUE(defined) || length(value) != 1L || !is.finite(value)) {
    return(transmission_text("undefined"))
  }
  scales::percent(value, accuracy = 0.1, trim = TRUE)
}

#' Spectral irradiance scale with an exact zero baseline
#'
#' @return A ggplot2 continuous-position scale.
#' @noRd
transmission_irradiance_y_scale <- function() {
  ggplot2::scale_y_continuous(
    limits = c(0, NA_real_),
    expand = ggplot2::expansion(mult = c(0, 0.05))
  )
}

#' Shared Spectran plot theme for transmission views
#'
#' @return A ggplot theme.
#' @noRd
transmission_plot_theme <- function(font_size = 15) {
  cowplot::theme_cowplot(font_size = font_size, font_family = "sans") +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(
        fill = "transparent",
        colour = NA
      ),
      panel.background = ggplot2::element_rect(
        fill = "transparent",
        colour = NA
      ),
      legend.background = ggplot2::element_rect(
        fill = "transparent",
        colour = NA
      ),
      axis.line = ggplot2::element_line(colour = "#333333"),
      axis.ticks = ggplot2::element_line(colour = "#333333"),
      plot.title = ggplot2::element_text(face = "bold", hjust = 0),
      plot.subtitle = ggtext::element_textbox_simple(
        colour = "#4b4b4b",
        hjust = 0,
        margin = ggplot2::margin(0, 0, 5, 0)
      )
    )
}

#' Stable identifiers for the action spectra available in result plots
#'
#' @return A named character vector from UI identifiers to `Specs` names.
#' @noRd
transmission_response_curve_map <- function() {
  c(
    melanopic = "Melanopsin",
    l_cone = "L-cone-opsin",
    m_cone = "M-cone-opsin",
    s_cone = "S-cone-opsin",
    rhodopic = "Rhodopsin",
    photopic = "V(lambda)"
  )
}

#' Human-facing labels for selectable response curves
#'
#' @return A named character vector from stable UI identifiers to labels.
#' @noRd
transmission_response_curve_labels <- function() {
  labels <- transmission_response_curve_map()
  labels[["photopic"]] <- "V(\u03bb)"
  labels
}

#' Normalize current and legacy response-curve selections
#'
#' @param response_curves Stable curve identifiers. Legacy values
#'   `"alphaopic"` and `"photopic"` remain accepted for saved tests and
#'   internal callers.
#'
#' @return Stable response-curve identifiers.
#' @noRd
normalize_transmission_response_curves <- function(response_curves) {
  selected <- unique(as.character(response_curves))
  if ("alphaopic" %in% selected) {
    selected <- c(
      selected,
      c("melanopic", "l_cone", "m_cone", "s_cone", "rhodopic")
    )
  }
  intersect(selected, names(transmission_response_curve_map()))
}

#' Describe the current Apply state without exposing audit counters
#'
#' @param snapshot Current applied snapshot or `NULL`.
#' @param stale Whether the applied snapshot is stale.
#' @param apply_ready Whether the current draft can be applied.
#' @param apply_error Current Apply error, if any.
#'
#' @return A list with `message` and `state_class`.
#' @noRd
transmission_apply_status_state <- function(
  snapshot,
  stale,
  apply_ready,
  apply_error = "",
  archived = FALSE,
  active_source_name = NULL
) {
  if (length(apply_error) == 1L && !is.na(apply_error) && nzchar(apply_error)) {
    return(list(message = apply_error, state_class = "apply-error"))
  }

  if (is.null(snapshot)) {
    if (isTRUE(apply_ready)) {
      return(list(
        message = transmission_text("ready_to_apply"),
        state_class = "ready-to-apply"
      ))
    }
    return(list(
      message = transmission_text("no_provisional"),
      state_class = "not-applied"
    ))
  }

  if (isTRUE(archived)) {
    if (
      is.null(active_source_name) ||
        length(active_source_name) != 1L ||
        is.na(active_source_name) ||
        !nzchar(trimws(active_source_name))
    ) {
      active_source_name <- transmission_text("active_source")
    }
    return(list(
      message = transmission_text(
        "results_archived",
        trimws(active_source_name)
      ),
      state_class = "is-archived"
    ))
  }

  if (isTRUE(stale)) {
    if (isTRUE(apply_ready)) {
      return(list(
        message = transmission_text("stale_valid"),
        state_class = "is-stale stale-valid"
      ))
    }
    return(list(
      message = transmission_text("stale_invalid"),
      state_class = "is-stale stale-invalid"
    ))
  }

  source_name <- snapshot$incident_name
  if (
    is.null(source_name) ||
      length(source_name) != 1L ||
      is.na(source_name) ||
      !nzchar(trimws(source_name))
  ) {
    source_name <- transmission_text("active_source")
  }
  list(
    message = transmission_text("results_current", trimws(source_name)),
    state_class = "is-current"
  )
}

#' Recognize keyboard activation for the Apply action
#'
#' @param event Browser keyboard event properties supplied by `shinyjs`.
#'
#' @return A single logical value.
#' @noRd
is_transmission_apply_key <- function(event) {
  is_transmission_activation_key(event)
}

#' Group undefined metric diagnostics for concise presentation
#'
#' @param snapshot Applied transmission snapshot.
#'
#' @return A list with a concise `summary`, grouped technical `details`, and
#'   counts.
#' @noRd
transmission_metric_warning_presentation <- function(snapshot) {
  if (
    is.null(snapshot) ||
      is.null(snapshot$metrics) ||
      is.null(snapshot$active_metrics)
  ) {
    return(list(
      summary = character(),
      details = list(),
      affected_metrics = 0L,
      detail_messages = 0L
    ))
  }

  metrics <- snapshot$metrics
  warning_rows <- metrics[
    !is.na(metrics$warning) & nzchar(metrics$warning),
    ,
    drop = FALSE
  ]
  if (nrow(warning_rows) == 0L) {
    return(list(
      summary = character(),
      details = list(),
      affected_metrics = 0L,
      detail_messages = 0L
    ))
  }

  active <- snapshot$active_metrics
  metric_value <- function(metric_id, column) {
    index <- match(metric_id, active$metric_id)
    if (is.na(index) || !column %in% names(active)) {
      return(NA_real_)
    }
    as.numeric(active[[column]][[index]])
  }
  incident_total <- metric_value("total_irradiance", "incident_value")
  incident_photopic <- metric_value(
    "photopic_illuminance",
    "incident_value"
  )
  transmitted_photopic <- metric_value(
    "photopic_illuminance",
    "transmitted_value"
  )
  exact_zero_source <- all(
    is.finite(c(incident_total, incident_photopic)) &
      c(incident_total, incident_photopic) == 0
  )
  guarded_zero_source <- all(
    is.finite(c(incident_total, incident_photopic)) &
      abs(c(incident_total, incident_photopic)) <= .Machine$double.eps
  )

  if (exact_zero_source) {
    summary <- transmission_text("warning_zero_source")
  } else {
    summary_parts <- character()
    retained_undefined <- active$comparison_type == "retained" &
      !active$comparison_defined
    balance_undefined <- active$comparison_type == "change" &
      !active$comparison_defined

    if (guarded_zero_source) {
      summary_parts <- c(
        summary_parts,
        transmission_text("warning_precision_source")
      )
    } else if (any(retained_undefined, na.rm = TRUE)) {
      summary_parts <- c(
        summary_parts,
        transmission_text("warning_retained")
      )
    }

    if (any(balance_undefined, na.rm = TRUE)) {
      if (is.finite(transmitted_photopic) && transmitted_photopic == 0) {
        summary_parts <- c(
          summary_parts,
          transmission_text("warning_zero_transmitted")
        )
      } else if (
        is.finite(transmitted_photopic) &&
          abs(transmitted_photopic) <= .Machine$double.eps
      ) {
        summary_parts <- c(
          summary_parts,
          transmission_text("warning_precision_transmitted")
        )
      } else {
        summary_parts <- c(
          summary_parts,
          transmission_text("warning_balance")
        )
      }
    }
    summary <- paste(
      paste(unique(summary_parts), collapse = " "),
      transmission_text("warning_undefined_cells")
    )
  }

  warning_rows$detail_group <- "other"
  warning_rows$detail_group[
    warning_rows$scope == "d65_filter"
  ] <- "d65"
  warning_rows$detail_group[
    warning_rows$comparison_type == "retained"
  ] <- "retained"
  warning_rows$detail_group[
    grepl("_action_factor$", warning_rows$metric_id)
  ] <- "action_factor"
  warning_rows$detail_group[
    grepl("_der$", warning_rows$metric_id)
  ] <- "der"
  warning_rows$display_label <- transmission_metric_labels(
    warning_rows$metric_id,
    warning_rows$metric_label
  )

  labels <- c(
    d65 = transmission_text("warning_group_d65"),
    retained = transmission_text("warning_group_retained"),
    action_factor = transmission_text("warning_group_action"),
    der = transmission_text("warning_group_der"),
    other = transmission_text("warning_group_other")
  )
  group_order <- names(labels)
  details <- lapply(group_order, function(group_id) {
    rows <- warning_rows[
      warning_rows$detail_group == group_id,
      ,
      drop = FALSE
    ]
    if (nrow(rows) == 0L) {
      return(NULL)
    }
    warning_sets <- split(rows, rows$warning, drop = TRUE)
    messages <- vapply(
      warning_sets,
      function(warning_set) {
        labels <- unique(warning_set$display_label)
        key <- switch(
          group_id,
          retained = if (length(labels) == 2L) {
            "warning_detail_retained_pair"
          } else {
            "warning_detail_retained"
          },
          action_factor = "warning_detail_action",
          der = "warning_detail_der",
          "warning_detail_generic"
        )
        do.call(transmission_text, c(list(key), as.list(labels)))
      },
      character(1)
    )
    list(
      id = group_id,
      label = unname(labels[[group_id]]),
      affected_metrics = nrow(rows),
      messages = unname(messages)
    )
  })
  details <- Filter(Negate(is.null), details)

  list(
    summary = summary,
    details = details,
    affected_metrics = nrow(warning_rows),
    detail_messages = sum(vapply(
      details,
      function(group) length(group$messages),
      integer(1)
    ))
  )
}

#' Render grouped undefined-metric guidance
#'
#' @param presentation Output from
#'   `transmission_metric_warning_presentation()`.
#'
#' @return HTML tags or `NULL`.
#' @noRd
transmission_metric_warning_ui <- function(presentation) {
  if (length(presentation$summary) == 0L) {
    return(NULL)
  }
  detail_groups <- lapply(presentation$details, function(group) {
    explanation_count <- length(group$messages)
    htmltools::tagList(
      htmltools::tags$strong(
        class = "transmission-warning-group-heading",
        if (explanation_count == 1L) {
          transmission_text("explanation_one", group$label)
        } else {
          transmission_text(
            "explanation_many",
            group$label,
            explanation_count
          )
        }
      ),
      htmltools::tags$ul(lapply(
        group$messages,
        htmltools::tags$li
      ))
    )
  })

  htmltools::tags$div(
    class = "transmission-metric-warnings",
    role = "status",
    `aria-live` = "polite",
    `aria-atomic` = "true",
    htmltools::tags$strong(transmission_text("undefined_heading")),
    htmltools::tags$p(presentation$summary),
    htmltools::tags$details(
      class = "transmission-warning-details",
      htmltools::tags$summary(
        if (presentation$detail_messages == 1L) {
          transmission_text("technical_detail")
        } else {
          transmission_text(
            "technical_details",
            presentation$detail_messages
          )
        }
      ),
      detail_groups
    )
  )
}

#' Build the incident/transmitted spectral comparison plot
#'
#' @param snapshot Immutable transmission snapshot.
#'
#' @param show_transmittance_panel Whether to add a narrower panel B with the
#'   applied filter's transmittance curve.
#' @param show_title Whether to draw the source and filter name in the plot.
#' @param panel_tag Optional panel tag.
#' @param incident_fill Whether to show the incident spectrum as a lighter
#'   spectral fill. The incident outline remains visible when this is false.
#' @param response_curves Stable identifiers for individually selected action
#'   spectra. Legacy values `"photopic"` and `"alphaopic"` are accepted.
#' @param panel_layout Whether panel B is placed to the right or below panel A.
#' @param font_size Base figure font size.
#' @param max_irradiance Optional upper scale limit in mW/m2/nm.
#' @param title_wrap_width Maximum title line width in characters. The default
#'   preserves the established export layout; in-app callers may supply a
#'   responsive value.
#' @param title_word_wrap_width Optional maximum length for an unbroken title
#'   word. This is used only by especially narrow nested in-app plots. `NULL`
#'   preserves words and the established export layout.
#' @param plot_margin_right Right plot margin in points. Narrow archived plots
#'   may request additional space after the title without changing exports.
#' @param adaptive_title_spacing Whether a title of three or more measured
#'   lines should reserve an additional gap before the semantic subtitle.
#'   This is used by narrow archived plots only.
#'
#' @return A ggplot or patchwork object.
#' @noRd
transmission_spectral_comparison_plot <- function(
  snapshot,
  show_transmittance_panel = FALSE,
  show_title = TRUE,
  panel_tag = NULL,
  incident_fill = TRUE,
  response_curves = character(),
  panel_layout = c("side", "stack"),
  font_size = 15,
  max_irradiance = NULL,
  title_wrap_width = 56L,
  title_word_wrap_width = NULL,
  plot_margin_right = 8,
  adaptive_title_spacing = FALSE
) {
  validate_transmission_applied_snapshot(snapshot)
  panel_layout <- match.arg(panel_layout)
  response_curves <- normalize_transmission_response_curves(response_curves)
  filter_name <- snapshot$metadata$filter_name
  if (
    is.null(filter_name) ||
      length(filter_name) != 1L ||
      is.na(filter_name) ||
      !nzchar(trimws(filter_name))
  ) {
    filter_name <- transmission_text("transmission_filter")
  }
  incident_data <- tibble::tibble(
    wavelength_nm = snapshot$incident_spectrum$Wellenlaenge,
    irradiance_mw_m2_nm = snapshot$incident_spectrum$Bestrahlungsstaerke * 1000
  )
  transmitted_data <- tibble::tibble(
    wavelength_nm = snapshot$transmitted_spectrum$Wellenlaenge,
    irradiance_mw_m2_nm = snapshot$transmitted_spectrum$Bestrahlungsstaerke *
      1000
  )
  maximum <- max(
    c(
      incident_data$irradiance_mw_m2_nm,
      transmitted_data$irradiance_mw_m2_nm
    ),
    na.rm = TRUE
  )
  if (!is.finite(maximum) || maximum <= 0) maximum <- 0.05
  requested_maximum <- suppressWarnings(as.numeric(max_irradiance))
  if (
    length(requested_maximum) != 1L ||
      !is.finite(requested_maximum) ||
      requested_maximum <= 0
  ) {
    requested_maximum <- maximum
  }
  upper_y <- requested_maximum * 1.10
  plot_title <- paste0(
    snapshot$incident_name,
    " \u00d7 ",
    trimws(filter_name)
  )
  title_wrap_width <- suppressWarnings(as.integer(title_wrap_width))
  if (
    length(title_wrap_width) != 1L ||
      is.na(title_wrap_width) ||
      title_wrap_width < 1L
  ) {
    title_wrap_width <- 56L
  }
  plot_title <- transmission_wrap_result_title(
    plot_title,
    width = title_wrap_width,
    word_width = title_word_wrap_width
  )
  title_subtitle_gap <- transmission_result_title_subtitle_gap(
    plot_title,
    adaptive = adaptive_title_spacing
  )
  plot_margin_right <- suppressWarnings(as.numeric(plot_margin_right))
  if (
    length(plot_margin_right) != 1L ||
      !is.finite(plot_margin_right) ||
      plot_margin_right < 0
  ) {
    plot_margin_right <- 8
  }

  main_plot <- ggplot2::ggplot()
  if (isTRUE(incident_fill)) {
    main_plot <- main_plot +
      ggridges::geom_ridgeline_gradient(
        data = incident_data,
        ggplot2::aes(
          x = .data$wavelength_nm,
          y = 0,
          height = .data$irradiance_mw_m2_nm,
          fill = .data$wavelength_nm
        ),
        inherit.aes = FALSE,
        scale = 1,
        colour = NA,
        alpha = 1
      ) +
      ggridges::geom_ridgeline(
        data = incident_data,
        ggplot2::aes(
          x = .data$wavelength_nm,
          y = 0,
          height = .data$irradiance_mw_m2_nm
        ),
        inherit.aes = FALSE,
        scale = 1,
        fill = "white",
        colour = NA,
        alpha = 0.85
      )
  }
  main_plot <- main_plot +
    ggridges::geom_ridgeline_gradient(
      data = transmitted_data,
      ggplot2::aes(
        x = .data$wavelength_nm,
        y = 0,
        height = .data$irradiance_mw_m2_nm,
        fill = .data$wavelength_nm
      ),
      inherit.aes = FALSE,
      scale = 1,
      colour = NA,
      alpha = 1
    ) +
    ggplot2::geom_line(
      data = incident_data,
      ggplot2::aes(
        x = .data$wavelength_nm,
        y = .data$irradiance_mw_m2_nm
      ),
      colour = "#1b1b1b",
      linetype = "22",
      linewidth = 1
    ) +
    ggplot2::geom_line(
      data = transmitted_data,
      ggplot2::aes(
        x = .data$wavelength_nm,
        y = .data$irradiance_mw_m2_nm
      ),
      colour = "#1b1b1b",
      linetype = "solid",
      linewidth = 1
    ) +
    ggplot2::scale_fill_gradientn(
      colours = transmission_spectral_palette(),
      guide = "none"
    ) +
    ggplot2::scale_x_continuous(
      limits = c(380, 780),
      breaks = c(400, 500, 600, 700, 780),
      expand = ggplot2::expansion(add = c(4, 10))
    ) +
    transmission_irradiance_y_scale() +
    ggplot2::coord_cartesian(ylim = c(0, upper_y), clip = "off") +
    ggplot2::labs(
      title = if (isTRUE(show_title)) plot_title else NULL,
      subtitle = if (isTRUE(show_title)) {
        transmission_text("plot_result_subtitle")
      } else {
        NULL
      },
      tag = panel_tag,
      x = transmission_text("plot_wavelength"),
      y = transmission_text("plot_spectral_irradiance")
    ) +
    transmission_plot_theme(font_size = font_size) +
    ggplot2::theme(
      legend.position = "none",
      plot.tag = ggplot2::element_text(
        face = "bold",
        size = font_size * 0.87
      ),
      plot.title = ggplot2::element_text(
        face = "bold",
        hjust = 0,
        margin = ggplot2::margin(0, 0, title_subtitle_gap, 0)
      ),
      plot.margin = ggplot2::margin(5, plot_margin_right, 5, 5)
    )

  response_map <- transmission_response_curve_map()
  selected_types <- unname(response_map[response_curves])
  if (length(selected_types) > 0L) {
    response_data <- Specs$AS_long[
      Specs$AS_long$Type %in% selected_types,
      ,
      drop = FALSE
    ]
    response_labels <- Specs$Plot[
      Specs$Plot$Names %in% selected_types,
      ,
      drop = FALSE
    ]
    response_labels$display_label <- response_labels$Names
    response_labels$display_label[
      response_labels$Names == "V(lambda)"
    ] <- "V(\u03bb)"
    main_plot <- main_plot +
      ggplot2::geom_line(
        data = response_data,
        ggplot2::aes(
          x = .data$Wellenlaenge,
          y = .data$rel_Sens * requested_maximum,
          colour = .data$Type
        ),
        linewidth = 0.75
      ) +
      ggplot2::scale_colour_manual(values = Specs$Plot$Col) +
      ggrepel::geom_label_repel(
        data = response_labels,
        ggplot2::aes(
          x = .data$Peak,
          y = requested_maximum,
          label = .data$display_label,
          colour = .data$Names
        ),
        min.segment.length = 0,
        ylim = c(requested_maximum, upper_y),
        size = 4 / 15 * font_size,
        alpha = 0.78,
        show.legend = FALSE
      )
  }

  if (!isTRUE(show_transmittance_panel)) {
    return(main_plot)
  }

  layout_arguments <- if (identical(panel_layout, "stack")) {
    list(ncol = 1L, heights = c(2, 1))
  } else {
    list(nrow = 1L, widths = c(2, 1))
  }
  do.call(
    patchwork::wrap_plots,
    c(
      list(
        main_plot,
        transmission_filter_panel_plot(
          snapshot,
          font_size = font_size
        )
      ),
      layout_arguments
    )
  ) +
    patchwork::plot_annotation(tag_levels = "A") &
    ggplot2::theme(
      plot.tag = ggplot2::element_text(
        face = "bold",
        size = font_size * 0.87
      )
    )
}

#' Choose a stable result-plot layout from Spectran's shared width sensor
#'
#' @param width Width reported by the app-level `Plotbreite` output.
#' @param breakpoint Width below which panel B is stacked.
#'
#' @return Either `"side"` or `"stack"`.
#' @noRd
transmission_result_panel_layout <- function(width, breakpoint = 700) {
  if (
    is.numeric(width) &&
      length(width) == 1L &&
      is.finite(width) &&
      width < breakpoint
  ) {
    return("stack")
  }
  "side"
}

#' Choose a readable in-app title width from Spectran's shared width sensor
#'
#' @param width Width reported by the app-level `Plotbreite` output.
#'
#' @return Maximum title line width in characters.
#' @noRd
transmission_result_title_wrap_width <- function(width) {
  if (
    !is.numeric(width) ||
      length(width) != 1L ||
      !is.finite(width)
  ) {
    return(56L)
  }
  if (width < 350) return(20L)
  if (width < 480) return(28L)
  if (width < 700) return(40L)
  56L
}

#' Wrap a result title without changing its visible characters
#'
#' @param title Complete result title.
#' @param width Maximum ordinary line width in characters.
#' @param word_width Optional maximum width for an unbroken word. Long words
#'   are split across lines without adding or removing characters.
#'
#' @return A title containing explicit line breaks.
#' @noRd
transmission_wrap_result_title <- function(
  title,
  width = 56L,
  word_width = NULL
) {
  width <- suppressWarnings(as.integer(width))
  if (length(width) != 1L || is.na(width) || width < 1L) width <- 56L
  word_width <- suppressWarnings(as.integer(word_width))
  if (
    length(word_width) != 1L ||
      is.na(word_width) ||
      word_width < 1L
  ) {
    word_width <- NULL
  }

  title <- gsub(
    "(?<=\\S)-(?=\\S)",
    "- ",
    title,
    perl = TRUE
  )
  if (!is.null(word_width)) {
    words <- strsplit(title, " ", fixed = TRUE)[[1L]]
    words <- vapply(
      words,
      function(word) {
        word_length <- nchar(word, type = "chars")
        if (word_length <= word_width) return(word)
        starts <- seq.int(1L, word_length, by = word_width)
        ends <- pmin(starts + word_width - 1L, word_length)
        paste0(substring(word, starts, ends), collapse = "\n")
      },
      character(1)
    )
    title <- paste(words, collapse = " ")
  }
  paragraphs <- strsplit(title, "\n", fixed = TRUE)[[1L]]
  title <- paste(
    unlist(
      lapply(
        paragraphs,
        function(paragraph) strwrap(paragraph, width = width)
      ),
      use.names = FALSE
    ),
    collapse = "\n"
  )
  gsub(
    "(?<=\\S)- (?=\\S)",
    "-",
    title,
    perl = TRUE
  )
}

#' Choose a hard-word wrap for an especially narrow nested result plot
#'
#' @param width Actual rendered width of the nested plot output.
#'
#' @return `10L` below 260 pixels, otherwise `NULL`.
#' @noRd
transmission_result_title_word_wrap_width <- function(width) {
  if (
    is.numeric(width) &&
      length(width) == 1L &&
      is.finite(width) &&
      width < 260
  ) {
    return(10L)
  }
  NULL
}

#' Choose conservative title wrapping for a narrow archived result plot
#'
#' @param width Actual rendered width of the archived plot output.
#'
#' @return Maximum title line width in characters.
#' @noRd
transmission_archived_result_title_wrap_width <- function(width) {
  if (
    is.numeric(width) &&
      length(width) == 1L &&
      is.finite(width) &&
      width < 260
  ) {
    return(16L)
  }
  transmission_result_title_wrap_width(width)
}

#' Reserve right-side title space for a narrow archived result plot
#'
#' @param width Actual rendered width of the archived plot output.
#'
#' @return Right plot margin in points.
#' @noRd
transmission_archived_result_title_right_margin <- function(width) {
  if (
    is.numeric(width) &&
      length(width) == 1L &&
      is.finite(width) &&
      width < 260
  ) {
    return(16)
  }
  8
}

#' Reserve subtitle space for a deeply wrapped archived title
#'
#' @param title Wrapped plot title.
#' @param adaptive Whether adaptive spacing is enabled for this plot.
#'
#' @return Bottom title margin in points.
#' @noRd
transmission_result_title_subtitle_gap <- function(title, adaptive = FALSE) {
  if (!isTRUE(adaptive)) return(0)
  title <- as.character(title)
  if (length(title) != 1L || is.na(title)) return(0)
  line_count <- length(strsplit(title, "\n", fixed = TRUE)[[1L]])
  if (line_count >= 3L) 12 else 0
}

#' Build the D65 properties display table
#'
#' @param snapshot Immutable transmission snapshot.
#'
#' @return A display-ready data frame.
#' @noRd
transmission_d65_display_table <- function(snapshot) {
  validate_transmission_applied_snapshot(snapshot)
  properties <- snapshot$d65_properties
  result <- data.frame(
    Property = transmission_metric_labels(
      properties$metric_id,
      properties$metric_label
    ),
    Symbol = properties$symbol,
    Fraction = vapply(
      seq_len(nrow(properties)),
      function(index) {
        format_transmission_metric(
          properties$transmitted_value[[index]],
          properties$defined[[index]],
          digits = 3L
        )
      },
      character(1)
    ),
    Percent = vapply(
      seq_len(nrow(properties)),
      function(index) {
        format_transmission_percent(
          properties$transmitted_value[[index]],
          properties$defined[[index]]
        )
      },
      character(1)
    ),
    check.names = FALSE
  )
  names(result) <- c(
    transmission_text("table_property"),
    transmission_text("table_symbol"),
    transmission_text("table_fraction"),
    transmission_text("table_percent")
  )
  result
}

#' Build the absolute and retained metrics display table
#'
#' @param snapshot Immutable transmission snapshot.
#'
#' @return A display-ready data frame.
#' @noRd
transmission_absolute_display_table <- function(snapshot) {
  validate_transmission_applied_snapshot(snapshot)
  metrics <- snapshot$active_metrics[
    snapshot$active_metrics$comparison_type == "retained",
    ,
    drop = FALSE
  ]
  result <- data.frame(
    Metric = transmission_metric_labels(
      metrics$metric_id,
      metrics$metric_label
    ),
    Symbol = metrics$symbol,
    Incident = vapply(
      seq_len(nrow(metrics)),
      function(index) {
        format_transmission_metric(
          metrics$incident_value[[index]],
          metrics$defined[[index]]
        )
      },
      character(1)
    ),
    Transmitted = vapply(
      seq_len(nrow(metrics)),
      function(index) {
        format_transmission_metric(
          metrics$transmitted_value[[index]],
          metrics$defined[[index]]
        )
      },
      character(1)
    ),
    Unit = metrics$unit,
    Retained = vapply(
      seq_len(nrow(metrics)),
      function(index) {
        format_transmission_percent(
          metrics$comparison_value[[index]],
          metrics$comparison_defined[[index]]
        )
      },
      character(1)
    ),
    check.names = FALSE
  )
  names(result) <- c(
    transmission_text("table_metric"),
    transmission_text("table_symbol"),
    transmission_text("table_incident"),
    transmission_text("table_transmitted"),
    transmission_text("table_unit"),
    transmission_text("table_retained")
  )
  result
}

#' Build the action-factor and DER display table
#'
#' @param snapshot Immutable transmission snapshot.
#'
#' @return A display-ready data frame.
#' @noRd
transmission_balance_display_table <- function(snapshot) {
  validate_transmission_applied_snapshot(snapshot)
  metrics <- snapshot$active_metrics[
    snapshot$active_metrics$comparison_type == "change",
    ,
    drop = FALSE
  ]
  result <- data.frame(
    Metric = transmission_metric_labels(
      metrics$metric_id,
      metrics$metric_label
    ),
    Symbol = metrics$symbol,
    Incident = vapply(
      seq_len(nrow(metrics)),
      function(index) {
        format_transmission_metric(
          metrics$incident_value[[index]],
          is.finite(metrics$incident_value[[index]])
        )
      },
      character(1)
    ),
    Transmitted = vapply(
      seq_len(nrow(metrics)),
      function(index) {
        format_transmission_metric(
          metrics$transmitted_value[[index]],
          is.finite(metrics$transmitted_value[[index]])
        )
      },
      character(1)
    ),
    `Absolute change` = vapply(
      seq_len(nrow(metrics)),
      function(index) {
        format_transmission_metric(
          metrics$absolute_change[[index]],
          metrics$comparison_defined[[index]]
        )
      },
      character(1)
    ),
    `Relative change` = vapply(
      seq_len(nrow(metrics)),
      function(index) {
        format_transmission_percent(
          metrics$relative_change[[index]],
          metrics$relative_change_defined[[index]]
        )
      },
      character(1)
    ),
    check.names = FALSE
  )
  names(result) <- c(
    transmission_text("table_metric"),
    transmission_text("table_symbol"),
    transmission_text("table_incident"),
    transmission_text("table_transmitted"),
    transmission_text("table_absolute_change"),
    transmission_text("table_relative_change")
  )
  result
}

#' Build a deterministic active-source fixture for the isolated app
#'
#' @param name Source fixture identifier.
#' @param target_lux Target photopic illuminance for non-zero fixtures.
#'
#' @return A 401-row Spectran spectrum.
#' @noRd
transmission_source_fixture <- function(
  name = c("d65", "equal_energy", "zero"),
  target_lux = 250
) {
  name <- match.arg(name)
  if (
    !is.numeric(target_lux) ||
      length(target_lux) != 1L ||
      !is.finite(target_lux) ||
      target_lux <= 0
  ) {
    stop("`target_lux` must be one positive finite number.", call. = FALSE)
  }

  if (name == "zero") {
    return(tibble::tibble(
      Wellenlaenge = 380:780,
      Bestrahlungsstaerke = rep(0, 401L)
    ))
  }
  if (name == "d65") {
    spectrum <- d65_visible_spectrum()
  } else {
    spectrum <- tibble::tibble(
      Wellenlaenge = examplespectra$Measurement$Wellenlaenge,
      Bestrahlungsstaerke = examplespectra$Measurement$equ
    )
    spectrum <- as_visible_spectrum(spectrum)
  }

  metrics <- calculate_visible_spectrum_metrics(spectrum)
  photopic <- metrics$responses$equivalent_illuminance_lx[
    metrics$responses$response_id == "photopic"
  ]
  ratio <- guarded_spectral_ratio(
    numerator = target_lux,
    denominator = photopic,
    metric_label = "Source normalization",
    denominator_label = "photopic illuminance"
  )
  if (!ratio$defined) {
    stop(ratio$warning, call. = FALSE)
  }
  spectrum$Bestrahlungsstaerke <- spectrum$Bestrahlungsstaerke * ratio$value
  spectrum
}

#' Server for the provisional Apply workflow
#'
#' @param id Shiny module identifier.
#' @param preparation Reactive transmission preparation.
#' @param metadata Reactive filter metadata.
#' @param ready Reactive filter readiness flag.
#' @param incident_spectrum Reactive active Spectran spectrum or `NULL`.
#' @param incident_name Reactive active-spectrum name.
#' @param draft_state Reactive state whose invalidation advances the draft
#'   revision.
#'
#' @return Named reactives for snapshot, revision, staleness, readiness, and
#'   future promotion/download gating.
#' @noRd
transmissionApplyServer <- function(
  id,
  preparation,
  metadata,
  ready,
  incident_spectrum,
  incident_name,
  draft_state
) {
  reactive_arguments <- list(
    preparation = preparation,
    metadata = metadata,
    ready = ready,
    incident_spectrum = incident_spectrum,
    incident_name = incident_name,
    draft_state = draft_state
  )
  if (!all(vapply(reactive_arguments, shiny::is.reactive, logical(1)))) {
    stop("All transmission Apply inputs must be reactive.", call. = FALSE)
  }

  shiny::moduleServer(id, function(input, output, session) {
    draft_revision <- shiny::reactiveVal(0L)
    apply_sequence <- shiny::reactiveVal(0L)
    snapshot <- shiny::reactiveVal(NULL)
    snapshot_archived <- shiny::reactiveVal(FALSE)
    apply_error <- shiny::reactiveVal("")
    not_ready_attempt <- shiny::reactiveVal(0L)
    last_click_activation <- shiny::reactiveVal(-Inf)
    last_keyboard_activation <- shiny::reactiveVal(-Inf)

    shiny::observeEvent(
      draft_state(),
      {
        draft_revision(draft_revision() + 1L)
      },
      ignoreInit = FALSE,
      ignoreNULL = FALSE,
      priority = 10
    )

    source_validation <- shiny::reactive({
      source <- incident_spectrum()
      if (is.null(source)) {
        return(list(
          spectrum = NULL,
          error = transmission_text("source_missing")
        ))
      }
      validated <- tryCatch(
        as_visible_spectrum(source, arg = "incident_spectrum"),
        error = function(error) error
      )
      if (inherits(validated, "error")) {
        return(list(
          spectrum = NULL,
          error = transmission_text("source_invalid")
        ))
      }
      list(spectrum = validated, error = "")
    })

    apply_ready <- shiny::reactive({
      current <- preparation()
      isTRUE(ready()) &&
        !is.null(current) &&
        !is.null(current$completed) &&
        !nzchar(source_validation()$error)
    })

    apply_current_draft <- function(trigger = c("click", "keyboard")) {
      trigger <- match.arg(trigger)
      now <- unname(proc.time()[["elapsed"]])
      cross_mode_window_seconds <- 0.5
      if (
        trigger == "click" &&
          now - last_keyboard_activation() < cross_mode_window_seconds
      ) {
        return(invisible(FALSE))
      }
      if (
        trigger == "keyboard" &&
          now - last_click_activation() < cross_mode_window_seconds
      ) {
        return(invisible(FALSE))
      }
      if (trigger == "click") {
        last_click_activation(now)
      } else {
        last_keyboard_activation(now)
      }

      if (!isTRUE(apply_ready())) {
        apply_error(transmission_text("apply_not_ready"))
        not_ready_attempt(not_ready_attempt() + 1L)
        return(invisible(FALSE))
      }

      result <- tryCatch(
        calculate_transmission_result(
          incident = source_validation()$spectrum,
          filter = preparation()$completed
        ),
        error = function(error) error
      )
      if (inherits(result, "error")) {
        apply_error(transmission_text("apply_failed"))
        return(invisible(FALSE))
      }

      next_sequence <- apply_sequence() + 1L
      current_name <- incident_name()
      if (
        is.null(current_name) ||
          length(current_name) != 1L ||
          is.na(current_name) ||
          !nzchar(trimws(current_name))
      ) {
        current_name <- transmission_text("active_source")
      }
      snapshot(new_transmission_applied_snapshot(
        result = result,
        metadata = metadata(),
        incident_name = current_name,
        draft_revision = draft_revision(),
        apply_sequence = next_sequence
      ))
      snapshot_archived(FALSE)
      apply_sequence(next_sequence)
      apply_error("")
      invisible(TRUE)
    }

    shiny::observeEvent(input$apply_filter, {
      apply_current_draft("click")
    })

    shinyjs::onevent(
      event = "keydown",
      id = "apply_filter",
      expr = function(event) {
        if (is_transmission_apply_key(event)) {
          apply_current_draft("keyboard")
        }
      },
      properties = c("key", "code", "repeat", "which")
    )

    stale <- shiny::reactive({
      current <- snapshot()
      !is.null(current) &&
        !identical(current$draft_revision, draft_revision())
    })

    can_promote <- shiny::reactive({
      !is.null(snapshot()) &&
        !isTRUE(snapshot_archived()) &&
        !isTRUE(stale()) &&
        isTRUE(ready())
    })
    can_download <- shiny::reactive({
      !is.null(snapshot()) &&
        !isTRUE(snapshot_archived()) &&
        !isTRUE(stale()) &&
        isTRUE(ready())
    })
    show_transmittance_panel <- shiny::reactive({
      isTRUE(input$show_transmittance_panel)
    })
    show_incident_fill <- shiny::reactive({
      is.null(input$show_incident_fill) || isTRUE(input$show_incident_fill)
    })
    response_curves <- shiny::reactive({
      normalize_transmission_response_curves(input$show_response_curves)
    })

    output$apply_controls <- shiny::renderUI({
      current <- snapshot()
      if (
        !is.null(current) &&
          !isTRUE(stale()) &&
          !isTRUE(snapshot_archived())
      ) {
        return(NULL)
      }
      htmltools::tags$section(
        class = "transmission-apply-section transmission-apply-controls",
        `aria-labelledby` = session$ns("heading"),
        htmltools::h3(
          id = session$ns("heading"),
          transmission_text("apply_heading")
        ),
        htmltools::p(transmission_text("apply_intro")),
        shiny::uiOutput(session$ns("source_summary")),
        shiny::actionButton(
          session$ns("apply_filter"),
          label = transmission_text("apply_button"),
          icon = shiny::icon("play"),
          class = "btn-primary btn-lg"
        ),
        shiny::uiOutput(session$ns("apply_status"))
      )
    })

    output$source_summary <- shiny::renderUI({
      validation <- source_validation()
      if (nzchar(validation$error)) {
        return(htmltools::tags$div(
          class = "transmission-source-summary source-unavailable",
          role = "status",
          htmltools::tags$strong(
            paste0(transmission_text("source_unavailable"), " ")
          ),
          validation$error
        ))
      }
      source <- calculate_visible_spectrum_metrics(validation$spectrum)
      photopic <- source$responses$equivalent_illuminance_lx[
        source$responses$response_id == "photopic"
      ]
      name <- incident_name()
      if (is.null(name) || !nzchar(trimws(name))) {
        name <- transmission_text("active_source")
      }
      htmltools::tags$div(
        class = "transmission-source-summary source-available",
        role = "status",
        htmltools::tags$strong(paste0(name, ": ")),
        transmission_text(
          "source_summary",
          format_transmission_metric(
            source$total_irradiance_mw_m2,
            is.finite(source$total_irradiance_mw_m2)
          ),
          format_transmission_metric(photopic, is.finite(photopic))
        )
      )
    })

    output$apply_status <- shiny::renderUI({
      current <- snapshot()
      status <- transmission_apply_status_state(
        snapshot = current,
        stale = stale(),
        apply_ready = apply_ready(),
        apply_error = apply_error(),
        archived = snapshot_archived(),
        active_source_name = incident_name()
      )

      htmltools::tags$div(
        class = paste("transmission-apply-status", status$state_class),
        role = "status",
        `aria-live` = "polite",
        `aria-atomic` = "true",
        status$message
      )
    })

    output$applied_outputs <- shiny::renderUI({
      current <- snapshot()
      if (is.null(current)) {
        return(NULL)
      }
      filter_name <- current$metadata$filter_name
      if (is.null(filter_name) || !nzchar(trimws(filter_name))) {
        filter_name <- transmission_text("transmission_filter")
      }

      htmltools::tags$div(
        class = paste(
          "transmission-applied-results transmission-analysis-package",
          if (isTRUE(snapshot_archived())) {
            "is-archived"
          } else if (isTRUE(stale())) {
            "is-stale"
          } else {
            "is-current"
          }
        ),
        `aria-label` = if (isTRUE(snapshot_archived())) {
          transmission_text("aria_applied_archived")
        } else if (isTRUE(stale())) {
          transmission_text("aria_applied_stale")
        } else {
          transmission_text("aria_applied_current")
        },
        if (isTRUE(snapshot_archived())) {
          htmltools::tags$div(
            class = "transmission-frozen-result-banner",
            role = "status",
            htmltools::tags$strong(
              transmission_text("frozen_result_heading")
            ),
            htmltools::tags$p(
              transmission_text("frozen_result_intro")
            )
          )
        },
        shiny::uiOutput(session$ns("metric_warnings")),
        htmltools::tags$div(
          class = "transmission-result-options",
          htmltools::h4(transmission_text("plot_options")),
          htmltools::tags$div(
            class = "transmission-spectrum-options",
            shiny::checkboxGroupInput(
              session$ns("show_response_curves"),
              label = transmission_text("show_action_spectra"),
              choiceNames = unname(transmission_response_curve_labels()),
              choiceValues = names(transmission_response_curve_labels()),
              selected = character(),
              inline = TRUE
            )
          ),
          htmltools::tags$div(
            class = "transmission-specific-plot-options",
            shiny::checkboxInput(
              session$ns("show_incident_fill"),
              label = transmission_text("show_incident_fill"),
              value = TRUE
            ),
            shiny::checkboxInput(
              session$ns("show_transmittance_panel"),
              label = transmission_text("show_transmittance_panel"),
              value = FALSE
            )
          )
        ),
        shiny::uiOutput(session$ns("plot_outputs")),
        transmission_tabset_panel(
          id = session$ns("metric_table_tabs"),
          type = "tabs",
          shiny::tabPanel(
            title = transmission_text("result_tab_d65"),
            value = "d65",
            htmltools::tags$div(
              class = "transmission-gt-scroll",
              tabindex = "0",
              `aria-label` = transmission_text("aria_d65_table"),
              transmission_gt_output(session$ns("d65_properties"))
            )
          ),
          shiny::tabPanel(
            title = transmission_text("result_tab_light"),
            value = "light",
            htmltools::tags$div(
              class = "transmission-gt-scroll",
              tabindex = "0",
              `aria-label` = transmission_text("aria_absolute_table"),
              transmission_gt_output(session$ns("absolute_metrics"))
            )
          ),
          shiny::tabPanel(
            title = transmission_text("result_tab_balance"),
            value = "balance",
            htmltools::tags$div(
              class = "transmission-gt-scroll",
              tabindex = "0",
              `aria-label` = transmission_text("aria_balance_table"),
              transmission_gt_output(session$ns("balance_metrics"))
            )
          ),
          selected = "d65"
        )
      )
    })

    output$plot_outputs <- shiny::renderUI({
      current <- snapshot()
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
          session$ns("spectral_comparison"),
          height = if (include_filter && identical(panel_layout, "stack")) {
            "680px"
          } else {
            "430px"
          }
        )
      )
    })

    output$spectral_comparison <- shiny::renderPlot(
      {
        current <- snapshot()
        shiny::req(current)
        transmission_spectral_comparison_plot(
          current,
          show_transmittance_panel = isTRUE(show_transmittance_panel()),
          show_title = TRUE,
          incident_fill = isTRUE(show_incident_fill()),
          response_curves = response_curves(),
          panel_layout = transmission_result_panel_layout(
            session$clientData$output_Plotbreite_width
          ),
          title_wrap_width = transmission_result_title_wrap_width(
            session$clientData$output_Plotbreite_width
          )
        )
      },
      alt = transmission_text("alt_spectral_comparison")
    )

    output$d65_properties <- shiny::renderUI({
      current <- snapshot()
      shiny::req(current)
      transmission_gt_html(transmission_d65_gt(current))
    })

    output$absolute_metrics <- shiny::renderUI({
      current <- snapshot()
      shiny::req(current)
      transmission_gt_html(transmission_absolute_gt(current))
    })

    output$balance_metrics <- shiny::renderUI({
      current <- snapshot()
      shiny::req(current)
      transmission_gt_html(transmission_balance_gt(current))
    })

    invisible(lapply(
      c("d65_properties", "absolute_metrics", "balance_metrics"),
      function(output_id) {
        shiny::outputOptions(
          output,
          output_id,
          suspendWhenHidden = FALSE
        )
      }
    ))

    output$metric_warnings <- shiny::renderUI({
      current <- snapshot()
      shiny::req(current)
      transmission_metric_warning_ui(
        transmission_metric_warning_presentation(current)
      )
    })

    clear_snapshot <- function() {
      snapshot(NULL)
      snapshot_archived(FALSE)
      apply_error("")
      invisible(NULL)
    }

    mark_snapshot_archived <- function() {
      if (!is.null(snapshot())) {
        snapshot_archived(TRUE)
      }
      apply_error("")
      invisible(NULL)
    }

    list(
      snapshot = shiny::reactive(snapshot()),
      draft_revision = shiny::reactive(draft_revision()),
      apply_sequence = shiny::reactive(apply_sequence()),
      stale = stale,
      apply_ready = apply_ready,
      can_promote = can_promote,
      can_download = can_download,
      source_validation = source_validation,
      show_transmittance_panel = show_transmittance_panel,
      show_incident_fill = show_incident_fill,
      response_curves = response_curves,
      not_ready_attempt = shiny::reactive(not_ready_attempt()),
      apply_current_draft = function(
        trigger = c("click", "keyboard")
      ) {
        apply_current_draft(match.arg(trigger))
      },
      snapshot_archived = shiny::reactive(snapshot_archived()),
      clear_snapshot = clear_snapshot,
      mark_snapshot_archived = mark_snapshot_archived
    )
  })
}
