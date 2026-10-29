material_path_label_width <- function(width) {
  # Reserve space for the axis and legend key at each font size.
  if (width >= 500) max(14L, min(75L, floor((width - 140) / 7.8))) else
    max(14L, min(32L, floor((width - 130) / 5.8)))
}

# Plot labels use proportional type. Measuring complete lines also handles
# wide capitals in custom names, which a character limit cannot reliably fit.
material_path_wrap_labels <- function(labels, width_px, font_size,
                                      reserve_px = if (font_size > 9) 145 else 130) {
  opened_device <- grDevices::dev.cur() == 1L
  if (opened_device) {
    grDevices::pdf(file = NULL)
    on.exit(grDevices::dev.off(), add = TRUE)
  }
  available <- max(40, width_px - reserve_px) / 96
  fits <- function(value) {
    text <- grid::textGrob(value, gp = grid::gpar(fontsize = font_size, fontfamily = "sans"))
    grid::convertWidth(grid::grobWidth(text), "inches", valueOnly = TRUE) <= available * .95
  }
  vapply(labels, function(label) {
    lines <- character()
    current <- ""
    for (word in strsplit(trimws(label), "[[:space:]]+")[[1L]]) {
      candidate <- if (nzchar(current)) paste(current, word) else word
      if (fits(candidate)) {
        current <- candidate
      } else {
        if (nzchar(current)) lines <- c(lines, current)
        current <- ""
        for (letter in strsplit(word, "", fixed = TRUE)[[1L]]) {
          candidate <- paste0(current, letter)
          if (nzchar(current) && !fits(candidate)) {
            lines <- c(lines, current)
            current <- letter
          } else current <- candidate
        }
      }
    }
    paste(c(lines, current), collapse = "\n")
  }, character(1), USE.NAMES = FALSE)
}

# The plot follows parent links. Sibling branches never enter a selected path.
# Spectra are the saved absolute levels, including explicit level adjustments.
material_path_spectra <- function(history, node_id = history$active_node_id) {
  path <- material_history_path(history, node_id)
  dplyr::bind_rows(lapply(seq_along(path), function(i) {
    node <- history$nodes[[path[[i]]]]
    tibble::tibble(step = i, node_id = node$node_id, name = node$name,
      wavelength_nm = node$spectrum$Wellenlaenge,
      irradiance_w_m2_nm = node$spectrum$Bestrahlungsstaerke,
      light_level_adjusted = isTRUE(node$provenance$explicitly_rescaled))
  }))
}

material_path_plot <- function(history, node_id = history$active_node_id,
                               font_size = 12, label_width = 65L, width_px = NULL) {
  data <- material_path_spectra(history, node_id)
  paths <- unique(data$node_id)
  n <- length(paths)
  labels <- vapply(seq_along(paths), function(i) {
    node <- history$nodes[[paths[[i]]]]
    suffix <- c(if (i == 1L) material_workspace_text("path_start"),
      if (i == n && n > 1L) material_workspace_text("path_selected"),
      if (isTRUE(node$provenance$explicitly_rescaled)) material_workspace_text("path_adjustment"))
    label <- if (n == 1L) node$name else paste0("N", node$sequence_id, " \u00b7 ", node$name,
      if (length(suffix)) paste0(" (", paste(suffix, collapse = "; "), ")"))
    if (is.null(width_px)) transmission_wrap_result_title(label, width = label_width,
      word_width = label_width) else label
  }, character(1))
  if (!is.null(width_px)) labels <- material_path_wrap_labels(labels, width_px, font_size * .85)
  caption <- if (n > 1L) material_workspace_text(if (any(data$light_level_adjusted))
    "path_plot_rescaled" else "path_plot_scenario")
  if (!is.null(caption)) caption <- if (is.null(width_px)) stringr::str_wrap(caption, label_width) else
    material_path_wrap_labels(caption, width_px, font_size * .8, reserve_px = 85)
  colours <- c("#78848b", if (n > 2L) grDevices::hcl.colors(n - 2L, "Dark 3"), "#202830")
  types <- c("longdash", if (n > 2L) rep(c("dashed", "dotdash", "dotted", "twodash"), length.out = n - 2L), "solid")
  if (n == 1L) { colours <- "#202830"; types <- "solid" }
  names(colours) <- names(types) <- paths
  data$node_id <- factor(data$node_id, levels = paths)
  first <- data[data$step == 1L, ]
  last <- data[data$step == n, ]
  plot <- ggplot2::ggplot()
  if (n > 1L) plot <- plot +
    ggridges::geom_ridgeline_gradient(data = first,
      ggplot2::aes(x = .data$wavelength_nm, y = 0,
        height = .data$irradiance_w_m2_nm * 1000, fill = .data$wavelength_nm),
      scale = 1, colour = NA) +
    ggridges::geom_ridgeline(data = first,
      ggplot2::aes(x = .data$wavelength_nm, y = 0,
        height = .data$irradiance_w_m2_nm * 1000),
      scale = 1, fill = "white", colour = NA, alpha = .85)
  plot +
    ggridges::geom_ridgeline_gradient(data = last,
      ggplot2::aes(x = .data$wavelength_nm, y = 0,
        height = .data$irradiance_w_m2_nm * 1000, fill = .data$wavelength_nm),
      scale = 1, colour = NA) +
    ggplot2::geom_line(data = data,
      ggplot2::aes(x = .data$wavelength_nm, y = .data$irradiance_w_m2_nm * 1000,
        colour = .data$node_id, linetype = .data$node_id), linewidth = .85) +
    ggplot2::scale_fill_gradientn(colours = transmission_spectral_palette(), limits = c(380, 780), guide = "none") +
    ggplot2::scale_colour_manual(values = colours, breaks = paths, labels = labels, name = NULL) +
    ggplot2::scale_linetype_manual(values = types, breaks = paths, labels = labels, name = NULL) +
    ggplot2::scale_x_continuous(limits = c(380, 780), breaks = c(400, 500, 600, 700, 780),
      expand = ggplot2::expansion(mult = c(0, .01))) +
    transmission_irradiance_y_scale() +
    ggplot2::labs(title = stringr::str_wrap(material_workspace_text(if (n == 1L) "source_preview" else "path_plot"), label_width),
      caption = caption,
      x = transmission_text("plot_wavelength"),
      y = sub(" (", "\n(", transmission_text("plot_spectral_irradiance"), fixed = TRUE)) +
    transmission_plot_theme(font_size) +
    ggplot2::theme(legend.position = "bottom", legend.direction = "vertical",
      legend.text = ggplot2::element_text(size = font_size * .85),
      legend.key.width = grid::unit(2.8, "lines"),
      legend.key.spacing.y = grid::unit(3, "pt"),
      legend.box.just = "left", legend.justification = "left",
      plot.caption = ggplot2::element_text(hjust = 0, size = font_size * .8, colour = "#59616a"),
      plot.margin = ggplot2::margin(10, 12, 8, 6)) +
    ggplot2::guides(colour = ggplot2::guide_legend(ncol = 1),
      linetype = ggplot2::guide_legend(ncol = 1))
}

write_material_path_plot <- function(history, node_id, file) {
  plot <- material_path_plot(history, node_id, width_px = 9 * 96)
  labels <- plot$scales$get_scales("colour")$labels
  legend_lines <- sum(stringr::str_count(labels, "\n") + 1L)
  ggplot2::ggsave(file, plot,
    width = 9, height = max(5.8, 4.5 + .2 * legend_lines + .08 * length(labels)),
    units = "in", dpi = 180, bg = "white")
  invisible(file)
}
