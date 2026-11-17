# Both age plots label the maximum of the curve they actually draw.
age_sensitivity_layers <- function(
  tau_rel,
  max_e,
  plot_multiplier,
  font_size,
  pupil_factor = 1
) {
  curve <- Specs$AS_long[Specs$AS_long$Type == "Melanopsin", ]
  curve$rel_Sens <- curve$rel_Sens * tau_rel * max_e * pupil_factor
  peak <- curve[which.max(curve$rel_Sens), ]

  # Younger observers can have a peak above the displayed range. Keep the
  # label below the plot's upper margin while retaining its peak wavelength.
  label_limit <- max_e * plot_multiplier
  label_y <- min(peak$rel_Sens + max_e * 0.2, label_limit)

  list(
    ggplot2::geom_path(
      data = curve,
      ggplot2::aes(x = Wellenlaenge, y = rel_Sens),
      inherit.aes = FALSE,
      colour = Specs$Plot$Col[[1]],
      linewidth = 0.75
    ),
    ggrepel::geom_label_repel(
      data = peak,
      ggplot2::aes(x = Wellenlaenge, y = rel_Sens),
      label = "Melanopsin",
      inherit.aes = FALSE,
      colour = Specs$Plot$Col[[1]],
      min.segment.length = 0,
      ylim = c(0, label_limit),
      direction = "y",
      nudge_y = label_y - peak$rel_Sens,
      seed = 1,
      alpha = 0.85,
      parse = TRUE,
      size = 4 / 15 * font_size
    )
  )
}
