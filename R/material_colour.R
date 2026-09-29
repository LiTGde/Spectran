# Approximate object colour, CIE 1931 2-degree observer, 380-780 nm.
# XYZ is relative to a perfect diffuser under the same incident spectrum.
# sRGB uses the D65 display white, without chromatic adaptation. See
# https://www.w3.org/Graphics/Color/srgb for the matrix and transfer function.
material_reflection_colour <- function(
  filter,
  illuminant = d65_visible_spectrum()
) {
  filter <- as_completed_filter(filter)
  illuminant <- as_visible_spectrum(illuminant)
  energy <- illuminant$Bestrahlungsstaerke
  if (max(energy) == 0) {
    return(list(
      defined = FALSE,
      hex = NA_character_,
      xyz = stats::setNames(rep(NA_real_, 3L), c("X", "Y", "Z")),
      linear_rgb = stats::setNames(rep(NA_real_, 3L), c("R", "G", "B")),
      out_of_gamut = FALSE
    ))
  }
  energy <- energy / max(energy)
  observer <- colorSpec::xyz1931.1nm
  cmf <- unclass(observer)[
    match(380:780, colorSpec::wavelength(observer)),
    ,
    drop = FALSE
  ]
  xyz <- colSums(cmf * (energy * filter$transmittance)) /
    sum(cmf[, "y"] * energy)
  xyz_to_rgb <- matrix(
    c(
      3.2406255,
      -1.5372080,
      -0.4986286,
      -0.9689307,
      1.8757561,
      0.0415175,
      0.0557101,
      -0.2040211,
      1.0569959
    ),
    nrow = 3L,
    byrow = TRUE
  )
  linear_rgb <- as.vector(xyz_to_rgb %*% xyz)
  clipped <- pmin(1, pmax(0, linear_rgb))
  srgb <- ifelse(
    clipped <= 0.0031308,
    12.92 * clipped,
    1.055 * clipped^(1 / 2.4) - 0.055
  )
  channels <- as.integer(round(255 * srgb))
  list(
    defined = TRUE,
    hex = sprintf("#%02X%02X%02X", channels[[1]], channels[[2]], channels[[3]]),
    xyz = stats::setNames(as.numeric(xyz), c("X", "Y", "Z")),
    linear_rgb = stats::setNames(linear_rgb, c("R", "G", "B")),
    out_of_gamut = any(linear_rgb < 0 | linear_rgb > 1)
  )
}

material_colour_preview_ui <- function(filter) {
  if (is.null(filter)) {
    return(htmltools::tags$p(
      class = "transmission-preview-note",
      material_text("colour_incomplete")
    ))
  }
  card <- function(label, colour) {
    htmltools::tags$div(
      class = "material-colour-card",
      htmltools::tags$strong(label),
      htmltools::tagList(
        htmltools::tags$div(
          class = "material-colour-swatch",
          role = "img",
          `aria-label` = paste(label, colour$hex),
          style = paste0("background-color:", colour$hex, ";")
        ),
        htmltools::tags$div(colour$hex),
        htmltools::tags$small(paste0(
          material_text("colour_luminance"),
          ": ",
          formatC(
            100 * colour$xyz[["Y"]],
            format = "f",
            digits = 1L,
            decimal.mark = if (transmission_language_setting() == "Deutsch")
              "," else "."
          ),
          " %"
        ))
      )
    )
  }
  htmltools::tags$section(
    class = "material-colour-preview",
    `aria-label` = material_text("colour_heading"),
    htmltools::h4(material_text("colour_heading")),
    htmltools::tags$div(
      class = "material-colour-cards",
      card(material_text("colour_d65"), material_reflection_colour(filter))
    ),
    htmltools::tags$p(
      class = "material-colour-note",
      material_text("colour_note")
    ),
    htmltools::tags$details(
      htmltools::tags$summary(material_text("colour_method_heading")),
      htmltools::tags$p(material_text("colour_method"))
    )
  )
}
