# Scaling uses the same visible-grid response calculations as material results.
material_light_level <- function(spectrum, metric = "photopic") {
  metric <- match.arg(metric, c("photopic", "melanopic"))
  responses <- calculate_visible_spectrum_metrics(spectrum)$responses
  unname(responses$equivalent_illuminance_lx[responses$response_id == metric])
}

material_scale_light <- function(spectrum, target, metric = "photopic") {
  spectrum <- as_visible_spectrum(spectrum)
  if (!is.numeric(target) || length(target) != 1L || !is.finite(target) || target < 0)
    stop(material_workspace_text("level_invalid"), call. = FALSE)
  current <- material_light_level(spectrum, metric)
  if (target > 0 && (!is.finite(current) || current <= 0))
    stop(material_workspace_text("level_zero"), call. = FALSE)
  factor <- if (target == 0) 0 else target / current
  spectrum$Bestrahlungsstaerke <- spectrum$Bestrahlungsstaerke * factor
  as_visible_spectrum(spectrum)
}

# Bundled source examples include both 1 nm and 5 nm grids. Normalize to the
# importer's linear 1 nm grid before deriving a scale factor, without changing
# the source samples or extrapolating beyond their measured wavelength range.
material_scale_source_data <- function(data, target, metric = "photopic") {
  visible <- tibble::tibble(Wellenlaenge = 380:780,
    Bestrahlungsstaerke = stats::approx(data[[1L]], data[[2L]], xout = 380:780,
      method = "linear", rule = 1)$y)
  material_scale_light(visible, target, metric)
  current <- material_light_level(visible, metric)
  data[[2L]] <- if (target == 0) data[[2L]] * 0 else data[[2L]] * target / current
  data
}

material_light_level_ui <- function(ns, id = "level_metric") {
  htmltools::div(class = "material-light-level",
    shiny::radioButtons(ns(id), material_workspace_text("light_level"),
      choices = stats::setNames(c("photopic", "melanopic"),
        c(material_workspace_text("illuminance"), material_workspace_text("melanopic_edi"))),
      selected = "photopic", inline = TRUE),
    htmltools::p(class = "help-block", material_workspace_text("level_help")))
}
