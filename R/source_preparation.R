# Prepare measured light before interpolation or material calculations. Keep the
# correction separate from material coefficients, which must remain in [0, 1].
prepare_spectran_source <- function(spectrum, provenance = list(), stage = "visible_grid") {
  stopifnot(is.data.frame(spectrum), is.list(provenance),
    is.numeric(spectrum$Wellenlaenge), is.numeric(spectrum$Bestrahlungsstaerke))
  negative <- which(is.finite(spectrum$Bestrahlungsstaerke) & spectrum$Bestrahlungsstaerke < 0)
  if (length(negative)) {
    provenance$source_preprocessing <- list(
      action = "negative_irradiance_to_zero",
      stage = stage,
      count = length(negative),
      unit = "W m^-2 nm^-1",
      samples = data.frame(
        wavelength_nm = spectrum$Wellenlaenge[negative],
        measured_irradiance_w_m2_nm = spectrum$Bestrahlungsstaerke[negative],
        used_irradiance_w_m2_nm = 0
      )
    )
    spectrum$Bestrahlungsstaerke[negative] <- 0
  }
  list(spectrum = spectrum, provenance = provenance)
}

material_source_preprocessing_note <- function(provenance) {
  correction <- provenance$source_preprocessing
  if (is.null(correction) || !isTRUE(correction$count > 0)) return(NULL)
  htmltools::p(class = "material-source-warning", role = "status",
    material_workspace_text("negative_source", correction$count))
}
