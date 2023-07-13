#creates a weighted Spectrum
Spec_wtd <- function(spectrum, sensitivity) {
  temp <- spectrum[[2]] * Specs$AS_wide[[sensitivity]]
  temp
}

#creates the integral of the spectrum

Spec_int <- function(spectrum, sensitivity) {
  temp <- spectrum$Bestrahlungsstaerke * Specs$AS_wide[[sensitivity]]
  temp <- sum(temp)
  temp
}

#Calculates die Photonfluxdensity in quanta/(cm^2*s) from W/m^2
PD <- function(wavelength, irradiance) {
  #constants
  const_h <- 6.626 * 10 ^ -34
  const_c <- 2.998 * 10 ^ 8
  (irradiance * wavelength * 10 ^ -9) / (const_h * const_c) / 100 ^
    2
}

#Calculates the wavelength with maximal energy
LambdaMax <- function(Spectrum) {
  Spectrum %>%
    dplyr::filter(Bestrahlungsstaerke == max(Bestrahlungsstaerke)) %>%
    dplyr::pull(Wellenlaenge) %>%
    base::mean()
}

#function that deactivates the operation of checkboxes
checkbox_update <- function(name, test) {
  #changes when no CRI can be calculated
  if(test) {
    shinyjs::disable(name)
    shiny::updateCheckboxInput(inputId = name, value = FALSE)
    shinyjs::html(
      "r_label", paste0("<p style='color:red'>", lang$server(48), "</p>"))
  }
  #changes when a CRI can be calculated
  else {
    shinyjs::enable(name)
    shinyjs::html(
      "r_label", paste0("<p style='color:black'>", lang$ui(99), "</p>"))
  }}

#Create a colorspecObject
cS_object <- function(Spectrum) {
  Spectrum[[2]] %>% colorSpec::colorSpec(wavelength = 380:780, 
                                         quantity = "energy")
}

#Takes error messages when trying to compute CRI and gives back NA
# input$CIE_grenzen
CRI <- function(Spectrum, tol) {
  test <- 
    try(
      colorSpec::computeCRI(
        Spectrum, tol = ifelse(tol, 5.4e-3, Inf)
        ), 
      silent = TRUE
      )
  
  if (is.na(test)) {NA}
  else if (test <= 100) {
    colorSpec::computeCRI(
      Spectrum,
      tol = ifelse(tol, 5.4e-3, Inf),
      attach = TRUE
    )
  }
  else {NA}
}

#Namespacing functions for Plot and Table generation
ns_plot <- function(name) {
  paste0("plot_", name)
}

ns_table <- function(name) {
  paste0("table_", name)
}

#create an alpha table
table_alpha <- function(Analysis) {
  tibble::tibble(
    Groesse = 
      c(lang$server(68), lang$server(69), lang$server(70), lang$server(71)),
    Zeichen =
      c("E<sub>v,&alpha;,D65</sub>", 
        "E<sub>e,&alpha;</sub>", 
        "a<sub>&alpha;,v</sub>", 
        "&gamma;<sub>&alpha;,v,D65</sub>"),
    Formelzeichen = 
      c("E_v,a,D65", "E_e,a", "a_a", "y_a,v,D65"),
    purrr::map_df(Specs$Plot$Names[1:5], 
                  \(x) {Analysis[[ns_table(x)]]$internal$Wert}),
    Einheit = 
      c("lux", "mW/m\u00b2", "", "" )
  )
}
