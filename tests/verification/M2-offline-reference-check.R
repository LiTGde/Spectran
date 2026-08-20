# Milestone 2 offline scientific reference checks

renv::load()
pkgload::load_all(quiet = TRUE)

source_spectrum <- transmission_source_fixture("equal_energy", target_lux = 250)
transmission <- seq(0.05, 0.95, length.out = 401L)
filter <- tibble::tibble(
  wavelength_nm = 380:780,
  transmittance = transmission,
  status = rep("supplied", 401L)
)
result <- calculate_transmission_result(source_spectrum, filter)

# Existing Analysis equations and the shared helpers must remain identical.
for (sensitivity in unname(Specs$Plot$Names)) {
  direct_weighted <- source_spectrum$Bestrahlungsstaerke *
    Specs$AS_wide[[sensitivity]]
  stopifnot(identical(Spec_wtd(source_spectrum, sensitivity), direct_weighted))
  stopifnot(identical(
    Spec_int(source_spectrum, sensitivity),
    sum(direct_weighted)
  ))
}

# D65 filter properties are checked with independent weighted sums.
d65_direct <- stats::approx(
  x = examplespectra$CIE$Wellenlaenge,
  y = examplespectra$CIE$D65,
  xout = 380:780,
  method = "linear",
  ties = "ordered"
)$y
d65_definitions <- spectral_response_definitions()[c(6L, 1:5), ]
d65_expected <- vapply(
  d65_definitions$action_spectrum,
  function(sensitivity) {
    weighting <- Specs$AS_wide[[sensitivity]]
    sum(d65_direct * transmission * weighting) /
      sum(d65_direct * weighting)
  },
  numeric(1)
)
stopifnot(isTRUE(all.equal(
  result$d65_properties$transmitted_value,
  unname(d65_expected),
  tolerance = 1e-15
)))

# Active-source absolute and retained photopic quantities are independent.
photopic_weighting <- Specs$AS_wide[["V(lambda)"]]
photopic_incident_w_m2 <- sum(
  source_spectrum$Bestrahlungsstaerke * photopic_weighting
)
photopic_transmitted_w_m2 <- sum(
  source_spectrum$Bestrahlungsstaerke * transmission * photopic_weighting
)
photopic_expected_retained <- photopic_transmitted_w_m2 /
  photopic_incident_w_m2
photopic_result <- result$active_metrics[
  result$active_metrics$metric_id == "photopic_illuminance",
]
stopifnot(isTRUE(all.equal(
  photopic_result$incident_value,
  photopic_incident_w_m2 * Specs$Efficacy$photopic,
  tolerance = 1e-15
)))
stopifnot(isTRUE(all.equal(
  photopic_result$transmitted_value,
  photopic_transmitted_w_m2 * Specs$Efficacy$photopic,
  tolerance = 1e-15
)))
stopifnot(isTRUE(all.equal(
  photopic_result$comparison_value,
  photopic_expected_retained,
  tolerance = 1e-15
)))

# Melanopic irradiance, EDI, action factor, DER, and changes are independent.
melanopic_weighting <- Specs$AS_wide[["Melanopsin"]]
melanopic_incident_w_m2 <- sum(
  source_spectrum$Bestrahlungsstaerke * melanopic_weighting
)
melanopic_transmitted_w_m2 <- sum(
  source_spectrum$Bestrahlungsstaerke * transmission * melanopic_weighting
)
melanopic_expected <- c(
  incident_irradiance_mw_m2 = melanopic_incident_w_m2 * 1000,
  transmitted_irradiance_mw_m2 = melanopic_transmitted_w_m2 * 1000,
  incident_edi_lx = melanopic_incident_w_m2 * Specs$Efficacy$melanopic,
  transmitted_edi_lx = melanopic_transmitted_w_m2 * Specs$Efficacy$melanopic,
  incident_action_factor = melanopic_incident_w_m2 /
    photopic_incident_w_m2,
  transmitted_action_factor = melanopic_transmitted_w_m2 /
    photopic_transmitted_w_m2
)
melanopic_irradiance <- result$active_metrics[
  result$active_metrics$metric_id == "melanopic_irradiance",
]
melanopic_edi <- result$active_metrics[
  result$active_metrics$metric_id == "melanopic_edi",
]
melanopic_action <- result$active_metrics[
  result$active_metrics$metric_id == "melanopic_action_factor",
]
stopifnot(isTRUE(all.equal(
  c(
    melanopic_irradiance$incident_value,
    melanopic_irradiance$transmitted_value
  ),
  unname(melanopic_expected[c(
    "incident_irradiance_mw_m2",
    "transmitted_irradiance_mw_m2"
  )]),
  tolerance = 1e-15
)))
stopifnot(isTRUE(all.equal(
  c(melanopic_edi$incident_value, melanopic_edi$transmitted_value),
  unname(melanopic_expected[c("incident_edi_lx", "transmitted_edi_lx")]),
  tolerance = 1e-15
)))
stopifnot(isTRUE(all.equal(
  c(melanopic_action$incident_value, melanopic_action$transmitted_value),
  unname(melanopic_expected[c(
    "incident_action_factor",
    "transmitted_action_factor"
  )]),
  tolerance = 1e-15
)))
stopifnot(identical(
  melanopic_action$absolute_change,
  melanopic_action$transmitted_value - melanopic_action$incident_value
))
stopifnot(identical(
  melanopic_action$relative_change,
  melanopic_action$absolute_change / melanopic_action$incident_value
))

# Zero denominators must remain undefined under the machine-precision guard.
zero_source <- tibble::tibble(
  Wellenlaenge = 380:780,
  Bestrahlungsstaerke = rep(0, 401L)
)
zero_result <- calculate_transmission_result(zero_source, filter)
zero_retained <- zero_result$active_metrics$comparison_type == "retained"
stopifnot(!any(zero_result$active_metrics$comparison_defined[zero_retained]))
stopifnot(any(grepl("machine precision", zero_result$warnings, fixed = TRUE)))

versions <- c(
  R = paste(R.version$major, R.version$minor, sep = "."),
  Spectran = as.character(utils::packageVersion("Spectran")),
  shiny = as.character(utils::packageVersion("shiny")),
  testthat = as.character(utils::packageVersion("testthat")),
  tibble = as.character(utils::packageVersion("tibble")),
  ggplot2 = as.character(utils::packageVersion("ggplot2"))
)
print(versions)
cat("Milestone 2 offline reference checks passed.\n")
