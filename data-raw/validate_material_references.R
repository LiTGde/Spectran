# Read-only scientific verification against a locally licensed DIN/TS 67600
# text extraction and the public TUB documentation. Derived reports go only
# to the specified output directory. Never bundle the licensed DIN tables.
#
# Rscript --vanilla data-raw/validate_material_references.R DIN.txt TUB.txt OUTPUT
# Start in the package root. Use the existing project R library via R_LIBS.
arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 3L)
  stop("Supply DIN text, TUB text, and output directory.")
pkgload::load_all(quiet = TRUE)
dir.create(arguments[[3]], recursive = TRUE, showWarnings = FALSE)
din <- readLines(arguments[[1]], warn = FALSE)
tub <- readLines(arguments[[2]], warn = FALSE)
source_spectrum <- function(name) {
  if (name == "D65") return(d65_visible_spectrum())
  if (name == "EE") return(transmission_source_fixture("equal_energy"))
  cie <- examplespectra$CIE
  values <- stats::approx(cie$Wellenlaenge, cie[[name]], xout = 380:780)$y
  as_visible_spectrum(tibble::tibble(
    Wellenlaenge = 380:780,
    Bestrahlungsstaerke = values
  ))
}
reference_rows <- function(lines, table, prefix, count, n_values) {
  starts <- grep(paste0("Tabelle ", table, " —"), lines, fixed = TRUE)
  stopifnot(length(starts) >= 1L)
  starts <- tail(starts, 1L)
  candidates <- lines[seq.int(starts, length(lines))]
  candidates <- candidates[grepl(
    paste0("^\\s*", prefix, "[0-9]{2}\\s"),
    candidates
  )]
  candidates <- head(candidates, count)
  stopifnot(length(candidates) == count)
  purrr::map(candidates, function(line) {
    fields <- strsplit(trimws(line), "\\s+")[[1]]
    decimals <- fields[grepl("^[0-9]+,[0-9]{3}$", fields)]
    stopifnot(length(decimals) >= n_values)
    c(
      as.numeric(fields[[1]]),
      as.numeric(gsub(",", ".", head(decimals, n_values), fixed = TRUE))
    )
  }) |>
    do.call(what = rbind)
}
prepared <- purrr::map(tub_material_records$catalogue_id, function(id) {
  curve <- transmission_catalogue_record(id)$curve
  prepare_transmission_curve(
    data.frame(
      wavelength_nm = curve$wavelength_nm,
      value = curve$transmittance
    ),
    scale = "fraction"
  )$completed
}) |>
  stats::setNames(tub_material_records$catalogue_id)
material_values <- function(id, illuminant) {
  source <- source_spectrum(illuminant)
  result <- calculate_material_result(
    source,
    prepared[[id]],
    if (grepl(":reflection:", id, fixed = TRUE)) "reflection" else
      "transmission"
  )
  metric <- function(name, field = "comparison_value")
    unname(result$active_metrics[[field]][
      result$active_metrics$metric_id == name
    ])
  c(
    photopic = metric("photopic_illuminance"),
    melanopic = metric("melanopic_irradiance"),
    der = metric("melanopic_der", "transmitted_value"),
    effective = metric("melanopic_der_effective", "transmitted_value")
  )
}
rows <- list()
record_comparison <- function(
  table,
  id,
  illuminant,
  metric,
  expected,
  actual,
  tolerance
) {
  rows[[length(rows) + 1L]] <<- tibble::tibble(
    reference = table,
    material = id,
    illuminant = illuminant,
    metric = metric,
    expected = expected,
    actual = actual,
    difference = actual - expected,
    rounding_only = abs(actual - expected) <=
      if (startsWith(table, "DIN")) .0005 else .005,
    tolerance = tolerance,
    within_precision_budget = abs(actual - expected) <= tolerance
  )
}
# The coefficient downloads are rounded to .001 and the DIN results to .001.
# Weighted coefficients therefore use a .001 absolute precision budget.
# Ratios such as MDER are reported separately, with a conservative .003 budget.
glass <- reference_rows(din, 6, "1", 12L, 4L)
stopifnot(identical(as.integer(glass[, 1]), 101:112))
for (i in seq_len(nrow(glass))) {
  id <- paste0("tub:transmission:G", i)
  actual <- material_values(id, "D65")
  for (j in 1:4)
    record_comparison(
      "DIN table 6, p20",
      id,
      "D65",
      names(actual)[[j]],
      glass[i, j + 1],
      actual[[j]],
      if (j == 3) .003 else .001
    )
}
reflection_ids <- c(
  "WC11",
  "WC12",
  paste0("WC", 1:10),
  "S3",
  "S4",
  "S5",
  "S1",
  "S2",
  "WF1",
  "WF3",
  "WF5",
  "WF7",
  "WF9",
  "WF2",
  "WF4",
  "WF6",
  "WF8",
  "WF10"
)
for (table in c(9L, 10L, 11L)) {
  illuminants <- switch(
    as.character(table),
    `9` = c("EE", "A", "D65", "FL11"),
    `10` = c("LED_B1", "LED_B2", "LED_B3", "LED_B5"),
    `11` = c("A", "FL11", "D65", "LED_B1", "LED_B2", "LED_B3", "LED_B5")
  )
  expected <- reference_rows(din, table, "4", 27L, if (table == 11L) 7L else 8L)
  stopifnot(identical(as.integer(expected[, 1]), 401:427))
  for (i in seq_len(27L)) {
    id <- paste0("tub:reflection:", reflection_ids[[i]])
    for (j in seq_along(illuminants)) {
      actual <- material_values(id, illuminants[[j]])
      if (table == 11L) {
        record_comparison(
          "DIN table 11, p27",
          id,
          illuminants[[j]],
          "effective",
          expected[i, j + 1L],
          actual[["effective"]],
          .001
        )
      } else {
        for (k in 1:2)
          record_comparison(
            paste("DIN table", table, if (table == 9L) "p25-26" else "p26"),
            id,
            illuminants[[j]],
            names(actual)[[k]],
            expected[i, 1L + 2L * (j - 1L) + k],
            actual[[k]],
            .001
          )
      }
    }
  }
}
# Public TUB integral rows. Table 2 prints its stone integral summaries in
# concrete/concrete/concrete/brick/brick order, although its spectral columns
# and headers are S1-S5. Preserve this discrepancy in the raw comparison.
for (table in 2:5) {
  start <- grep(paste0("Table ", table, ":"), tub, fixed = TRUE)
  end <- grep(paste0("Table ", table + 1L, ":"), tub, fixed = TRUE)
  block <- tub[seq.int(start + 1L, end - 1L)]
  indices <- switch(
    as.character(table),
    `2` = 1:15,
    `3` = 16:28,
    `4` = 29:43,
    `5` = 44:55
  )
  for (illuminant in c("A", "D65")) {
    line <- block[grepl(paste0("(", illuminant, ")"), block, fixed = TRUE)]
    line <- line[grepl("[0-9],[0-9]{2}", line)]
    stopifnot(length(line) == 1L)
    values <- regmatches(line, gregexpr("[0-9],[0-9]{2}", line))[[1L]]
    expected <- as.numeric(gsub(",", ".", values, fixed = TRUE))
    stopifnot(length(expected) == length(indices))
    for (j in seq_along(indices)) {
      id <- tub_material_records$catalogue_id[[indices[[j]]]]
      record_comparison(
        paste("TUB table", table),
        id,
        illuminant,
        "photopic",
        expected[[j]],
        material_values(id, illuminant)[["photopic"]],
        .0055
      )
    }
  }
}
report <- purrr::list_rbind(rows)
utils::write.csv(
  report,
  file.path(arguments[[3]], "material-reference-comparisons.csv"),
  row.names = FALSE
)
summary <- report |>
  dplyr::group_by(reference) |>
  dplyr::summarise(
    comparisons = dplyr::n(),
    rounding_only = sum(rounding_only),
    within_precision_budget = sum(within_precision_budget),
    max_absolute_difference = max(abs(difference))
  )
print(summary, width = Inf)
utils::write.csv(
  summary,
  file.path(arguments[[3]], "material-reference-summary.csv"),
  row.names = FALSE
)
capture.output(
  utils::sessionInfo(),
  file = file.path(arguments[[3]], "sessionInfo.txt")
)
writeLines(
  c(
    "Inputs:",
    normalizePath(arguments[1:2]),
    "Input MD5:",
    capture.output(tools::md5sum(arguments[1:2])),
    "Calculation and data MD5:",
    capture.output(tools::md5sum(c(
      "R/spectral_metrics.R",
      "R/material_core.R",
      "R/sysdata.rda",
      "data-raw/validate_material_references.R",
      "inst/extdata/tub/Spectral_reflectance.csv",
      "inst/extdata/tub/Spectral_transmittance.csv"
    ))),
    "Tables 7 and 8: no matching spectra identified in this TUB release; not validated."
  ),
  file.path(arguments[[3]], "inputs.txt")
)
