# Build the small, offline fixture for the external-reference unit tests.
# Run from the package root: Rscript --vanilla data-raw/validation_references.R
# Requires readxl only. Never run this builder automatically during tests.
# CIE, luox and DIN expected values come from the archived external outputs,
# not from Spectran's metric functions. See tests/testthat/fixtures/README.md.

local({
  article <- "vignettes/articles"
  spectrum_ids <- c(
    "EE_Spektrum", "Fluoreszenz_NW", "Fluoreszenz_WW", "Halogen",
    "LED_2200K", "LED_4000K_1", "LED_4000K_2", "LED_6900K",
    "Nordhimmel", "Norm_TL_6500K"
  )
  responses <- c(
    "s_cone_opic", "m_cone_opic", "l_cone_opic", "rhodopic", "melanopic"
  )
  metrics <- c(
    "illuminance", paste0(responses, "_irradiance"),
    paste0(responses, "_edi")
  )
  units <- c("lx", rep("mW/m2", 5), rep("lx", 5))
  inputs <- character()
  input <- function(...) {
    path <- file.path(...)
    stopifnot(file.exists(path))
    inputs <<- c(inputs, path)
    path
  }

  spectra <- stats::setNames(lapply(spectrum_ids, function(id) {
    spectrum <- utils::read.csv(input(
      article, "Original_Spectra", paste0(id, ".csv")
    ))
    stopifnot(ncol(spectrum) == 2L, identical(spectrum[[1]], 380:780),
      all(is.finite(as.matrix(spectrum))))
    names(spectrum) <- c("Wellenlaenge", "Bestrahlungsstaerke")
    spectrum
  }), spectrum_ids)

  cie <- do.call(rbind, lapply(spectrum_ids, function(id) {
    sheet <- suppressMessages(readxl::read_xlsx(input(
      article, "Results_CIE_Toolbox", id, "CIE S 026 alpha-opic Toolbox.xlsx"
    ), sheet = "Outputs"))
    response_labels <- c("S-cone-opic", "M-cone-opic", "L-cone-opic",
      "Rhodopic", "Melanopic")
    stopifnot(grepl("v1.049a", names(sheet)[1], fixed = TRUE),
      identical(as.character(sheet[19, ]), response_labels),
      identical(as.character(sheet[31, ]), response_labels),
      sheet[[3]][13] == "illuminance, lx",
      sheet[[1]][13] == "irradiance, W.m-2")
    expected <- c(
      as.numeric(sheet[[3]][14]), as.numeric(sheet[20, ]) * 1000,
      as.numeric(sheet[32, ]), as.numeric(sheet[[1]][14]) * 1000
    )
    stopifnot(length(expected) == 12L, all(is.finite(expected)),
      all(expected > 0))
    data.frame(spectrum = id, metric = c(metrics, "irradiance"),
      unit = c(units, "mW/m2"), expected = expected)
  }))

  luox <- do.call(rbind, lapply(spectrum_ids, function(id) {
    sheet <- utils::read.csv(input(
      article, "Results_Luox_2022-10-21", id, "download-calc.csv"
    ), check.names = FALSE, fileEncoding = "UTF-8")
    rows <- c(1L, 6:15, 22L, 24L)
    # Fail visibly if the layout changes; do not silently extract other metrics.
    labels <- trimws(sheet[rows, 1])
    stopifnot(labels[1] == "Illuminance (lx)",
      all(startsWith(labels[2:6], c("S-cone-opic irradiance",
        "M-cone-opic irradiance", "L-cone-opic irradiance",
        "Rhodopic irradiance", "Melanopic irradiance"))),
      all(startsWith(labels[7:11], c("S-cone-opic EDI", "M-cone-opic EDI",
        "L-cone-opic EDI", "Rhodopic EDI", "Melanopic EDI"))),
      labels[12] == "CCT (K) - Robertson, 1968",
      labels[13] == "Colour Rendering Index [Ra]")
    expected <- as.numeric(sheet[rows, 2])
    stopifnot(length(expected) == 13L, all(is.finite(expected)),
      all(expected > 0))
    result <- data.frame(spectrum = id, metric = c(metrics, "cct", "cri"),
      unit = c(units, "K", "Ra"), expected = expected)
    result$applicable <- !(id %in% c("EE_Spektrum", "LED_4000K_2") &
      result$metric == "cri")
    result
  }))

  din_inputs <- file.path(article, "material-validation-data")
  din_all <- utils::read.csv(input(din_inputs,
    "din-67600-material-coefficients.csv"), fileEncoding = "UTF-8")
  assignments <- utils::read.csv(input(din_inputs, "tub-assignments.csv"))
  stopifnot(nrow(din_all) == 260L, nrow(assignments) == 39L,
    !anyDuplicated(din_all[c("din_id", "illuminant")]),
    !anyDuplicated(assignments$din_id))
  pairs <- merge(din_all, assignments, by = "din_id")
  din <- do.call(rbind, lapply(seq_len(nrow(pairs)), function(i) {
    row <- pairs[i, ]
    data.frame(din_id = row$din_id, catalogue_id = row$catalogue_id,
      mode = tolower(row$mode), illuminant = row$illuminant,
      metric = c("photopic_illuminance", "melanopic_irradiance"),
      unit = "fraction", expected = c(row$din_vis, row$din_mel),
      table = row$din_table_ref, page = row$page)
  }))
  stopifnot(nrow(din) == 456L, all(is.finite(din$expected)))

  # Exact known cases, not a general exception for all FL11 results.
  # DIN 414 and 415 already satisfy the normal rounding budget.
  exception_ids <- c(401:413, 416:427)
  din$known_issue <- din$din_id %in% exception_ids &
    din$mode == "reflection" & din$illuminant == "FL11" &
    din$metric == "photopic_illuminance"

  # These are regression anchors, NOT corrected DIN reference values.
  # Derive them with explicit weighted sums without calling Spectran code.
  # The input catalogue, illuminant and response are the bundled data used by
  # the article. Their hashes below pin the scope of this regression evidence.
  bundled <- new.env(parent = emptyenv())
  for (path in c("R/sysdata.rda", "data/examplespectra.rda", "data/Specs.rda"))
    load(input(path), envir = bundled)
  source <- stats::approx(bundled$examplespectra$CIE$Wellenlaenge,
    bundled$examplespectra$CIE$FL11, xout = 380:780)$y
  weight <- bundled$Specs$AS_wide[["V(lambda)"]]
  stopifnot(identical(bundled$Specs$AS_wide$Wellenlaenge, as.numeric(380:780)),
    all(is.finite(source)), all(is.finite(weight)))
  fl11 <- din[din$known_issue, c("din_id", "catalogue_id")]
  fl11$regression_value <- vapply(fl11$catalogue_id, function(id) {
    curve <- bundled$tub_material_curves[
      bundled$tub_material_curves$catalogue_id == id, ]
    stopifnot(nrow(curve) == 81L)
    coefficient <- stats::approx(curve$wavelength_nm, curve$transmittance,
      xout = 380:780)$y
    round(sum(source * coefficient * weight) / sum(source * weight), 6)
  }, numeric(1))
  stopifnot(nrow(fl11) == 25L, all(is.finite(fl11$regression_value)),
    all(fl11$regression_value - din$expected[din$known_issue] < -0.001))
  missing_ids <- sort(setdiff(din_all$din_id, assignments$din_id))
  stopifnot(length(missing_ids) == 32L)

  references <- list(
    schema_version = 1L,
    spectra = spectra,
    cie = cie,
    luox = luox,
    din = list(expected = din, fl11_regression = fl11,
      unmatched_material_ids = missing_ids),
    provenance = list(
      cie = "CIE S 026 alpha-opic Toolbox v1.049a (2020/11); exported 2022-10-21",
      luox = "luox exports dated 2022-10-21; Robertson (1968) CCT",
      din = "DIN/TS 67600:2022-08, Tables 6, 9 and 10; TUB specimen assignments",
      fl11 = paste("Known photopic reflection discrepancy: correspondence",
        "between Johannes Zauner and Dieter Lang, 2026-10-08;",
        "see Material_Validation.qmd. Regression anchors are explicit sums,",
        "not independent or corrected DIN values."),
      command = "Rscript --vanilla data-raw/validation_references.R",
      R = R.version.string,
      readxl = as.character(utils::packageVersion("readxl")),
      inputs = data.frame(path = inputs,
        md5 = unname(tools::md5sum(inputs)))
    )
  )
  destination <- "tests/testthat/fixtures/validation-references.rds"
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  saveRDS(references, destination, compress = "xz", version = 2)
  # Readable audit copies let reviewers inspect every expected value, exception
  # and source checksum without an R runtime or a binary-file viewer.
  audit <- rbind(
    data.frame(source = "CIE", case = cie$spectrum, metric = cie$metric,
      unit = cie$unit, reference_value = cie$expected, status = "compare",
      fl11_regression_anchor = NA_real_),
    data.frame(source = "luox", case = luox$spectrum, metric = luox$metric,
      unit = luox$unit, reference_value = luox$expected,
      status = ifelse(luox$applicable, "compare", "inapplicable_cri"),
      fl11_regression_anchor = NA_real_),
    data.frame(source = "DIN",
      case = paste(din$din_id, din$catalogue_id, din$illuminant, sep = " | "),
      metric = din$metric, unit = din$unit, reference_value = din$expected,
      status = ifelse(din$known_issue, "known_fl11_discrepancy", "compare"),
      fl11_regression_anchor = ifelse(din$known_issue,
        fl11$regression_value[match(din$din_id, fl11$din_id)], NA_real_))
  )
  for (column in c("reference_value", "fl11_regression_anchor"))
    audit[[column]] <- ifelse(is.na(audit[[column]]), NA_character_,
      sprintf("%.17g", audit[[column]]))
  utils::write.csv(audit, file.path(dirname(destination), "reference-values.csv"),
    row.names = FALSE, na = "")
  utils::write.csv(references$provenance$inputs,
    file.path(dirname(destination), "reference-input-checksums.csv"),
    row.names = FALSE)
  message("Wrote ", destination, " (", file.info(destination)$size, " bytes)")
})
