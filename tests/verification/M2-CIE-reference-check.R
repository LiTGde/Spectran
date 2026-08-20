# Milestone 2 checksum-pinned comparison with official CIE data tables

renv::load()
pkgload::load_all(quiet = TRUE)

urls <- c(
  alpha = "https://files.cie.co.at/CIE_a-opic_action_spectra.csv",
  d65 = "https://files.cie.co.at/CIE_std_illum_D65.csv",
  photopic = "https://files.cie.co.at/CIE_sle_photopic.csv"
)
expected_md5 <- c(
  alpha = "f1ddfef144176812c3cb9d8fba1f3141",
  d65 = "03d4eb9b837c60671627c946fb534deb",
  photopic = "f389958555461a7d9a7562145e8ca9c0"
)
paths <- stats::setNames(
  file.path(tempdir(), basename(urls)),
  names(urls)
)
for (index in seq_along(urls)) {
  utils::download.file(
    urls[[index]],
    paths[[index]],
    mode = "wb",
    quiet = TRUE
  )
}
actual_md5 <- unname(tools::md5sum(paths))
stopifnot(identical(actual_md5, unname(expected_md5)))

alpha <- utils::read.csv(paths[["alpha"]], header = FALSE)
d65 <- utils::read.csv(paths[["d65"]], header = FALSE)
photopic <- utils::read.csv(paths[["photopic"]], header = FALSE)
alpha <- alpha[alpha[[1]] >= 380 & alpha[[1]] <= 780, ]
d65 <- d65[d65[[1]] >= 380 & d65[[1]] <= 780, ]
photopic <- photopic[photopic[[1]] >= 380 & photopic[[1]] <= 780, ]

# CIE metadata prescribes zero extrapolation for blank action-spectrum cells.
alpha[is.na(alpha)] <- 0
official_alpha <- as.matrix(alpha[, 2:6])
bundled_alpha <- as.matrix(Specs$AS_wide[, c(
  "S-cone-opsin",
  "M-cone-opsin",
  "L-cone-opsin",
  "Rhodopsin",
  "Melanopsin"
)])
alpha_max_abs_difference <- max(abs(bundled_alpha - official_alpha))

official_photopic <- photopic[[2]]
bundled_photopic <- Specs$AS_wide[["V(lambda)"]]
photopic_max_abs_difference <- max(abs(
  bundled_photopic - official_photopic
))

official_d65 <- d65[[2]] / d65[[2]][d65[[1]] == 560]
bundled_d65_raw <- d65_visible_spectrum()$Bestrahlungsstaerke
bundled_d65 <- bundled_d65_raw / bundled_d65_raw[d65[[1]] == 560]
d65_max_abs_difference <- max(abs(bundled_d65 - official_d65))

# Derive the six Spectran efficacy constants from official D65 and responses.
official_weights <- cbind(
  alpha[, c(6, 4, 3, 2, 5)],
  official_photopic
)
official_d65_at_one_lux <- d65[[2]] /
  sum(d65[[2]] * official_photopic) /
  683.0015
derived_efficacy <- 1 / colSums(official_d65_at_one_lux * official_weights)
names(derived_efficacy) <- c(
  "melanopic",
  "L-cone-opic",
  "M-cone-opic",
  "S-cone-opic",
  "rhodopic",
  "photopic"
)
bundled_efficacy <- unlist(Specs$Efficacy)
efficacy_relative_difference <- derived_efficacy / bundled_efficacy - 1

# Spectran's bundled values are rounded versions of the CIE data and remain
# authoritative for compatibility with existing Analysis outputs.
numerical_slack <- .Machine$double.eps
stopifnot(alpha_max_abs_difference <= 5e-4 + numerical_slack)
stopifnot(photopic_max_abs_difference <= 5e-7 + numerical_slack)
stopifnot(d65_max_abs_difference <= 8e-6 + numerical_slack)
stopifnot(max(abs(efficacy_relative_difference)) <= 2e-5 + numerical_slack)

comparison <- list(
  checksums = actual_md5,
  alpha_max_abs_difference = alpha_max_abs_difference,
  photopic_max_abs_difference = photopic_max_abs_difference,
  d65_normalized_max_abs_difference = d65_max_abs_difference,
  efficacy = cbind(
    derived = derived_efficacy,
    bundled = bundled_efficacy,
    relative_difference = efficacy_relative_difference
  )
)
print(comparison)
cat("Milestone 2 official CIE reference checks passed.\n")
