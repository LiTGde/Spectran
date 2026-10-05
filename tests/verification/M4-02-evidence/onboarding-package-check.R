source("/private/tmp/spectran-revision/load.R")
args <- commandArgs(trailingOnly = TRUE)
version <- if (length(args)) args[[1L]] else "M4-01"
result <- rcmdcheck::rcmdcheck(path = file.path("/private/tmp/spectran-review-oct03", version, "source"),
  args = c("--no-tests", "--no-manual", "--as-cran"), build_args = "--no-build-vignettes",
  check_dir = paste0("/private/tmp/spectran-revision/check-", version), error_on = "never",
  env = c("_R_CHECK_CRAN_INCOMING_REMOTE_" = "false", "_R_CHECK_CRAN_INCOMING_" = "false",
    "_R_CHECK_FORCE_SUGGESTS_" = "false", "_R_CHECK_SYSTEM_CLOCK_" = "false", "NOT_CRAN" = "true"))
saveRDS(result, paste0("/private/tmp/spectran-revision/", version, "-check-results.rds"))
cat("Errors:", length(result$errors), "Warnings:", length(result$warnings), "Notes:", length(result$notes), "\n")
