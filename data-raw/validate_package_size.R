# Maintainer release check. Run on an R CMD build tarball and, optionally,
# the corresponding installed package directory from R CMD check.
#
# Rscript --vanilla data-raw/validate_package_size.R \
#   /tmp/Spectran_1.0.6.tar.gz /tmp/Spectran.Rcheck/Spectran
#
# These conservative project budgets are 5 decimal MB each. CRAN's policy
# distinguishes data/documentation (normally 5 MB each) from source archives
# (preferably at most 10 MB):
# https://cran.r-project.org/web/packages/policies.html
# Repository README screenshots and review records stay in the repository.

arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) < 1L || length(arguments) > 2L) {
  stop(
    "Supply a source .tar.gz path and optionally its installed package directory.",
    call. = FALSE
  )
}
archive <- normalizePath(arguments[[1L]], mustWork = TRUE)
if (!grepl("\\.tar\\.gz$", archive)) {
  stop("The first argument must be an R source .tar.gz archive.", call. = FALSE)
}
entries <- utils::untar(archive, list = TRUE)
if (
  length(entries) == 0L ||
    any(grepl("(^/|(^|/)\\.\\.(/|$)|\\\\)", entries)) ||
    !all(grepl("^Spectran(/|$)", entries))
) {
  stop(
    "Expected a Spectran archive with only package-relative paths.",
    call. = FALSE
  )
}
excluded <- c(
  "^Spectran/README\\.md$",
  "^Spectran/man/figures/English(/|$)",
  "^Spectran/tests/verification(/|$)",
  "/Rplots\\.pdf$",
  "^Spectran/\\.Renviron$",
  "^Spectran/(renv|rsconnect|data-raw)(/|$)"
)
if (any(grepl(paste(excluded, collapse = "|"), entries))) {
  stop(
    "The source archive contains excluded development or gallery files.",
    call. = FALSE
  )
}

local({
  unpacked <- tempfile("spectran-size-")
  dir.create(unpacked)
  on.exit(unlink(unpacked, recursive = TRUE), add = TRUE)
  utils::untar(archive, exdir = unpacked)
  root <- file.path(unpacked, "Spectran")
  files <- list.files(
    root,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  sizes <- file.info(files)$size
  relative <- substring(files, nchar(root) + 2L)
  data_files <- grepl("^(data/|R/sysdata\\.rda$|inst/extdata/)", relative)
  # Include all app resources in the documentation budget, conservatively.
  documentation <- grepl("^(man/|vignettes/|inst/doc/|inst/app/)", relative) |
    grepl("\\.(md|pdf|html)$", relative, ignore.case = TRUE)
  report <- data.frame(
    component = c(
      "Source archive",
      "Bundled data",
      "Documentation and app resources"
    ),
    bytes = c(
      file.info(archive)$size,
      sum(sizes[data_files]),
      sum(sizes[documentation])
    ),
    budget_bytes = 5000000
  )
  if (length(arguments) == 2L) {
    installed <- normalizePath(arguments[[2L]], mustWork = TRUE)
    description <- file.path(installed, "DESCRIPTION")
    if (
      !file.exists(description) ||
        read.dcf(description)[1L, "Package"] != "Spectran"
    ) {
      stop(
        "The second argument must be the installed Spectran package directory.",
        call. = FALSE
      )
    }
    installed_files <- list.files(
      installed,
      recursive = TRUE,
      full.names = TRUE,
      all.files = TRUE,
      no.. = TRUE
    )
    installed_info <- file.info(installed_files)
    report <- rbind(
      report,
      data.frame(
        component = "Installed package",
        bytes = sum(installed_info$size[!installed_info$isdir]),
        budget_bytes = 5000000
      )
    )
  }
  report$megabytes <- round(report$bytes / 1000000, 3L)
  report$within_budget <- report$bytes <= report$budget_bytes
  cat(R.version.string, "\nArchive:", archive, "\n")
  print(report, row.names = FALSE)
  cat("Archive MD5:", unname(tools::md5sum(archive)), "\n")
  if (!all(report$within_budget)) {
    stop("Spectran exceeds its 5 MB release size budget.", call. = FALSE)
  }
})
