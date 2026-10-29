# Run from the package root in the project's R environment:
# Rscript tests/verification/check-data-rebuild.R
# The existing catalogue builder requires photobiologyFilters 0.6.1 and its
# pinned Spitschan archive. Set SPECTRAN_SPITSCHAN_ARCHIVE for an offline run.
# SPECTRAN_DATA_REBUILD_CHECK_DIR optionally preserves the temporary package
# and evidence at a chosen, new path. No source-project files are modified.

local({
  repo <- normalizePath(getwd())
  qa_dir <- Sys.getenv("SPECTRAN_DATA_REBUILD_CHECK_DIR",
    unset = tempfile("spectran-data-rebuild-"))
  stopifnot(!dir.exists(qa_dir))
  dir.create(qa_dir, recursive = TRUE)
  stage <- file.path(qa_dir, "package")
  dir.create(stage)
  inputs <- c("DESCRIPTION", "NAMESPACE", "Spectran.Rproj", "data-raw", "R",
    "data", "inst", "manifest.json")
  stopifnot(all(file.copy(file.path(repo, inputs), stage, recursive = TRUE)))
  old_wd <- setwd(stage)
  on.exit(setwd(old_wd), add = TRUE)

  # A new maintenance helper must not become part of the dataset build.
  writeLines('stop("Unexpected maintenance script execution")',
    "data-raw/maintenance-regression-sentinel.R")
  protected <- c(list.files(c("data-raw", "inst"), recursive = TRUE,
    full.names = TRUE), "manifest.json")
  # The transmission translation source deliberately regenerates this workbook.
  # Its rebuilt contents are checked below through the internal language data.
  protected <- setdiff(protected, "data-raw/languages.xlsx")
  before <- tools::md5sum(protected)
  workspace_before <- as.list(.GlobalEnv, all.names = TRUE)
  caller <- new.env(parent = .GlobalEnv)
  caller$sentinel <- "preserve the caller's objects"
  source("data-raw/00_internal_data.R", local = caller)
  stopifnot(identical(as.list(caller),
    list(sentinel = "preserve the caller's objects")))
  stopifnot(identical(workspace_before, as.list(.GlobalEnv, all.names = TRUE)))
  stopifnot(identical(before, tools::md5sum(protected)))

  compare_rda <- function(relative) {
    original <- new.env(parent = emptyenv())
    rebuilt <- new.env(parent = emptyenv())
    expected <- load(file.path(repo, relative), envir = original)
    actual <- load(file.path(stage, relative), envir = rebuilt)
    stopifnot(setequal(expected, actual))
    comparisons <- vapply(expected, function(name) {
      result <- all.equal(original[[name]], rebuilt[[name]], tolerance = 0)
      if (isTRUE(result)) "equal" else paste(result, collapse = "; ")
    }, character(1))
    data.frame(file = relative, object = expected, comparison = comparisons)
  }
  outputs <- c("R/sysdata.rda", file.path("data", c("ColorP.rda",
    "examplespectra.rda", "examplespectra_descriptor.rda", "Specs.rda")))
  comparisons <- do.call(rbind, lapply(outputs, compare_rda))
  write.csv(comparisons, file.path(qa_dir, "comparisons.csv"), row.names = FALSE)
  print(comparisons, row.names = FALSE)
  writeLines(capture.output(sessionInfo()), file.path(qa_dir, "sessionInfo.txt"))
  packages <- c("dplyr", "tidyr", "purrr", "tibble", "readr", "readxl", "here",
    "tidyverse", "usethis", "photobiologyFilters", "R.matlab", "digest", "withr")
  write.csv(data.frame(package = packages, version = vapply(packages,
    function(p) as.character(packageVersion(p)), character(1))),
    file.path(qa_dir, "package-versions.csv"), row.names = FALSE)
  write.csv(data.frame(path = protected, md5 = unname(before)),
    file.path(qa_dir, "input-checksums.csv"), row.names = FALSE)
  stopifnot(all(comparisons$comparison == "equal"))
  cat("PASS: rebuilt datasets match; unrelated files, assets, manifest and",
    "caller workspace are unchanged.\nEvidence:", normalizePath(qa_dir), "\n")
})
