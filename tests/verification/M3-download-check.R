# Milestone 3 browser-download verification

paths <- list(
  completed = "/tmp/spectran-m3-branch-completed.csv",
  comparison = "/tmp/spectran-m3-branch-comparison.csv",
  d65 = "/tmp/spectran-m3-branch-d65.csv",
  metrics = "/tmp/spectran-m3-branch-metrics.csv",
  history = "/tmp/spectran-m3-branch-history.csv",
  audit = "/tmp/spectran-m3-branch-audit.zip"
)

stopifnot(all(file.exists(unlist(paths))))
completed <- read.csv(paths$completed, stringsAsFactors = FALSE)
comparison <- read.csv(paths$comparison, stringsAsFactors = FALSE)
d65 <- read.csv(paths$d65, stringsAsFactors = FALSE)
metrics <- read.csv(paths$metrics, stringsAsFactors = FALSE)
history <- read.csv(paths$history, stringsAsFactors = FALSE)

stopifnot(
  nrow(completed) == 401L,
  identical(completed$wavelength_nm, 380:780),
  all(completed$transmittance >= 0 & completed$transmittance <= 1),
  nrow(comparison) == 401L,
  isTRUE(all.equal(
    comparison$transmitted_spectral_irradiance_w_m2_nm,
    comparison$incident_spectral_irradiance_w_m2_nm *
      comparison$transmittance,
    tolerance = 1e-12
  )),
  nrow(d65) == 6L,
  all(d65$scope == "d65_filter"),
  nrow(metrics) == 22L,
  all(metrics$scope == "active_source"),
  nrow(history) == 4L,
  identical(history$node_id, paste0("node-", 1:4)),
  identical(
    history$parent_id[c(2L, 3L, 4L)],
    c(
      "node-1",
      "node-2",
      "node-1"
    )
  ),
  identical(which(history$active), 4L)
)

extract_directory <- tempfile("spectran-m3-audit-")
dir.create(extract_directory)
on.exit(unlink(extract_directory, recursive = TRUE, force = TRUE), add = TRUE)
archive_listing <- unzip(paths$audit, list = TRUE)$Name
required_archive_files <- c(
  "README.txt",
  "input/development-fixture-record.csv",
  "input/input-record-metadata.csv",
  "input/parsed-curve.csv",
  "filter/completed-filter.csv",
  "filter/completion-status.csv",
  "spectra/incident-spectrum.csv",
  "spectra/transmitted-spectrum.csv",
  "spectra/spectral-comparison.csv",
  "metrics/d65-properties.csv",
  "metrics/active-metrics.csv",
  "metrics/all-metrics.csv",
  "history/history-tree.csv",
  "history/node-spectra.csv",
  "metadata/applied-metadata.csv",
  "metadata/decisions-warnings.csv",
  "citations-licenses.csv",
  "manifest.csv"
)
stopifnot(setequal(archive_listing, required_archive_files))
unzip(paths$audit, exdir = extract_directory)

archived_history <- read.csv(
  file.path(extract_directory, "history", "history-tree.csv"),
  stringsAsFactors = FALSE
)
archived_spectra <- read.csv(
  file.path(extract_directory, "history", "node-spectra.csv"),
  stringsAsFactors = FALSE
)
manifest <- read.csv(
  file.path(extract_directory, "manifest.csv"),
  stringsAsFactors = FALSE
)
readme <- readLines(
  file.path(extract_directory, "README.txt"),
  encoding = "UTF-8"
)

stopifnot(
  identical(archived_history$node_id, history$node_id),
  identical(archived_history$parent_id, history$parent_id),
  identical(archived_history$active, history$active),
  nrow(archived_spectra) == 4L * 401L,
  all(table(archived_spectra$node_id) == 401L),
  all(manifest$bytes > 0L),
  all(nchar(manifest$md5) == 32L),
  any(readme == "English"),
  any(readme == "Deutsch"),
  any(grepl("Active history node: node-4", readme, fixed = TRUE))
)

cat(R.version.string, "\n")
cat("Spectran:", as.character(utils::packageVersion("Spectran")), "\n")
cat("zip:", as.character(utils::packageVersion("zip")), "\n")
cat("All Milestone 3 browser downloads and audit contents verified.\n")
