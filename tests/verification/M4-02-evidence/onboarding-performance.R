Sys.setenv(NOT_CRAN = "true")
source("/private/tmp/spectran-revision/load.R")
args <- commandArgs(trailingOnly = TRUE)
version <- if (length(args)) args[[1L]] else "M4-02"
build <- file.path("/private/tmp/spectran-review-oct03", version)
work <- file.path("/private/tmp/spectran-revision", paste0(version, "-performance"))
dir.create(work, showWarnings = FALSE)
app_source <- c(
  'Sys.setenv(R_USER_CACHE_DIR = "/private/tmp/spectran-revision/performance-cache")',
  '.libPaths(c("/Users/zauner/Library/Caches/org.R-project.R/R/renv/library/Spectran-fd34ab45/macos/R-4.6/aarch64-apple-darwin23", .libPaths()))',
  paste0('pkgload::load_all("', build, '/source", quiet = TRUE)'))
for (condition in c("with_help", "without_help")) {
  folder <- file.path(work, condition)
  dir.create(folder, showWarnings = FALSE)
  stub <- if (condition == "without_help") c(
    'assignInNamespace("spectran_explanations_ui", function(id) NULL, ns = "Spectran")',
    'assignInNamespace("spectran_explanations_server", function(id, active_page, navigate) NULL, ns = "Spectran")',
    'assignInNamespace("spectran_explanation_links_ui", function(id) stats::setNames(rep(list(NULL), length(Spectran:::spectran_explanation_routes())), names(Spectran:::spectran_explanation_routes())), ns = "Spectran")'
  ) else character()
  writeLines(c(app_source, stub, 'Spectran::Spectran("Deutsch")'), file.path(folder, "app.R"))
}
measure <- function(condition, index, warmup = FALSE) {
  gc()
  start <- proc.time()[["elapsed"]]
  app <- shinytest2::AppDriver$new(app_dir = file.path(work, condition),
    name = paste0("performance-", condition, "-", index), width = 1200, height = 900,
    load_timeout = 40000, timeout = 20000, seed = 1L)
  elapsed <- proc.time()[["elapsed"]] - start
  on.exit(app$stop())
  resources <- app$get_js("performance.getEntriesByType('resource').map(function(x){return x.name;})")
  row <- data.frame(condition = condition, index = index, warmup = warmup,
    app_driver_ready_s = elapsed,
    explanation_svg_requests = sum(grepl("/explanations/", resources, fixed = TRUE)),
    stringsAsFactors = FALSE)
  cat(condition, index, sprintf("%.3f s", elapsed), "SVG requests:", row$explanation_svg_requests, "\n")
  flush.console()
  row
}
# First use of each variant is a warm-up. Each measurement starts a fresh R
# server and browser page. ABBA order reduces bias from local warm-cache drift.
order <- c("with_help", "without_help", rep(c("with_help", "without_help", "without_help", "with_help"), 2L))
rows <- lapply(seq_along(order), function(i) measure(order[[i]], i, i <= 2L))
result <- do.call(rbind, rows)
write.csv(result, file.path(work, "startup-measurements.csv"), row.names = FALSE)
summaries <- do.call(rbind, lapply(split(result[!result$warmup, ], result$condition[!result$warmup]), function(x) {
  data.frame(condition = x$condition[[1L]], n = nrow(x),
    median_s = median(x$app_driver_ready_s), min_s = min(x$app_driver_ready_s), max_s = max(x$app_driver_ready_s))
}))
write.csv(summaries, file.path(work, "startup-summary.csv"), row.names = FALSE)
print(summaries, row.names = FALSE)
writeLines(c(paste("Build:", readLines(file.path(build, "build-id.txt"))),
  "Command: Rscript --vanilla /private/tmp/spectran-revision/onboarding-performance.R M4-02",
  "Local Chromium / shinytest2 initial AppDriver-ready elapsed wall time, including fresh R server launch and package loading.",
  "Comparison holds introduction, sidebar and CSS constant; only explanation UI, server and contextual help buttons are temporarily stubbed out in the benchmark process.",
  "This is not hosted cold-start latency. File-system and browser executable caches may be warm. Both variant warm-ups excluded.",
  capture.output(sessionInfo())), file.path(work, "provenance.txt"))
