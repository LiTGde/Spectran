# Summarize the before/after startup and material-navigation measurements in R.
root <- "/private/tmp/spectran-startup-lazy-2026-09-29"
read_measurements <- function(pattern) {
  paths <- list.files(root, pattern, full.names = TRUE)
  do.call(rbind, lapply(paths, function(path) {
    x <- read.csv(path)
    x$file <- basename(path)
    x$recorded_at <- as.character(file.info(path)$mtime)
    x
  }))
}
startup <- read_measurements("-browser.csv$")
startup <- startup[order(startup$recorded_at), ]
startup$run <- ave(seq_len(nrow(startup)), startup$label, FUN = seq_along)
startup$phase <- ifelse(startup$run == 1L, "first_session", "repeat")
visits <- read_measurements("-material-[12].csv$")
visits <- merge(visits, startup[c("session", "phase", "run")], by = "session")
write.csv(startup, file.path(root, "startup-measurements.csv"), row.names = FALSE)
write.csv(visits, file.path(root, "material-measurements.csv"), row.names = FALSE)
warm <- startup[startup$phase == "repeat", ]
stopifnot(all(table(warm$label) == 5L))
summary <- do.call(rbind, lapply(split(warm, warm$label), function(x) {
  visit <- visits[visits$label == x$label[[1L]] & visits$phase == "repeat", ]
  first <- visit$elapsed_ms[visit$visit == 1L]
  second <- visit$elapsed_ms[visit$visit == 2L]
  stopifnot(length(first) == 5L, length(second) == 5L)
  data.frame(
    label = x$label[[1L]], n = nrow(x),
    startup_median_ms = median(x$ready_ms),
    startup_min_ms = min(x$ready_ms), startup_max_ms = max(x$ready_ms),
    first_visit_median_ms = median(first),
    first_visit_min_ms = min(first), first_visit_max_ms = max(first),
    repeat_visit_median_ms = median(second),
    repeat_visit_min_ms = min(second), repeat_visit_max_ms = max(second)
  )
}))
write.csv(summary, file.path(root, "summary.csv"), row.names = FALSE)
print(summary, row.names = FALSE)
before <- summary[summary$label == "before_lazy", ]
after <- summary[summary$label == "lazy", ]
cat("Startup saved ms:", before$startup_median_ms - after$startup_median_ms,
    "; reduction %:", 100 * (1 - after$startup_median_ms / before$startup_median_ms), "\n")
cat("First-visit added ms:", after$first_visit_median_ms - before$first_visit_median_ms, "\n")
sink(file.path(root, "sessionInfo.txt")); print(sessionInfo()); sink()
