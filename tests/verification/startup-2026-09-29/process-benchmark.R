# Repeat fresh R-process startup measurements in alternating order.
args <- commandArgs(trailingOnly=TRUE)
root <- '/private/tmp/spectran-startup-2026-09-29'
paths <- c(original=file.path(root,'original'),prototype=file.path(root,'prototype'),b08='/private/tmp/spectran-ux-review/B08/Spectran',b09='/Users/zauner/Documents/Gremienarbeit/TWA/Projekte/Spectran/Spectran')
for (run in 1:5) {
  order <- if (run %% 2) names(paths) else rev(names(paths))
  for (label in order) {
    prefix <- file.path(root,paste0(label,'-process-',run))
    status <- system2(file.path(R.home('bin'),'Rscript'), c('--vanilla',shQuote(file.path(root,'probe.R')),shQuote(paths[[label]]),shQuote(prefix)),stdout=paste0(prefix,'.log'),stderr=paste0(prefix,'.log'))
    stopifnot(status==0L)
  }
}
