source('/private/tmp/spectran-revision/load.R')
pdf(file = NULL)
for (f in c('R/introduction.R','R/spectran_explanations.R')) parse(f)
for (language in c('Deutsch', 'English')) {
  settings <- Spectran:::the
  settings$language <- language
  ui <- htmltools::renderTags(Spectran:::introductionUI('intro'))$html
  for (topic in names(Spectran:::spectran_explanation_topics())) {
    x <- htmltools::renderTags(Spectran:::spectran_explanation_content(topic))$html
    stopifnot(nzchar(x))
  }
  cat(language, ': introduction and ten topics render\n')
}
cat(R.version.string, '\n')
for (p in c('shiny', 'htmltools', 'shinydashboard')) cat(p, as.character(packageVersion(p)), '\n')
assets <- list.files('inst/app/www', recursive=TRUE, full.names=TRUE)
assets <- assets[!file.info(assets)$isdir]
new <- list.files('inst/app/www/intro-examples',full.names=TRUE)
print(data.frame(file=basename(new), bytes=file.info(new)$size))
cat('All web assets (bytes):', sum(file.info(assets)$size), '\n')
