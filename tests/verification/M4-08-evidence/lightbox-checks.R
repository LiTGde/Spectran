source("/private/tmp/spectran-revision/load.R")
pdf(file = NULL)
for (file in c("R/introduction.R", "R/impressum.R")) invisible(parse(file))
settings <- Spectran:::the
for (language in c("Deutsch", "English")) {
  settings$language <- language
  # Validate every trigger against its dialog in two independently namespaced UIs.
  markup <- htmltools::renderTags(htmltools::tagList(
    Spectran:::introductionUI("intro"), Spectran:::introductionUI("second")))$html
  doc <- xml2::read_html(markup)
  ids <- xml2::xml_attr(xml2::xml_find_all(doc, "//*[@id]"), "id")
  stopifnot(!anyDuplicated(ids))
  triggers <- xml2::xml_find_all(doc, "//button[@data-toggle='modal']")
  stopifnot(length(triggers) == 24L)
  for (trigger in triggers) {
    target <- substring(xml2::xml_attr(trigger, "data-target"), 2L)
    dialog <- xml2::xml_find_first(doc, paste0("//*[@id='", target, "']"))
    stopifnot(!inherits(dialog, "xml_missing"),
      identical(xml2::xml_attr(dialog, "role"), "dialog"),
      identical(xml2::xml_attr(xml2::xml_find_first(trigger, ".//img"), "src"),
        xml2::xml_attr(xml2::xml_find_first(dialog, ".//img"), "src")))
    path <- xml2::xml_attr(xml2::xml_find_first(dialog, ".//img"), "src")
    stopifnot(file.exists(file.path("inst/app/www", sub("^extr/", "", path))))
  }
  stopifnot(length(xml2::xml_find_all(doc, "//nav[@class='spectran-intro-jump-links']")) == 0L)
  cat(language, ": all 12 image dialogs resolve to the correct local image in both module instances; unique IDs\n")
}
testthat::test_file("tests/testthat/test-introduction-navigation.R", reporter = "summary")
cat(R.version.string, "\n")
for (package in c("shiny", "shinydashboard", "htmltools", "testthat", "xml2"))
  cat(package, as.character(packageVersion(package)), "\n")
