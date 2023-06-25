#Run a Module App with correct dependencies on the www folder

run_app <- function(lang_setting = NULL) {
  #If no language is chosen, the last language will be used:
  if (!is.null(lang_setting)) {
    usethis::write_over(
      paste0(system.file("app", package = "Spectran"), "/app.R"), 
      paste0("pkgload::load_all('.')\nanalysis_setupApp('", lang_setting, "')"),
      quiet = TRUE
      )
  }
  #Run the app:
  shiny::runApp(appDir = system.file("app", package = "Spectran"))
}

