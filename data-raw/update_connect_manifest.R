# Refresh Connect Cloud dependencies after updating DESCRIPTION or renv.lock.
# Run from the repository root with its renv project active:
# source("data-raw/update_connect_manifest.R")
# Commit manifest.json with the app changes before publishing from GitHub.

stopifnot(file.exists("app.R"), file.exists("DESCRIPTION"), file.exists("renv.lock"))
if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop("rsconnect must be available in the project library.", call. = FALSE)
}

tracked <- system2("git", c("-c", "core.quotepath=false", "ls-files"), stdout = TRUE)
if (!is.null(attr(tracked, "status"))) {
  stop("Cannot list the files tracked in Git.", call. = FALSE)
}
startup <- c("app.R", "DESCRIPTION", "NAMESPACE", "LICENSE", "LICENSE.md",
  ".Rprofile", ".Renviron", "renv.lock", "renv/activate.R", "renv/settings.json")
app_files <- tracked[tracked %in% startup | grepl("^(R|data|inst|man)/", tracked)]
stopifnot(all(c("app.R", "DESCRIPTION", "NAMESPACE", "renv.lock") %in% app_files),
  all(file.exists(app_files)))

rsconnect::writeManifest(appDir = ".", appFiles = app_files,
  appPrimaryDoc = "app.R", appMode = "shiny")
