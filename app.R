pkgload::load_all(".")
# Connect Cloud sets these per deployment, so both prototypes use this file.
# The uppercase setting takes precedence over the legacy .Renviron language.
app_language <- Sys.getenv(
  "SPECTRAN_LANGUAGE",
  unset = Sys.getenv("language", unset = "Deutsch")
)
show_language_link <- identical(
  tolower(Sys.getenv("SPECTRAN_LANG_LINK", unset = "true")),
  "true"
)
Spectran(lang_setting = app_language, lang_link = show_language_link)

