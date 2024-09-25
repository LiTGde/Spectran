pkgload::load_all(".")
# config_env <- Sys.getenv("CONFIG_ENV", "deutsch")
# config_env <- Sys.getenv("CONFIG_ENV")
# settings <- config::get(file = "_shiny_config.yml", config = config_env)
# Spectran(lang_setting = settings$language, lang_link = TRUE)
Spectran(lang_setting = Sys.getenv("CONFIG_ENV"), lang_link = TRUE)

