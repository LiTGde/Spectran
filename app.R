pkgload::load_all(".")
config_env <- Sys.getenv("CONFIG_ENV", "default")
settings <- config::get(file = "_shiny_config.yml", config = config_env)
Spectran(lang_setting = settings$language, lang_link = TRUE)

