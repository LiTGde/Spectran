pkgload::load_all(".")
# Sys.setenv(CONFIG_ENV = "deutsch")
config_env <- Sys.getenv("CONFIG_ENV", "default")
settings <- config::get(file = "_shiny_config.yml", config = config_env)
Spectran(lang_setting = settings$language, lang_link = TRUE)

