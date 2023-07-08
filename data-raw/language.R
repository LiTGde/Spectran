## code to prepare `language` dataset goes here

library(readxl)
library(tidyverse)
library(here)
library(usethis)

path <- paste0(here(), "/data-raw/languages.xlsx")

sheets <- c("global", "ui", "server")

language <- map(sheets, read_excel, path = path, trim_ws = FALSE)

names(language) <- sheets

# usethis::use_data(language, overwrite = TRUE)

