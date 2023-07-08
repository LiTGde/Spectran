files <- list.files(path = "data-raw", pattern = "[.]R$", full.names = TRUE)

purrr::map(files, source)

usethis::use_data(ColorP, 
                  examplespectra, 
                  examplespectra_descriptor, 
                  Specs,
                  language,
                  internal = TRUE,
                  overwrite = TRUE)

rm(list = ls())
