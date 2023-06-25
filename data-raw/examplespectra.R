## code to prepare `examplespectra` dataset goes here
library(readxl)
library(here)
library(tidyverse)


# Spectral descriptor -----------------------------------------------------

#Defining the path and the languages used
path <- paste0(here(), "/data-raw/examplespectra.xlsx")
langs <- c("Deutsch", "English")

#importing the descriptors into a list
examplespectra_descriptor <- 
  map(langs, read_xlsx, path = path) %>% setNames(langs) %>% map(as_tibble)

#splitting the strings
examplespectra_descriptor <- 
map(examplespectra_descriptor, 
    \(y) {y %>% 
        rowwise() %>% 
        mutate(
          across(
            c(Identifier, Button_Name, Dateinamen), \(x) {str_split(x, ", ")}),
          embargo = as.logical(embargo)
        )
      }
    )

#creating a named list for the downloadbuttons
examplespectra_descriptor <- 
  map(examplespectra_descriptor, 
      \(x) {
        x %>% 
          mutate(download = list(setNames(Identifier, Button_Name)))})

usethis::use_data(examplespectra_descriptor, overwrite = TRUE)

# Spectra -----------------------------------------------------------------

path <- paste0(here(), "/data-raw/Rezeptorkurven_nm.csv")

Action_Spectra_wide <- read_csv(path)

Action_Spectra_long <-
  Action_Spectra_wide %>% pivot_longer(cols = -1,
                                       names_to = "Type",
                                       values_to = "rel_Sens")

Efficacy <-
  c(melanopic = 1 / 0.0013262,
    erytrhopic = 1 / 0.0016289,
    chloropic = 1 / 0.0014558,
    cyanopic = 1 / 0.0008173,
    rhodopic = 1 / 0.0014497,
    photopic = 683.0015478)

path <- paste0(here(), "/data-raw/example_spectra_1nm.csv")

Measurement_data <- read.csv(path)

path <- paste0(here(), "/data-raw/CIE_Standard_Illuminants.csv")

CIE_data <- read.csv(path)

#function that takes a data frame with spectra and scales those to 1 lux
scale_spectrum <- 
  function(spectrum_df, Action_Spectra_wide, Efficacy, Stepwidth = 1) {
  spectrum_df %>% 
  left_join(Action_Spectra_wide[,c("Wellenlaenge", "V(lambda)")]) %>%
  mutate(
    across(
      !all_of(c("Wellenlaenge", "V(lambda)")), 
      ~ .x/
        ((.x*`V(lambda)`) %>% sum(na.rm = TRUE)*Efficacy["photopic"]*Stepwidth)
      ),
    `V(lambda)` = NULL)
}

#Calculate photopic lux for a Spectrum (380:780nm)
Calc_lux <- 
  function(spectrum_vec, Action_Spectra_vec, Efficacy, Stepwidth = 1) {
  (spectrum_vec*Action_Spectra_vec[["V(lambda)"]]) %>% 
    sum(na.rm = TRUE)*Efficacy[["photopic"]]*Stepwidth
}

Measurement_data <-
  scale_spectrum(Measurement_data, Action_Spectra_wide, Efficacy)

CIE_data <-
  scale_spectrum(CIE_data, Action_Spectra_wide, Efficacy, 5)

# test %>% left_join(Action_Spectra_wide[,c("Wellenlaenge", "V(lambda)")]) %>% 
#   summarize(across(!all_of(c("Wellenlaenge", "V(lambda)")), ~ (.x*`V(lambda)`) %>% sum(na.rm = TRUE)*Efficacy[["photopic"]]*5))

# Measurement_data %>% summarize(across(!all_of(c("Wellenlaenge")),
#                           ~ Calc_lux(.x, Action_Spectra_wide, Efficacy, 1)))

examplespectra <- 
  list(Measurement_data, CIE_data) %>% setNames(c("Measurement", "CIE"))

usethis::use_data(examplespectra, overwrite = TRUE)
