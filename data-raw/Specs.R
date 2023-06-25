## code to prepare `SpecCharacteristics` dataset goes here

#Action spectra and other characteristics
library(tidyverse)
library(here)

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

#saving common terminology in a frame
Alpha <- list()

Alpha$names <-
  c("Melanopsin",
    "Erythropsin",
    "Chloropsin",
    "Cyanopsin",
    "Rhodopsin")

Alpha$adjectives <- 
  map(setNames(nm =c("Deutsch", "English")), 
      \(x) {unlist(map(1:5, lang$global, language_direct = x))}) %>% list2DF()

Alpha$descriptions <- map(setNames(nm =c("Deutsch", "English")), \(x) {unlist(map(6:10, lang$global, language_direct = x))}) %>% list2DF()

Alpha$abb <- c("M", "E", "Ch", "Cy", "R")

#Saving Plot colors and peaks for the plot
Plot <- tibble(Peak = 1:6)
Plot$Names <- c(Alpha$names, "V(lambda)")
Plot$Abbr <- c("mel", "lc", "mc", "sc", "rh", "V(lambda)")
Plot$Col <- c("#1D63DC",
              "darkred",
              "limegreen",
              "darkviolet",
              "grey60",
              "gold1")
Plot$Peak <- c(490, 569, 541, 448, 507, 555)
Plot <- Plot %>% mutate(across(everything(), \(x) {set_names(x, nm = Names)}))


Specs <- list(
  Action_Spectra_wide,
  Action_Spectra_long,
  Efficacy %>% as.list(),
  Alpha,
  "V(\\(\\lambda\\))",
  Plot
)

names(Specs) <- 
  c("AS_wide", "AS_long", "Efficacy", "Alpha", "Vlambda", "Plot")

usethis::use_data(Specs, overwrite = TRUE)
