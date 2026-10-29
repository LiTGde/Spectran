# Rebuild bundled datasets from the package root:
# source("data-raw/00_internal_data.R")
#
# Keep an explicit dependency order, including the generated translations.
# Licensed reference preparation, validation, illustrations and deployment
# maintenance must run separately from a dataset rebuild.
withr::with_preserve_seed(local({
  library(dplyr)

  dataset_scripts <- c(
    "00_transmission_language.R",
    "language.R",
    "ColorP.R",
    "examplespectra.R",
    "Specs.R",
    "01_transmission_catalogue_localization.R",
    "transmission_catalogues.R",
    "tub_materials.R"
  )

  for (script in dataset_scripts) {
    source(file.path("data-raw", script), local = TRUE)
  }

  usethis::use_data(
    language,
    ColorP,
    examplespectra,
    examplespectra_descriptor,
    Specs,
    transmission_catalogue_curves,
    transmission_catalogue_provenance,
    transmission_catalogue_records,
    tub_material_records,
    tub_material_curves,
    tub_material_provenance,
    internal = TRUE,
    overwrite = TRUE
  )

  usethis::use_data(
    ColorP,
    examplespectra,
    examplespectra_descriptor,
    Specs,
    overwrite = TRUE
  )
}))
