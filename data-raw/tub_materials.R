# Offline, reproducible preparation of TUB version 2 (CC BY 4.0).
# Run from the package root; input CSVs are pinned in inst/extdata/tub.
# R >= 4.1; dplyr, purrr, tibble are package dependencies.
tub_ids <- list(
  reflection = c(
    paste0("WF", 1:10),
    paste0("S", 1:5),
    paste0("WC", 1:12),
    "L1"
  ),
  transmission = c(
    "PC1",
    "PC2",
    "PETG1",
    "PETG2",
    "SAN1",
    "SAN2",
    "XT1",
    "XT2",
    "GFR1",
    paste0("MSS", 1:6),
    paste0("G", 1:12)
  )
)
tub_names_en <- list(
  reflection = c(
    "Maple, varnished",
    "Maple, oiled",
    "Beech, varnished",
    "Beech, oiled",
    "Oak, varnished",
    "Oak, oiled",
    "Cherry, varnished",
    "Cherry, oiled",
    "Walnut, varnished",
    "Walnut, oiled",
    "Brick, beige",
    "Brick, red",
    "Concrete, fine-pored",
    "Concrete, medium-pored",
    "Concrete, coarse-pored",
    "Palazzo 235",
    "Palazzo 240",
    "Palazzo 90",
    "Marill 30",
    "Marill 120",
    "Siena 150",
    "Siena 120",
    "Siena 115",
    "Ceramic 90",
    "Ceramic 60",
    "Lachs 20",
    "Lachs 30",
    "Lacquer, white"
  ),
  transmission = c(
    "Polycarbonate, clear, 3 mm",
    "Polycarbonate, opal, 3 mm",
    "PETG, clear, 3 mm",
    "PETG, opal, 3 mm",
    "SAN, clear, 1.6 mm",
    "SAN, opal, 1.6 mm",
    "Extruded acrylic, clear, 3 mm",
    "Extruded acrylic, opal, 3 mm",
    "Glass-fibre reinforced plastic, clear, 1.1 mm",
    "Multi-skin sheet 10/4, clear",
    "Multi-skin sheet 10/4, opal",
    "Multi-skin sheet 16/7, clear",
    "Multi-skin sheet 16/7, opal",
    "Multi-skin sheet 20/7, clear",
    "Multi-skin sheet 20/7, opal",
    "FG",
    "FG-A-FG",
    "FG-A-LSG",
    "FG-Ar-TIG1",
    "FG-Ar-TIG2",
    "FG-Ar-TIG3",
    "SCG1-Ar-FG",
    "SCG2-Ar-FG",
    "SCG3-Ar-FG",
    "TIG1-Ar-FG-A-TIG1",
    "SCG1-Ar-FG-Ar-TIG1",
    "SCG2-Ar-FG-Ar-TIG3"
  )
)
tub_names_de <- list(
  reflection = c(
    "Ahorn, lackiert",
    "Ahorn, geölt",
    "Buche, lackiert",
    "Buche, geölt",
    "Eiche, lackiert",
    "Eiche, geölt",
    "Kirschbaum, lackiert",
    "Kirschbaum, geölt",
    "Nussbaum, lackiert",
    "Nussbaum, geölt",
    "Backstein, gelb (TUB: beige)",
    "Backstein, rot",
    "Beton fein",
    "Beton mittel",
    "Beton grob",
    tub_names_en$reflection[16:27],
    "Lack, weiß"
  ),
  transmission = c(
    "Polycarbonat, klar, 3 mm",
    "Polycarbonat, opal, 3 mm",
    "PETG, klar, 3 mm",
    "PETG, opal, 3 mm",
    "SAN, klar, 1,6 mm",
    "SAN, opal, 1,6 mm",
    "Acrylglas extrudiert, klar, 3 mm",
    "Acrylglas extrudiert, opal, 3 mm",
    "GFK, klar, 1,1 mm",
    "Stegplatte 10/4, klar",
    "Stegplatte 10/4, opal",
    "Stegplatte 16/7, klar",
    "Stegplatte 16/7, opal",
    "Stegplatte 20/7, klar",
    "Stegplatte 20/7, opal",
    tub_names_en$transmission[16:27]
  )
)
# The glazing examples map to DIN/TS 67600:2022-08, Table 6, rows 101-112.
# Preserve the original TUB construction codes alongside the common names.
tub_glazing_codes <- tub_names_en$transmission[16:27]
tub_glazing_type_de <- c(
  "Einfachverglasung",
  rep("Doppel-ISV", 2),
  rep("2-Scheiben-WMSV", 3),
  rep("2-Scheiben-SSV", 3),
  "3-Scheiben-WMSV",
  rep("3-Scheiben-SSV", 2)
)
tub_glazing_type_en <- c(
  "Single glazing",
  rep("Double insulating glazing", 2),
  rep("Double low-e glazing", 3),
  rep("Double solar-control glazing", 3),
  "Triple low-e glazing",
  rep("Triple solar-control glazing", 2)
)
tub_names_en$transmission[16:27] <- paste0(
  tub_glazing_type_en,
  " (",
  tub_glazing_codes,
  ")"
)
tub_names_de$transmission[16:27] <- paste0(
  tub_glazing_type_de,
  " (",
  tub_glazing_codes,
  ")"
)

tub_prepared <- purrr::map(names(tub_ids), function(mode) {
  file <- if (mode == "reflection") "Spectral_reflectance.csv" else
    "Spectral_transmittance.csv"
  path <- file.path("inst/extdata/tub", file)
  raw <- utils::read.table(
    path,
    header = TRUE,
    sep = ";",
    check.names = FALSE,
    fileEncoding = "UTF-8-BOM"
  )
  empty <- names(raw) == "" & vapply(raw, function(x) all(is.na(x)), logical(1))
  original_headers <- names(raw)[!empty]
  raw <- raw[, !empty, drop = FALSE]
  names(raw) <- original_headers
  ids <- tub_ids[[mode]]
  stopifnot(
    ncol(raw) == length(ids) + 1L,
    identical(raw[[1]], seq(380L, 780L, 5L))
  )
  stopifnot(
    all(is.finite(as.matrix(raw[-1]))),
    all(as.matrix(raw[-1]) >= 0),
    all(as.matrix(raw[-1]) <= 1)
  )
  category <- if (mode == "reflection")
    c(rep("wood", 10), rep("stone", 5), rep("wall", 12), "lacquer") else
    c(rep("plastic", 9), rep("sheet", 6), rep("glazing", 12))
  categories_en <- c(
    wood = "Wood",
    stone = "Stone",
    wall = "Wall colours",
    lacquer = "Lacquer",
    plastic = "Plastics",
    sheet = "Multi-skin sheets",
    glazing = "Glazing"
  )
  categories_de <- c(
    wood = "Holz",
    stone = "Stein",
    wall = "Wandfarben",
    lacquer = "Lack",
    plastic = "Kunststoffe",
    sheet = "Stegplatten",
    glazing = "Verglasung"
  )
  geometry_en <- if (mode == "reflection")
    "Relative spectral reflectance scaled to integrating-sphere luminous reflectance; illuminant A, 8 degree incidence. No angular scattering distribution supplied." else
    "Normal incidence. Transparent samples: direct transmission against air. Diffuse samples: relative spectrum scaled to total luminous transmission in an integrating sphere under illuminant A."
  geometry_de <- if (mode == "reflection")
    "Relatives Reflexionsspektrum, skaliert auf den Lichtreflexionsgrad in der Ulbricht-Kugel; Lichtart A, 8 Grad Einfall. Keine Winkelverteilung angegeben." else
    "Senkrechter Einfall. Transparente Proben: direkte Transmission gegen Luft. Diffuse Proben: relatives Spektrum, skaliert auf den Gesamtlichttransmissionsgrad in der Ulbricht-Kugel unter Lichtart A."
  records <- tibble::tibble(
    catalogue_id = paste("tub", mode, ids, sep = ":"),
    catalogue = "tub67600",
    catalogue_label_en = "TUB / DIN/TS 67600",
    catalogue_label_de = "TUB / DIN/TS 67600",
    material_mode = mode,
    scattering = if (mode == "reflection") "unknown" else
      ifelse(grepl("opal", tub_names_en[[mode]], fixed = TRUE), "yes", "no"),
    display_name = paste(ids, tub_names_en[[mode]], sep = " · "),
    display_name_en = display_name,
    display_name_de = paste(ids, tub_names_de[[mode]], sep = " · "),
    manufacturer = NA_character_,
    product_name = tub_names_en[[mode]],
    category_id = category,
    category_en = unname(categories_en[category]),
    category_de = unname(categories_de[category]),
    featured = TRUE,
    feature_reason_en = "TUB / 67600 example",
    feature_reason_de = "TUB / 67600 Beispiel",
    transmittance_type = "total",
    scale = "fraction",
    source_reference = "Rudawski, Aydınlı & Broszio (2022), version 2, DOI 10.14279/depositonce-11893.2",
    source_description = paste(
      "Spectral",
      mode,
      "coefficient of",
      tub_names_en[[mode]]
    ),
    source_description_en = source_description,
    source_description_de = paste(
      "Spektraler Materialkoeffizient:",
      tub_names_de[[mode]]
    ),
    measurement_geometry = geometry_en,
    measurement_geometry_en = geometry_en,
    measurement_geometry_de = geometry_de,
    # Measurement methods, p. 1 of the version-2 dataset description:
    # https://api-depositonce.tu-berlin.de/server/api/core/bitstreams/891785be-774c-4de7-a168-22af70e19302/content
    measurement_instrument = "Bruins Instruments OMEGA 20; calibration year not documented",
    measurement_instrument_en = "Bruins Instruments OMEGA 20; calibration year not documented",
    measurement_instrument_de = "Bruins Instruments OMEGA 20; Kalibrierjahr nicht dokumentiert",
    relative_measurement_error = NA_character_,
    relative_measurement_error_en = NA_character_,
    relative_measurement_error_de = NA_character_,
    thickness_mm = NA_real_,
    source_file = file,
    source_record = ids,
    source_column = seq_along(ids) + 1L,
    source_column_header = names(raw)[-1L],
    source_url = "https://doi.org/10.14279/depositonce-11893.2",
    source_version = "2 (2022-04-07)",
    source_commit = NA_character_,
    licence = "CC BY 4.0",
    citation = source_reference,
    transformation = "Original coefficients retained. Linear interpolation to 1 nm occurs at application. Reflection wood identities follow PDF Table 2 by source column position; wall colours use WC identifiers to avoid the PDF's WF alias ambiguity.",
    source_tail_treatment = NA_character_,
    wavelength_min_nm = 380,
    wavelength_max_nm = 780,
    source_points = 81L,
    source_luminous_transmittance_percent = NA_real_,
    source_melanopic_transmittance_percent = NA_real_
  )
  curves <- purrr::map(
    seq_along(ids),
    function(index)
      tibble::tibble(
        catalogue_id = records$catalogue_id[[index]],
        wavelength_nm = raw[[1]],
        transmittance = raw[[index + 1L]],
        source_status = "TUB version 2 source coefficient"
      )
  ) |>
    purrr::list_rbind()
  if (mode == "transmission") {
    glazing <- which(category == "glazing")
    records$source_description_en[glazing] <- paste0(
      tub_glazing_type_en,
      "; TUB construction: ",
      tub_glazing_codes,
      ". DIN/TS 67600:2022-08, Table 6, example ",
      101:112,
      "."
    )
    records$source_description_de[glazing] <- paste0(
      tub_glazing_type_de,
      "; TUB-Aufbau: ",
      tub_glazing_codes,
      ". DIN/TS 67600:2022-08, Tabelle 6, Beispiel ",
      101:112,
      ". ISV: Isolierverglasung; WMSV: Wärmeschutzverglasung; SSV: Sonnenschutzverglasung."
    )
    records$source_description[glazing] <- records$source_description_en[
      glazing
    ]
  } else {
    # DIN surface numbers identify the same spectral columns. Keep the TUB
    # names visible where the German DIN wording differs.
    din_surface <- c(
      418,
      423,
      419,
      424,
      420,
      425,
      421,
      426,
      422,
      427,
      416,
      417,
      413,
      414,
      415,
      403:412,
      401,
      402,
      NA
    )
    mapped <- which(!is.na(din_surface))
    records$source_description_de[mapped] <- paste0(
      records$source_description_de[mapped],
      ". DIN/TS 67600:2022-08, ",
      "Tabellen 9-11, Oberfläche ",
      din_surface[mapped],
      "."
    )
    records$source_description_en[mapped] <- paste0(
      records$source_description_en[mapped],
      ". DIN/TS 67600:2022-08, ",
      "Tables 9-11, surface ",
      din_surface[mapped],
      "."
    )
    records$source_description_de[11] <- paste0(
      records$source_description_de[11],
      " DIN-Bezeichnung: Backstein gelb; TUB-Bezeichnung: Brick, beige."
    )
    records$source_description_en[11] <- paste0(
      records$source_description_en[11],
      " DIN name: Backstein gelb (yellow brick); TUB name: Brick, beige."
    )
    records$source_description_de[13:15] <- paste0(
      records$source_description_de[13:15],
      " TUB: ",
      c(
        "Concrete, fine-pored (feinporig)",
        "Concrete, medium-pored (mittelporig)",
        "Concrete, coarse-pored (grobporig)"
      ),
      "."
    )
    records$source_description <- records$source_description_en
  }
  list(records = records, curves = curves)
})
tub_material_records <- purrr::map(tub_prepared, "records") |>
  purrr::list_rbind()
tub_material_curves <- purrr::map(tub_prepared, "curves") |> purrr::list_rbind()
tub_material_provenance <- list(
  catalogue = "tub67600",
  source_url = "https://doi.org/10.14279/depositonce-11893.2",
  source_version = "2 (2022-04-07)",
  measurement_metadata_source = "Dataset description, Measurement method, p. 1: https://api-depositonce.tu-berlin.de/server/api/core/bitstreams/891785be-774c-4de7-a168-22af70e19302/content",
  glazing_descriptions = "Common glazing types follow DIN/TS 67600:2022-08, Table 6, rows 101-112; original TUB construction codes and all coefficients are retained.",
  german_surface_names = "German surface names follow DIN/TS 67600:2022-08, Tables 9-11. S1 shows both DIN yellow and TUB beige brick. S3-S5 use the concise DIN concrete names; the TUB pore descriptors remain in source details. Spectral identities and coefficients are unchanged.",
  licence = "CC BY 4.0",
  citation = "Rudawski, Frederic; Aydınlı, Sırrı; Broszio, Kai. Spectral reflectance and transmittance of building materials. TU Berlin, version 2.",
  raw_md5 = as.list(tools::md5sum(file.path(
    "inst/extdata/tub",
    c("Spectral_reflectance.csv", "Spectral_transmittance.csv")
  ))),
  corrections = "Wood WF1-WF10 mapped by column position against PDF Table 2, whose spectra exactly match the CSV. Raw CSV repeats HB4/WF4 and shifts subsequent wood labels. Wall WF aliases in PDF Table 3 are represented as WC1-WC12. No coefficient values changed."
)
