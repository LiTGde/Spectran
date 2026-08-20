# Localized catalogue narrative used to prepare bundled records.
#
# Stable scientific and bibliographic fields remain language-independent.
# Narrative fields are materialized as `_en` and `_de` columns so another
# locale can be added here without changing the Shiny presentation code.

transmission_catalogue_localized_copy <- tibble::tribble(
  ~catalogue, ~field, ~English, ~Deutsch,
  "facade_windows",
  "measurement_geometry",
  "Numerical data from CIE.",
  "Numerische Daten der CIE.",
  "facade_windows",
  "licence",
  paste(
    "GPL-2-or-later package distribution; underlying CIE source",
    "attribution retained"
  ),
  paste(
    "Paketverteilung unter GPL-2 oder später; die Zuordnung der",
    "zugrunde liegenden CIE-Quelle bleibt erhalten"
  ),
  "facade_windows",
  "transformation",
  paste(
    "Spectral values copied without numerical transformation from",
    "photobiologyFilters 0.6.1; thickness metadata converted from metres",
    "to millimetres. Values outside 380-780 nm are retained for audit",
    "and boundary bracketing."
  ),
  paste(
    "Spektralwerte ohne numerische Veränderung aus photobiologyFilters",
    "0.6.1 übernommen; Dickenmetadaten von Metern in Millimeter",
    "umgerechnet. Werte außerhalb 380–780 nm bleiben für Audit und",
    "Randbereichsbildung erhalten."
  ),
  "spitschan2019",
  "licence",
  paste(
    "CC BY 4.0 according to the Oxford University Research Archive record;",
    "the GitHub repository contains no separate licence file"
  ),
  paste(
    "CC BY 4.0 gemäß dem Datensatz im Oxford University Research Archive;",
    "das GitHub-Repository enthält keine separate Lizenzdatei"
  ),
  "spitschan2019",
  "transformation",
  paste(
    "The source authors averaged duplicate digitized wavelengths, applied",
    "PCHIP interpolation on 380-780 nm, divided percent values by 100, and",
    "left samples outside each digitized range as NaN. Spectran bundles the",
    "finite values unchanged and exposes missing tails for explicit review."
  ),
  paste(
    "Die Autoren der Quelle mittelten doppelte digitalisierte Wellenlängen,",
    "wendeten eine PCHIP-Interpolation auf 380–780 nm an, teilten",
    "Prozentwerte durch 100 und beließen Werte außerhalb des jeweiligen",
    "digitalisierten Bereichs als NaN. Spectran übernimmt die endlichen",
    "Werte unverändert und legt fehlende Randbereiche zur ausdrücklichen",
    "Prüfung vor."
  ),
  "spitschan2019",
  "source_tail_treatment",
  paste(
    "The source paper's analysis replaced missing tail values with 0.",
    "Spectran does not apply that choice automatically."
  ),
  paste(
    "In der Analyse der Quellpublikation wurden fehlende Randwerte durch",
    "0 ersetzt. Spectran übernimmt diese Festlegung nicht automatisch."
  )
)

add_transmission_catalogue_localization <- function(records) {
  localized_fields <- c(
    "source_description",
    "measurement_geometry",
    "licence",
    "transformation",
    "source_tail_treatment"
  )

  for (field in localized_fields) {
    records[[paste0(field, "_en")]] <- records[[field]]
    records[[paste0(field, "_de")]] <- records[[field]]
  }

  facade <- records$catalogue == "facade_windows"
  records$source_description_de[facade] <- gsub(
    "; from ",
    "; Quelle: ",
    gsub(
      "; thickness ",
      "; Dicke ",
      records$source_description[facade],
      fixed = TRUE
    ),
    fixed = TRUE
  )

  spitschan <- records$catalogue == "spitschan2019"
  records$source_description_de[spitschan] <- paste0(
    "Digitalisierte Kurve eines Kurzwellenspektralfilters; Quellkategorie: ",
    records$category_de[spitschan]
  )

  for (row in seq_len(nrow(transmission_catalogue_localized_copy))) {
    catalogue <- transmission_catalogue_localized_copy$catalogue[[row]]
    field <- transmission_catalogue_localized_copy$field[[row]]
    selected <- records$catalogue == catalogue
    records[[paste0(field, "_en")]][selected] <-
      transmission_catalogue_localized_copy$English[[row]]
    records[[paste0(field, "_de")]][selected] <-
      transmission_catalogue_localized_copy$Deutsch[[row]]
  }

  records
}
