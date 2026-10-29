# Prepare the article's DIN coefficients from a local, licensed text extraction.

#####
# Step 1: Read the source
#####

library(dplyr)
library(tidyr)
library(purrr)

arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 2L) {
  stop("Supply a pdftotext -layout DIN extraction and an output directory.")
}
din_lines <- readLines(arguments[[1]], encoding = "UTF-8", warn = FALSE)
stopifnot(any(grepl("DIN/TS 67600:2022-08", din_lines, fixed = TRUE)))
output_dir <- arguments[[2]]
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

#####
# Step 2: Record material designations
#####

glazing_de <- c(
  "Einfachverglasung", rep("Doppel-ISV", 2), rep("2-Scheiben-WMSV", 3),
  rep("2-Scheiben-SSV", 3), "3-Scheiben-WMSV", rep("3-Scheiben-SSV", 2)
)
glazing_en <- c(
  "Single glazing", rep("Double insulating glazing", 2),
  rep("Double low-e glazing", 3), rep("Double solar-control glazing", 3),
  "Triple low-e glazing", rep("Triple solar-control glazing", 2)
)
sheet_type <- rep(1:4, c(3, 3, 3, 2))
sheet_thickness <- rep(c(16, 16, 25, 20), c(3, 3, 3, 2))
sheet_finish_de <- c(rep(c("durchsichtig", "streuend", "durchsichtig, eingefärbt"), 3),
  "durchsichtig", "streuend")
sheet_finish_en <- c(rep(c("clear", "diffusing", "clear, tinted"), 3),
  "clear", "diffusing")
grades <- c(602, 625, 627, 400, 303, 837, 838, 882, 700, 701, 777, 478, 501, 502, 802, 807)
colours_de <- c(rep("blau", 3), "braun", "gelb", rep("grau", 3), rep("grün", 3),
  "orange", rep("rot", 2), rep("umbra", 2))
colours_en <- c(rep("blue", 3), "brown", "yellow", rep("grey", 3), rep("green", 3),
  "orange", rep("red", 2), rep("umbra", 2))
paint_names <- c("Lachs 20", "Lachs 30", "Palazzo 235", "Palazzo 240", "Palazzo 90",
  "Marill 30", "Marill 120", "Siena 150", "Siena 120", "Siena 115", "Ceramic 90", "Ceramic 60")
wood_de <- c("Ahorn", "Buche", "Eiche", "Kirschbaum", "Nussbaum")
wood_en <- c("Maple", "Beech", "Oak", "Cherry", "Walnut")

materials <- tibble(
  din_id = c(101:112, 201:211, 301:321, 401:427),
  din_name_de = c(
    glazing_de,
    sprintf("Typ %d, %d mm, %s", sheet_type, sheet_thickness, sheet_finish_de),
    paste("Plexiglas®", c("GS233", "XT", "7H", "8N")),
    "Sonnenschutzglas RT95 (Schutzbrillen bei Makuladegeneration)",
    sprintf("Plexiglas® GS %s %d, 3 mm", colours_de, grades),
    paint_names, "Beton fein", "Beton mittel", "Beton grob", "Backstein gelb", "Backstein rot",
    paste(wood_de, "lackiert"), paste(wood_de, "geölt")
  ),
  din_name_en = c(
    glazing_en,
    sprintf("Type %d, %d mm, %s", sheet_type, sheet_thickness, sheet_finish_en),
    paste("Plexiglas", c("GS233", "XT", "7H", "8N")), "RT95 sun-protection filter",
    sprintf("Plexiglas GS %s %d, 3 mm", colours_en, grades),
    paint_names, "Concrete, fine-pored", "Concrete, medium-pored", "Concrete, coarse-pored",
    "Yellow brick", "Red brick", paste0(wood_en, ", varnished"), paste0(wood_en, ", oiled")
  ),
  group = rep(c("Glazing", "Multi-skin sheets", "Plastics and filters", "Surfaces"),
    c(12, 11, 21, 27)),
  mode = rep(c("Transmission", "Reflection"), c(44, 27))
)

#####
# Step 3: Extract the two weighted coefficients
#####

read_din_table <- function(number, ids, values_per_row) {
  start <- grep(paste0("Tabelle ", number, " —"), din_lines, fixed = TRUE)
  stopifnot(length(start) >= 1L)
  block <- din_lines[seq.int(tail(start, 1L), length(din_lines))]
  pattern <- paste0("^\\s*(", paste(ids, collapse = "|"), ")\\s")
  rows <- head(block[grepl(pattern, block)], length(ids))
  row_ids <- as.integer(sub("^\\s*([0-9]+).*$", "\\1", rows))
  stopifnot(identical(row_ids, as.integer(ids)))

  map(rows, function(line) {
    fields <- strsplit(trimws(line), "\\s+")[[1]]
    decimals <- fields[grepl("^[0-9]+,[0-9]{3}$", fields)]
    stopifnot(length(decimals) == values_per_row)
    as.numeric(sub(",", ".", decimals, fixed = TRUE))
  }) |>
    do.call(what = rbind)
}

reference <- map(6:10, function(table_number) {
  ids <- switch(as.character(table_number),
    `6` = 101:112, `7` = 201:211, `8` = 301:321, `9` = 401:427, `10` = 401:427)
  illuminants <- switch(as.character(table_number),
    `6` = "D65", `7` = "D65", `8` = "D65",
    `9` = c("EE", "A", "D65", "FL11"),
    `10` = c("LED-B1", "LED-B2", "LED-B3", "LED-B5"))
  values <- read_din_table(table_number, ids, if (table_number <= 8) 4L else 8L)
  coefficient_columns <- if (table_number <= 8) 1:2 else 1:8

  expand_grid(din_id = ids, illuminant = illuminants, response = c("vis", "mel")) |>
    mutate(
      value = as.vector(t(values[, coefficient_columns, drop = FALSE])),
      din_table_ref = table_number,
      page = case_when(
        table_number == 6L ~ 20L,
        table_number == 7L ~ 22L,
        table_number == 8L & din_id <= 313L ~ 22L,
        table_number == 8L ~ 23L,
        table_number == 9L & din_id <= 425L ~ 25L,
        table_number == 9L ~ 26L,
        table_number == 10L & din_id <= 425L ~ 26L,
        TRUE ~ 27L
      )
    )
}) |>
  list_rbind() |>
  pivot_wider(names_from = response, values_from = value, names_prefix = "din_") |>
  left_join(materials, by = "din_id", relationship = "many-to-one") |>
  select(din_id, din_name_de, din_name_en, group, mode, illuminant,
    din_vis, din_mel, din_table_ref, page)

stopifnot(
  nrow(reference) == 260L, n_distinct(reference$din_id) == 71L,
  !anyDuplicated(reference[c("din_id", "illuminant")]), !anyNA(reference),
  all(between(reference$din_vis, 0, 1)), all(between(reference$din_mel, 0, 1))
)

#####
# Step 4: Save the reference and the separate TUB assignments
#####

surface_ids <- c("WC11", "WC12", paste0("WC", 1:10), "S3", "S4", "S5", "S1", "S2",
  "WF1", "WF3", "WF5", "WF7", "WF9", "WF2", "WF4", "WF6", "WF8", "WF10")
assignments <- tibble(
  din_id = c(101:112, 401:427),
  catalogue_id = c(paste0("tub:transmission:G", 1:12), paste0("tub:reflection:", surface_ids))
)
write.csv(reference, file.path(output_dir, "din-67600-material-coefficients.csv"),
  row.names = FALSE, fileEncoding = "UTF-8")
write.csv(assignments, file.path(output_dir, "tub-assignments.csv"),
  row.names = FALSE, fileEncoding = "UTF-8")
provenance <- c(
  "Source: DIN/TS 67600:2022-08, Tables 6 to 10.",
  "Printed page numbers are recorded for each row.",
  "DIN labels are transcribed with line breaks removed; English labels are editorial translations.",
  "Only photopic and melanopic material coefficients are retained, not MDER or effective MDER.",
  paste("Source text MD5:", unname(tools::md5sum(arguments[[1]]))),
  paste("R version:", getRversion()),
  paste("dplyr:", packageVersion("dplyr")),
  paste("tidyr:", packageVersion("tidyr")),
  paste("purrr:", packageVersion("purrr")),
  "Preparation: Rscript --vanilla data-raw/prepare_din_material_coefficients.R DIN.txt OUTPUT"
)
writeLines(provenance, file.path(output_dir, "preparation-provenance.txt"))
print(count(reference, din_table_ref, name = "material_illuminant_pairs"))
