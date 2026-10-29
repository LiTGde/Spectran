material_catalogue_records_data <- function(mode = "transmission") {
  mode <- material_mode(mode = mode)
  legacy <- transmission_catalogue_records_data()
  legacy$material_mode <- "transmission"
  records <- dplyr::bind_rows(tub_material_records, legacy)
  records[records$material_mode == mode, , drop = FALSE]
}
