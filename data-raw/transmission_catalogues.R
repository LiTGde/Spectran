# Prepare the bundled transmission-filter catalogues ----------------------
#
# This script is the only networked part of catalogue preparation. The
# installed application uses the generated internal data and performs no
# catalogue network access. Set SPECTRAN_SPITSCHAN_ARCHIVE to a local copy of
# the pinned archive to rebuild without downloading it.

spitschan_commit <- "3f68efef2101a4eacc4a756e4c44df46dd90a7e5"
spitschan_archive_url <- paste0(
  "https://github.com/spitschan/Spitschan2019_OPO/archive/",
  spitschan_commit,
  ".tar.gz"
)
spitschan_archive_sha256 <-
  "5ab0d81db46cb17ddf179df36fd9104dd3b7ac33442036bb58968aa0445518f9"

photobiology_filters_version <- "0.6.1"
photobiology_filters_archive_url <- paste0(
  "https://cran.r-project.org/src/contrib/photobiologyFilters_",
  photobiology_filters_version,
  ".tar.gz"
)
photobiology_filters_archive_sha256 <-
  "c41ed38ae8b489b89260cef2c4d6655bf96f93e363c5d967243ed2100c8c756b"

sha256_file <- function(path) {
  digest::digest(file = path, algo = "sha256")
}

download_verified_archive <- function(url, expected_sha256, local_path = "") {
  if (nzchar(local_path)) {
    archive <- normalizePath(local_path, mustWork = TRUE)
  } else {
    archive <- tempfile(fileext = ".tar.gz")
    utils::download.file(url, archive, mode = "wb", quiet = TRUE)
  }
  observed_sha256 <- sha256_file(archive)
  if (!identical(observed_sha256, expected_sha256)) {
    stop(
      "Catalogue archive checksum mismatch. Expected ",
      expected_sha256,
      ", observed ",
      observed_sha256,
      ".",
      call. = FALSE
    )
  }
  archive
}

if (
  !requireNamespace("photobiologyFilters", quietly = TRUE) ||
    as.character(utils::packageVersion("photobiologyFilters")) !=
      photobiology_filters_version
) {
  stop(
    "Catalogue preparation requires photobiologyFilters ",
    photobiology_filters_version,
    ".",
    call. = FALSE
  )
}

# Façade and window glazing catalogue -------------------------------------

suppressPackageStartupMessages(library(photobiologyFilters))

facade_ids <- photobiologyFilters::glass_windows
if (length(facade_ids) != 26L) {
  stop("Expected 26 façade/window spectra.", call. = FALSE)
}

facade_spectra <- photobiologyFilters::filters.mspct[facade_ids]

facade_curves <- purrr::map2_dfr(
  facade_spectra,
  facade_ids,
  function(spectrum, source_id) {
    tibble::tibble(
      catalogue_id = paste0("facade:", source_id),
      wavelength_nm = as.numeric(spectrum$w.length),
      transmittance = as.numeric(spectrum$Tfr),
      source_status = "photobiologyFilters source value"
    )
  }
)

facade_records <- purrr::map2_dfr(
  facade_spectra,
  facade_ids,
  function(spectrum, source_id) {
    filter_properties <- attr(spectrum, "filter.properties")
    tibble::tibble(
      catalogue_id = paste0("facade:", source_id),
      catalogue = "facade_windows",
      catalogue_label_en = "Façade and window glazing",
      catalogue_label_de = "Fassaden- und Fensterglas",
      display_name = source_id,
      manufacturer = NA_character_,
      product_name = source_id,
      category_id = "facade_glazing",
      category_en = "Façade glazing",
      category_de = "Fassadenverglasung",
      featured = TRUE,
      feature_reason_en = "Façade/window example",
      feature_reason_de = "Beispiel für Fassaden/Fenster",
      transmittance_type = attr(spectrum, "Tfr.type"),
      scale = "fraction",
      source_reference = attr(spectrum, "comment"),
      source_description = attr(spectrum, "what.measured"),
      measurement_geometry = attr(spectrum, "how.measured"),
      thickness_mm = as.numeric(filter_properties$thickness) * 1000,
      source_file = "photobiologyFilters::filters.mspct",
      source_record = source_id,
      source_url = "https://www.docs.r4photobiology.info/photobiologyFilters/reference/glass_windows.html",
      source_version = photobiology_filters_version,
      source_commit = NA_character_,
      licence = paste(
        "GPL-2-or-later package distribution; underlying CIE source",
        "attribution retained"
      ),
      citation = paste(
        "Aphalo, P. J. (2015). The r4photobiology suite.",
        "UV4Plants Bulletin, 2015(1), 21-29.",
        "https://doi.org/10.19232/uv4pb.2015.1.14"
      ),
      transformation = paste(
        "Spectral values copied without numerical transformation from",
        "photobiologyFilters 0.6.1; thickness metadata converted from metres",
        "to millimetres. Values outside 380-780 nm are retained for audit",
        "and boundary bracketing."
      ),
      source_tail_treatment = NA_character_,
      wavelength_min_nm = min(spectrum$w.length),
      wavelength_max_nm = max(spectrum$w.length),
      source_points = nrow(spectrum),
      source_luminous_transmittance_percent = NA_real_,
      source_melanopic_transmittance_percent = NA_real_
    )
  }
)

# Spitschan et al. 2019 catalogue ----------------------------------------

spitschan_archive <- download_verified_archive(
  url = spitschan_archive_url,
  expected_sha256 = spitschan_archive_sha256,
  local_path = Sys.getenv("SPECTRAN_SPITSCHAN_ARCHIVE", unset = "")
)
spitschan_extract_dir <- tempfile("spectran-spitschan-")
dir.create(spitschan_extract_dir)
utils::untar(spitschan_archive, exdir = spitschan_extract_dir)
spitschan_root <- list.dirs(
  spitschan_extract_dir,
  full.names = TRUE,
  recursive = FALSE
)[[1L]]

spitschan_files <- file.path(
  spitschan_root,
  c(
    "trans_bbFilters.mat",
    "Table1.csv",
    "prepare_filters.m",
    "GetMetaDataFilters.m"
  )
)
spitschan_file_checksums <- vapply(
  spitschan_files,
  sha256_file,
  character(1)
)
expected_spitschan_file_checksums <- c(
  trans_bbFilters.mat = "7d174a0841e445c633f32bcdb390f828a5af3bfeb7ea7008ba1668edaa77bfcf",
  Table1.csv = "3784991e8382ffd72e427bf753baf4e54a0150ddb3e5658c252093a0be18a68c",
  prepare_filters.m = "4be8bfcd35d025dde24b4df048b51c65d67e7ce9c774aecfe6e05ce3966dd965",
  GetMetaDataFilters.m = "9f0a51ceb70abf925cd4fa133e921a1d6bee89da5b73ef0d0a464ed145f76e91"
)
names(spitschan_file_checksums) <- basename(spitschan_files)
if (!identical(spitschan_file_checksums, expected_spitschan_file_checksums)) {
  stop("Pinned Spitschan source-file checksum mismatch.", call. = FALSE)
}

spitschan_mat <- R.matlab::readMat(
  file.path(spitschan_root, "trans_bbFilters.mat")
)
spitschan_matrix <- spitschan_mat$trans.bbFilters
spitschan_wavelengths <- as.numeric(spitschan_mat$wls.bbFilters)
spitschan_type <- as.integer(spitschan_mat$type.bbFilters)
spitschan_source_files <- vapply(
  spitschan_mat$src.bbFilters,
  function(value) as.character(unlist(value, use.names = FALSE)[[1L]]),
  character(1)
)
spitschan_type_labels <- vapply(
  spitschan_mat$type.label,
  function(value) as.character(unlist(value, use.names = FALSE)[[1L]]),
  character(1)
)

if (
  !identical(dim(spitschan_matrix), c(401L, 121L)) ||
    !identical(spitschan_wavelengths, as.numeric(380:780)) ||
    length(spitschan_source_files) != 121L
) {
  stop("Unexpected Spitschan catalogue structure.", call. = FALSE)
}

spitschan_table <- readr::read_csv(
  file.path(spitschan_root, "Table1.csv"),
  col_names = c(
    "manufacturer",
    "product_name",
    "reference",
    "luminous_percent",
    "melanopic_percent",
    "colour_shift",
    "gamut_percent",
    "filter_type"
  ),
  locale = readr::locale(encoding = "ISO-8859-1"),
  show_col_types = FALSE
)

# analyseFilters.m writes type 1, type 2, and then types 3-6. Restore the
# original matrix order before joining metadata to curves.
spitschan_table$matrix_index <- c(
  which(spitschan_type == 1L),
  which(spitschan_type == 2L),
  which(spitschan_type >= 3L)
)
spitschan_table <- dplyr::arrange(spitschan_table, .data$matrix_index)
if (!identical(as.integer(spitschan_table$filter_type), spitschan_type)) {
  stop(
    "Spitschan metadata could not be aligned to the curve matrix.",
    call. = FALSE
  )
}

spitschan_category_en <- c(
  "Medical lenses",
  "Safety lenses",
  "Task-specific lenses: sports",
  "Task-specific lenses: driving",
  "Task-specific lenses: screen use",
  "Task-specific lenses: other"
)
spitschan_category_de <- c(
  "Medizinische Filter",
  "Schutzfilter",
  "Aufgabenspezifisch: Sport",
  "Aufgabenspezifisch: Fahren",
  "Aufgabenspezifisch: Bildschirmarbeit",
  "Aufgabenspezifisch: sonstige"
)

spitschan_feature_pattern <- paste(
  c(
    "NoIR",
    "FL-41",
    "migraine",
    "sleep",
    "contact",
    "aphakic",
    "albino",
    "cataract",
    "Chron-Optic",
    "SCT Orange",
    "StressFree",
    "BlueProtect",
    "BlueControl",
    "Screen",
    "Noflex"
  ),
  collapse = "|"
)
spitschan_featured <- grepl(
  spitschan_feature_pattern,
  paste(
    spitschan_table$manufacturer,
    spitschan_table$product_name,
    spitschan_source_files
  ),
  ignore.case = TRUE
)

spitschan_curves <- purrr::map_dfr(
  seq_len(ncol(spitschan_matrix)),
  function(i) {
    keep <- is.finite(spitschan_matrix[, i])
    tibble::tibble(
      catalogue_id = sprintf("spitschan2019:%03d", i),
      wavelength_nm = spitschan_wavelengths[keep],
      transmittance = as.numeric(spitschan_matrix[keep, i]),
      source_status = "Source PCHIP interpolation"
    )
  }
)

spitschan_bounds <- purrr::map_dfr(
  seq_len(ncol(spitschan_matrix)),
  function(i) {
    keep <- is.finite(spitschan_matrix[, i])
    tibble::tibble(
      wavelength_min_nm = min(spitschan_wavelengths[keep]),
      wavelength_max_nm = max(spitschan_wavelengths[keep]),
      source_points = sum(keep)
    )
  }
)

spitschan_records <- tibble::tibble(
  catalogue_id = sprintf("spitschan2019:%03d", seq_len(121L)),
  catalogue = "spitschan2019",
  catalogue_label_en = "Spitschan et al. 2019 blue-blocking filters",
  catalogue_label_de = "Blaulichtfilter nach Spitschan et al. 2019",
  display_name = paste(
    spitschan_table$manufacturer,
    spitschan_table$product_name,
    sep = " - "
  ),
  manufacturer = spitschan_table$manufacturer,
  product_name = spitschan_table$product_name,
  category_id = paste0("spitschan_", spitschan_type),
  category_en = spitschan_category_en[spitschan_type],
  category_de = spitschan_category_de[spitschan_type],
  featured = spitschan_featured,
  feature_reason_en = ifelse(
    spitschan_featured,
    "Featured clinical or blue-blocking example",
    ""
  ),
  feature_reason_de = ifelse(
    spitschan_featured,
    "Hervorgehobenes klinisches oder Blaulichtfilter-Beispiel",
    ""
  ),
  transmittance_type = "unknown",
  scale = "fraction",
  source_reference = spitschan_table$reference,
  source_description = paste(
    "Digitized short-wavelength-filter curve; source category:",
    spitschan_type_labels[spitschan_type]
  ),
  measurement_geometry = NA_character_,
  thickness_mm = NA_real_,
  source_file = spitschan_source_files,
  source_record = sprintf("matrix column %d", seq_len(121L)),
  source_url = "https://github.com/spitschan/Spitschan2019_OPO",
  source_version = "Pinned Git commit",
  source_commit = spitschan_commit,
  licence = paste(
    "CC BY 4.0 according to the Oxford University Research Archive record;",
    "the GitHub repository contains no separate licence file"
  ),
  citation = paste(
    "Spitschan, M., Lazar, R., & Cajochen, C. (2019).",
    "Visual and non-visual properties of filters manipulating",
    "short-wavelength light. Ophthalmic and Physiological Optics,",
    "39(6), 459-468. https://doi.org/10.1111/opo.12648"
  ),
  transformation = paste(
    "The source authors averaged duplicate digitized wavelengths, applied",
    "PCHIP interpolation on 380-780 nm, divided percent values by 100, and",
    "left samples outside each digitized range as NaN. Spectran bundles the",
    "finite values unchanged and exposes missing tails for explicit review."
  ),
  source_tail_treatment = paste(
    "The source paper's analysis replaced missing tail values with 0.",
    "Spectran does not apply that choice automatically."
  ),
  wavelength_min_nm = spitschan_bounds$wavelength_min_nm,
  wavelength_max_nm = spitschan_bounds$wavelength_max_nm,
  source_points = spitschan_bounds$source_points,
  source_luminous_transmittance_percent = spitschan_table$luminous_percent,
  source_melanopic_transmittance_percent = spitschan_table$melanopic_percent
)

# Installed-data contracts ------------------------------------------------

transmission_catalogue_records <- dplyr::bind_rows(
  facade_records,
  spitschan_records
)
transmission_catalogue_records <- add_transmission_catalogue_localization(
  transmission_catalogue_records
)
transmission_catalogue_curves <- dplyr::bind_rows(
  facade_curves,
  spitschan_curves
)

transmission_catalogue_provenance <- list(
  facade_windows = list(
    title = "Façade and window glazing spectra",
    source_url = "https://www.docs.r4photobiology.info/photobiologyFilters/reference/glass_windows.html",
    package_url = "https://cran.r-project.org/package=photobiologyFilters",
    package_version = photobiology_filters_version,
    package_archive_url = photobiology_filters_archive_url,
    package_archive_sha256 = photobiology_filters_archive_sha256,
    upstream_url = "https://files.cie.co.at/206.xls",
    licence = paste(
      "photobiologyFilters is GPL-2-or-later; original CIE attribution is",
      "retained per the package metadata."
    ),
    citation = facade_records$citation[[1L]],
    transformations = facade_records$transformation[[1L]],
    record_count = nrow(facade_records)
  ),
  spitschan2019 = list(
    title = "Spitschan et al. 2019 short-wavelength filter collection",
    source_url = "https://github.com/spitschan/Spitschan2019_OPO",
    archive_url = spitschan_archive_url,
    commit = spitschan_commit,
    archive_sha256 = spitschan_archive_sha256,
    source_file_sha256 = as.list(spitschan_file_checksums),
    article_url = "https://doi.org/10.1111/opo.12648",
    archive_record_url = paste0(
      "https://ora.ox.ac.uk/objects/",
      "uuid:c341be73-cf42-4b70-86c9-c49d6920ca67"
    ),
    licence = spitschan_records$licence[[1L]],
    licence_url = "https://creativecommons.org/licenses/by/4.0/",
    citation = spitschan_records$citation[[1L]],
    transformations = spitschan_records$transformation[[1L]],
    record_count = nrow(spitschan_records)
  )
)

stopifnot(
  nrow(facade_records) == 26L,
  nrow(spitschan_records) == 121L,
  nrow(transmission_catalogue_records) == 147L,
  all(is.finite(transmission_catalogue_curves$wavelength_nm)),
  all(is.finite(transmission_catalogue_curves$transmittance)),
  all(
    transmission_catalogue_curves$transmittance >= 0 &
      transmission_catalogue_curves$transmittance <= 1
  ),
  all(
    transmission_catalogue_records$catalogue_id %in%
      transmission_catalogue_curves$catalogue_id
  )
)
