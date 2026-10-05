source("/private/tmp/spectran-revision/load.R")
args <- commandArgs(trailingOnly = TRUE)
version <- if (length(args)) args[[1L]] else "M4-02"
base <- file.path("/private/tmp/spectran-revision", paste0("check-", version))
tarball <- list.files(base, pattern = "[.]tar[.]gz$", full.names = TRUE)
stopifnot(length(tarball) == 1L)
installed <- file.path(base, "Spectran.Rcheck", "Spectran")
bytes <- function(folder) sum(file.info(list.files(folder, recursive = TRUE, full.names = TRUE, all.files = TRUE))$size, na.rm = TRUE)
rows <- data.frame(component = c("Source tarball", "Installed package", "Data", "Web assets (all)", "Explanation SVGs", "R help databases"),
  bytes = c(file.info(tarball)$size, bytes(installed), bytes(file.path(installed, "data")),
    bytes(file.path(installed, "app", "www")), bytes(file.path(installed, "app", "www", "explanations")), bytes(file.path(installed, "help"))))
rows$MB_decimal <- rows$bytes / 1e6
stopifnot(rows$bytes[rows$component == "Source tarball"] < 10e6,
  rows$bytes[rows$component == "Data"] < 5e6,
  sum(rows$bytes[rows$component %in% c("Web assets (all)", "R help databases")]) < 5e6)
print(rows, row.names = FALSE)
write.csv(rows, file.path("/private/tmp/spectran-revision", paste0(version, "-sizes.csv")), row.names = FALSE)
contents <- utils::untar(tarball, list = TRUE)
stopifnot(!any(grepl("data-raw|/Rplots[.]pdf$", contents)))
cat("Data and combined web assets plus R help remain below 5 MB; source tarball below 10 MB.\n")
cat("Source tarball MD5:", unname(tools::md5sum(tarball)), "\n")
cat("R version:", R.version.string, "\n")
