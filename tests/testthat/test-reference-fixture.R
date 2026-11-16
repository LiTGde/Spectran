test_that("readable reference values and checksums match the binary fixture", {
  fixture <- validation_references()
  audit <- utils::read.csv(test_path("fixtures", "reference-values.csv"),
    colClasses = "character", na.strings = "")
  expect_equal(nrow(audit), 706L)
  expect_setequal(unique(audit$source), c("CIE", "luox", "DIN"))
  references <- list(CIE = fixture$cie, luox = fixture$luox,
    DIN = fixture$din$expected)
  for (source in names(references)) {
    reference <- references[[source]]
    rows <- audit[audit$source == source, ]
    cases <- if (source == "DIN")
      paste(reference$din_id, reference$catalogue_id,
        reference$illuminant, sep = " | ") else reference$spectrum
    expect_identical(rows$case, cases)
    expect_identical(rows$metric, reference$metric)
    expect_identical(rows$unit, reference$unit)
    expect_identical(rows$reference_value, sprintf("%.17g", reference$expected))
    status <- rep("compare", nrow(reference))
    anchors <- rep(NA_character_, nrow(reference))
    if (source == "luox") status[!reference$applicable] <- "inapplicable_cri"
    if (source == "DIN") {
      status[reference$known_issue] <- "known_fl11_discrepancy"
      regression <- fixture$din$fl11_regression
      anchors[reference$known_issue] <- sprintf("%.17g",
        regression$regression_value[match(
          reference$din_id[reference$known_issue], regression$din_id)])
    }
    expect_identical(rows$status, status)
    expect_identical(rows$fl11_regression_anchor, anchors)
  }
  checksums <- utils::read.csv(test_path("fixtures", "reference-input-checksums.csv"),
    colClasses = "character")
  expect_identical(checksums, fixture$provenance$inputs)
})
