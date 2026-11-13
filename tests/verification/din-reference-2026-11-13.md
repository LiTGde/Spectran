# DIN reference update, 2026-11-13

DIN Media identifies the current edition as
[DIN 5031-100:2025-09](https://www.dinmedia.de/en/standard/din-5031-100/392132532),
DOI `10.31030/3622790`, replacing the 2021 technical specification.
This is a citation update following the maintainer's confirmation that the
new edition requires no changes to Spectran's names or calculations.

Updated the age-view introduction, validity section, age-table source note,
age explanation, German and English illustration footers, package description,
dataset documentation and their source generators. Historical release notes
about removing DIN/SPEC weighting remain historical records.

Verification used R 4.6.1 and the existing project library, with roxygen2 8.1.0,
testthat 3.3.2, shiny 1.14.0, svglite 2.2.2 and systemfonts 1.3.2:

- `devtools::document()` regenerated the two affected Rd files.
- The existing illustration generator ran in a temporary directory. Only the
  two age SVGs differed. Their numerical source CSV matched the previous CSV
  with zero tolerance, and the parsed age-model code was identical.
- R rendered the age UI, validity UI, explanation and table note in both
  languages. Each contains the new edition and link, with no old reference.
- Existing explanation and standard-plot-style tests passed without warnings.
- Both SVG footers were visually checked in the browser and fit their layouts.
- R scanned bundled data objects and language workbooks for additional old
  citations; none were present. A source search found no old 2021 citation or
  obsolete link in the active app or documentation.
- Connect manifest file checksums were refreshed; dependency metadata and
  all bundled datasets are unchanged.

Commands: `Rscript --vanilla /private/tmp/spectran-din-2025-assets.R` and
`Rscript --vanilla /private/tmp/spectran-din-2025-verify.R`, with the existing
project library prepended to `.libPaths()`. The asset check's inputs, outputs
and R session information are in
`/private/tmp/spectran-din-2025-20261113-render/`; the corresponding logs are
`/private/tmp/spectran-din-2025-assets.log` and
`/private/tmp/spectran-din-2025-verify.log`. The refreshed illustration
provenance records input hashes and the packages used for rendering.
