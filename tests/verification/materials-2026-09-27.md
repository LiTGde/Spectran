# Material extension verification, 27 September 2026

The transmission prototype now supports reflection, all 55 TUB examples,
editable receiver illuminance, mixed interaction histories, and two cumulative
comparisons. The physical assumptions and reference limitations are documented
in `inst/material-model.md`.

## Computation and inputs

Scientific calculations and comparisons used R 4.6.1 with the existing project
library. Consequential versions include dplyr 1.2.1, tibble 3.3.1, purrr 1.2.2,
testthat 3.3.2, shiny 1.14.0, ggplot2 4.0.3, and gt 1.3.0. No package installation
or full legacy data rebuild was needed. All eight pre-existing objects in
`R/sysdata.rda` compare identically with the original file; only the three TUB
objects were added.

Reference inputs:

- Unmodified TUB version 2 CSVs in `inst/extdata/tub`, DOI
  10.14279/depositonce-11893.2, CC BY 4.0.
- Public TUB documentation and the locally supplied DIN/TS 67600:2022-08 PDF,
  extracted with `pdftotext -layout`. Licensed DIN tables are not bundled.
- Official CIE fluorescent-illuminant CSV, DOI 10.25039/CIE.DS.ukaymjdn,
  MD5 `441613e501ab0a58e62f2669ff005db7`, for the additional FL11 shape check.

Run the reference comparison from the package root, with the existing package
library on `R_LIBS`:

```sh
Rscript --vanilla data-raw/validate_material_references.R DIN.txt TUB.txt OUTPUT
```

The local detailed comparisons, input hashes, and session information are in
`/private/tmp/spectran-material-validation`. All matching non-FL11 DIN cases
meet the documented precision budget. FL11 photopic and effective-MDER
discrepancies remain unresolved. The official CIE FL11 shape agrees with the
bundled spectrum to floating-point precision. The ten inconsistent TUB stone
summary cells remain reported as discrepancies and are excluded from the
100-value public photopic regression fixture.

## Automated verification

Load, documentation, focused regression tests, the full test suite, and
`devtools::check(document = FALSE, cran = FALSE, manual = FALSE)` were run using
the project library. Sass's cache was redirected to a temporary directory.
The local check log is `/private/tmp/spectran-check-final.log`; the test run
records its R session in `/private/tmp/spectran-test-sessionInfo.txt`.
The package check completed with zero errors, warnings, or notes, and all
1,320 test assertions passed. A final German wording adjustment also passed
the focused language suite.

Tests cover quantity labels, F = 1 preservation, rescaling including zero and
invalid targets, immutable snapshots, coloured coefficient products, mixed
and repeated interactions, branch ancestry, stale-parent protection,
catalogue metadata initialization, bilingual labels, and audit ZIP contents.

## Browser verification

The in-app browser exercised the English and German interfaces. The default
catalogue is TUB / DIN/TS 67600 in both interaction modes. Reflection exposes
exitance, rho coefficients, the receiver assumption, and the editable
illuminance control with its tooltip.

An English 6500 K daylight source at 100 lx was reflected by TUB L1, promoted
at an explicit 40 lx target, then transmitted through TUB G1 with the default
illuminance. The cumulative CSV contained both comparisons and the branch
path. Independent R reconstruction reproduced every numeric field within
`1e-10` relative tolerance. The material-only and actual final illuminances
were 73.8370754 lx and 36.0496505 lx; effective MDER values were 0.7247356 and
0.3538394. These are scenario checks, not DIN reference values.

The CSV and its R verification are retained locally at
`/Users/zauner/Downloads/Spectran-cumulative-comparisons.csv` and
`/private/tmp/spectran-browser-csv-check.R`. The corresponding log and session
information are alongside the script. Restoring the original node preserved
both later nodes and their archived results.

The browser emitted existing shared input/output-ID warnings for legacy
Analysis tables. The new material controls did not add duplicate IDs. Catalogue
metadata timing, translated names, and archived reflection descriptions found
during browser review were corrected. No deployment, commit, or push was made.
