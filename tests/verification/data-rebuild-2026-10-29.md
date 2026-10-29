# Dataset rebuild correction, 2026-10-29

The [review of PR #60](https://github.com/LiTGde/Spectran/pull/60#discussion_r4412966122)
identified a real maintenance bug. The directory-wide source loop included
`prepare_din_material_coefficients.R`, which stops without two command-line
arguments, as well as illustration and deployment generators. Reproducing the
DIN helper without arguments returned its expected argument error.

The rebuild now explicitly lists dataset builders in dependency order. The
transmission translation generator remains a necessary first step: it updates
the generated language workbook before `language.R` reads it. Licensed DIN
reference preparation, validation, illustrations and deployment maintenance
are separate operations. Build intermediates are local, the caller's objects
are retained, and the workbook writer's use of the random seed is restored.

This follows the [R Packages data workflow](https://r-pkgs.org/data.html#sec-data-data-raw):
retain reproducible data preparation under `data-raw/`, save public datasets in
`data/` and internal datasets in `R/sysdata.rda`, and exclude preparation code
from the built package.

## Verification

R 4.6.1 on aarch64 macOS, with the existing project library. Relevant versions:
dplyr 1.2.1, tidyr 1.3.2, purrr 1.2.2, tibble 3.3.1, readr 2.2.0,
readxl 1.5.0, here 1.0.2, tidyverse 2.0.0, usethis 3.2.1,
photobiologyFilters 0.6.1, R.matlab 3.7.0, digest 0.6.39 and withr 3.0.3.

Run from the package root in the project R environment:

```sh
Rscript tests/verification/check-data-rebuild.R
```

For this verification, `Rscript --vanilla` used the existing project library
through `R_LIBS_USER`. `SPECTRAN_SPITSCHAN_ARCHIVE` pointed to a local copy of the
pinned public archive, after the initial R download timed out. The existing
builder verified SHA-256
`5ab0d81db46cb17ddf179df36fd9104dd3b7ac33442036bb58968aa0445518f9` and its
individual source-file checksums. No licensed DIN input was provided.

The test builds in a temporary copy and verifies:

- Both `use_data()` calls complete, producing all 11 internal and 4 public
  datasets. Every object matches the committed object with
  `all.equal(..., tolerance = 0)`, including attributes and translations.
- A new unrelated R helper containing `stop()` is not executed.
- Checksums of all other `data-raw/` files, all `inst/` files and the deployment
  manifest are unchanged. Only the generated translation workbook is an
  expected intermediate output.
- Caller objects and the global workspace are preserved.

Evidence: `/private/tmp/spectran-data-rebuild-passed/` contains the object
comparisons, input hashes, package versions and `sessionInfo()`.
The console log is `/private/tmp/spectran-data-rebuild-passed.log`.
No bundled `.rda` file or deployed app code was changed by this correction.

## Follow-up: verify regeneration and relative evidence paths

The [review of PR #61](https://github.com/LiTGde/Spectran/pull/61) identified two
defects in the verification script itself. A relative evidence directory was
resolved again after `setwd()`, and copied `.rda` outputs could let a skipped
save pass. Both findings were reproduced in isolated package copies.

The evidence directory is now normalized before constructing the staging
path. All five staged `.rda` files are deleted before the rebuild, their
absence is asserted, and every expected output must exist afterwards before
its contents can be compared. Only the temporary copies are deleted.

Ten scenarios were checked in separate R processes with the same R version,
existing package library and checksum-verified local archive listed above:

| Scenario | Previous checker | Corrected checker |
|---|---|---|
| Relative evidence path, full rebuild | Failed after rebuilding | Passed |
| Absolute evidence path, full rebuild | Previously passed | Passed |
| Default evidence path, full rebuild | Not rerun | Passed |
| Entire rebuild replaced by a no-op | Incorrectly passed | Failed with missing outputs |
| Internal-data save omitted | Incorrectly passed | Failed with missing `R/sysdata.rda` |
| Public-data save omitted | Incorrectly passed | Failed with all four public outputs missing |

The three successful corrected runs each recreated all 11 internal and
4 public objects and matched the committed values and attributes with zero
numerical tolerance. Evidence files were written at the requested relative
and absolute locations. The original package files retained their checksums.

Command: `Rscript --vanilla /private/tmp/spectran-rebuild-check-regressions.R`,
with `R_LIBS_USER` pointing to the existing project library. The runner creates
all mutations in temporary copies. Its output is recorded in
`/private/tmp/spectran-rebuild-check-regressions.log`; per-scenario logs,
comparison tables, source checksums and R session information are under
`/private/tmp/spectran-rebuild-check-regressions-20261029/`. The default-path
scenario uses R's temporary directory, which is removed when that process exits.

## Release checks and legacy workflow

The [R-CMD-check run for the development merge](https://github.com/LiTGde/Spectran/actions/runs/40883206281)
completed successfully with `Status: OK` at commit
`6e5f7c53c970ef5b6ac08bac47b73113a02007a0`. It does not run data preparation,
because `data-raw/` is excluded by `.Rbuildignore`.

The old Docker image workflow was disabled in GitHub Actions; its API state
is `disabled_manually`. Only the legacy shinyapps.io deployment references
that image. Both Posit Connect Cloud prototypes publish directly from
`transmission`, `app.R` and `manifest.json`.
