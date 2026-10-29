# Development preview and bilingual hosting, 2026-10-29

## Scope

Spectran 2.0.0.9000 adds a German/English startup notice once per session,
links the existing material verification article, and identifies the interface
as a development prototype. Hosted language links point to Posit Connect Cloud.
The macOS packaging experiment is excluded from these commits.

## Verification

R 4.6.1 on aarch64 macOS, using the existing project library; shiny 1.14.0,
shinydashboard 0.7.3 (Bootstrap 3), testthat 3.3.2, bslib 0.12.0,
pkgdown 2.2.1, rmarkdown 2.31 and Quarto CLI 1.9.37.

Commands run from the isolated worktree with `Rscript --vanilla` and the
existing project library prepended to `.libPaths()`:

```r
options(sass.cache = file.path(tempdir(), "sass"))
devtools::test(reporter = "summary", stop_on_failure = TRUE)
devtools::test(filter = "startup-notice", reporter = "summary",
  stop_on_failure = TRUE)
pkgdown::build_site_github_pages(new_process = FALSE, install = FALSE)
```

- The full suite passes. Four pre-existing manual-colour-scale warnings occur
  in source-picker tests, as already recorded in the 2026-10-07 verification.
- All 18 startup expectations pass without warnings. The test supplies the two
  spectrum-builder UI defaults that `testServer()` does not populate.
- Browser checks confirm German and English text, keyboard dismissal, and no
  repeated notice after navigation. New sessions display the notice again.
- Both language links and their directions were checked against live apps.
- R checks confirm that only the two hosted URLs changed in the language
  workbook and internal language data. Every other internal object and cell
  value remains identical.
- R checks of `app.R` confirm German and English overrides with no switch,
  plus the legacy-language and default fallbacks.
- The complete pkgdown build succeeds, including `Material_Validation.qmd`.
  The older `Validation.Rmd` has pre-existing missing-alt-text warnings.
  Home, material article and table navigation were inspected in the browser.
- Manifest file checksums match the deployed source files; dependency entries
  were retained because this change adds no package dependencies.

The pkgdown check installed only this repository into a temporary library.
It installed no dependencies. The material article uses its existing R/knitr
workflow and bundled reference inputs. Input hashes, software versions and
session information from that render are retained in
`/private/tmp/spectran-development-qa/material-validation/`; build provenance
and logs are in the parent directory. Full test output is in
`/private/tmp/spectran-development-tests.log`.

## Connect Cloud configuration

Both prototypes use repository `LiTGde/Spectran`, branch `transmission`,
primary file `app.R`, and enabled **Automatically publish on push**.

| Setting | German | English |
|---|---|---|
| Content ID | `01a121fe-bb0f-e138-1595-0d2f04b5a5f9` | `01a187d7-c91b-6e6b-ccdb-c666903aaaa5` |
| Public URL | <https://litg-spectran-prototype.share.connect.posit.cloud/> | <https://litg-spectran-prototype-en.share.connect.posit.cloud/> |
| `SPECTRAN_LANGUAGE` | `Deutsch` | `English` |
| `SPECTRAN_LANG_LINK` | `false` | `false` |

The German content ID and public URL are preserved. The older migrated
`Spectran_transmission` deployment is not part of this change.
The uppercase language override takes precedence over `language` in the
committed `.Renviron`; otherwise the German default would override an English
deployment. Regular hosted builds retain the optional language switch.

Merge to `main` with `[no-deploy]` in the merge message to skip the legacy
shinyapps.io workflow while allowing pkgdown to publish to GitHub Pages.
