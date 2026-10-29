# Introduction and learning illustrations verification

Accepted review candidate: `TRX-M4-04-fd198ccd6a43`, ready for the user's final review. Original performance comparison: `TRX-M4-02-4cb5af865d07`.

This change integrates the six accepted LEARN-G3 diagrams in German and English, modernises Introduction, adds the material-module project committee to About, and uses separate Transmission and Reflection icons on two sidebar lines. The committee is Karin Bieske, Kai Broszio and Nils Haferkemper. Scientific calculations and material-workflow state contracts are unchanged.

## Assets and package size

The German SVGs are byte-identical to the six approved LEARN-G3 originals. The R generator and its illustrative data are retained in `data-raw/`, which is excluded from the source package. Only SVGs are shipped, not the large raster review previews. No illustrations are generated at app runtime.

Measured from the built tarball and installed package; decimal MB:

| Component | Bytes | MB |
|---|---:|---:|
| Source tarball | 3,486,053 | 3.486 |
| Installed package, total | 5,755,592 | 5.756 |
| Data | 100,715 | 0.101 |
| All web assets | 4,227,159 | 4.227 |
| All 20 explanation SVGs, included in web assets | 1,480,453 | 1.480 |
| R help databases | 38,500 | 0.039 |

The [CRAN Repository Policy](https://cran.r-project.org/web/packages/policies.html), checked on 2026-10-05, generally limits data and documentation to 5 MB each and recommends source tarballs no larger than 10 MB. All web assets plus R help total 4.266 MB, a conservative documentation grouping. The 5 MB guidance is not a blanket maximum for the installed package. The size check printed INFO for the 5.7 MB installed size, not a NOTE, warning or error.

Source tarball MD5: `da2644e5b0fdc64b4f199c6e7a5106fd`.

## Initial-load performance

The actual initial browser resource log contained **zero explanation SVG requests**. Opening the spectrum explanation then requested its SVG. Intro's collapsed material illustration was also not fetched initially. The video uses `preload="none"` and example screenshots are lazy loaded in collapsed native details.

Local A/B benchmark used the same frozen source and a fresh R app server and Chromium page for each run. The control temporarily replaced only the explanation UI, explanation server and contextual help buttons with no-op functions in the benchmark R process. The current Introduction, sidebar and CSS remained constant. No product code or frozen candidate was changed.

One warm-up per condition was discarded. Four measured runs per condition followed in balanced ABBA order, repeated twice. The endpoint was shinytest2 AppDriver initial readiness, including local R server launch and package loading.

| Condition | Runs | Median | Range |
|---|---:|---:|---:|
| With explanations | 4 | 2.4830 s | 2.385–2.601 s |
| Without explanation UI/server/buttons | 4 | 2.4605 s | 2.353–2.581 s |

The median difference is 22.5 ms (0.91%), much smaller than the observed variation. This small local sample shows no material startup penalty. It does not establish zero overhead or measure a cold start on shinyapps.io. File-system and browser executable caches may be warm. This timing comparison was made on M4-02. M4-04 only adds return-focus routing and CSS corrections; it again passed the no-initial-SVG-request check. No new startup timing is claimed for that correction.

## Checks

- Focused testthat checks: introduction navigation, repeated actions, session isolation, explanation navigation and return focus, and asset availability passed (80 expectations, including the new external-entry focus regression).
- Isolated introduction browser check: all three destinations, including repeated selection, passed.
- Integrated browser check: Import and material routes, explanation topic image loading, return focus to the Introduction learning button, and repeated entry opening the topic overview passed.
- A direct keyboard check of M4-04 in the in-app browser confirmed that the collapsed desktop sidebar is skipped and focus proceeds to visible Introduction actions.
- `R CMD check --as-cran --no-tests --no-manual`: 0 errors, 0 warnings, 0 notes. Tests were run separately. Remote incoming checks were disabled; this is not a CRAN submission or a cross-platform release check.
- Removed unused `spsComps` from Imports after replacing its introduction gallery with native details and static thumbnails. No other runtime use remained.
- Whitespace check passed for this change. Unrelated pre-existing whitespace in `renv/activate.R` was not changed.

Environment: R 4.6.1, macOS arm64; shiny 1.14.0, shinydashboard 0.7.3, shinytest2 0.5.1, chromote 0.5.1, testthat 3.3.2, pkgload 1.5.3. Full session details and raw A/B observations accompany this note under `M4-02-evidence/`; final package/browser check evidence is under `M4-04-evidence/`.

Reproduction commands used:

```sh
Rscript --vanilla /private/tmp/spectran-revision/onboarding-check.R M4-04
Rscript --vanilla /private/tmp/spectran-revision/onboarding-performance.R M4-02
Rscript --vanilla /private/tmp/spectran-revision/onboarding-package-check.R M4-04
Rscript --vanilla /private/tmp/spectran-revision/onboarding-size.R M4-04
```

The temporary scripts and frozen manifest are copied into the evidence directory. Their absolute paths document the local test environment and need adapting when reproducing on another machine.

## Independent visual review

M4-02 received UI 9.1/10 and UX 8.7/10, with three production acceptance findings: M4-UX-R01 (return focus to the Introduction learning button), M4-UX-R02 (invisible keyboard stops in collapsed navigation), and M4-UX-R03 (repeated Introduction help entry must open the overview). M4-04 addresses all three. Its targeted independent recheck closed all three by visible evidence and accepted the build with **UI 9.2/10 and UX 9.1/10**.

Both languages were rechecked at 1440 x 1100 and 320 x 844. Return focus and scroll context, repeated overview entry, closed/open menus, contextual material-help destinations and preserved reflection/L1 material drafts passed. The yellow primary source action also passed. One transient mixed topic/overview capture was resolved by a fresh settled-rendering observation; it was not classified as either a persistent defect or an initial pass. No remaining in-scope acceptance defect was observed. The full independent report is retained as `M4-04-evidence/independent-review.txt`.

Protected visual passes cover all six German integrated graphics and all six full-size English translations, both About committees, separate two-line sidebar icons, example disclosures and previews, preserved reflection drafts and a saved material light-path result after help/Back. Both languages' Introduction passed 1440/1200/1024/768/390/320 px. Additional topic, About, enlargement and menu samples passed at narrower widths. The unchanged external core-module video's playback is outside this slice; findability and explicit labelling passed. The pre-existing M3-08 topic-select controller residual is protected, not newly claimed as a pass.

Review is restricted to visible browser operation. Numerical correctness and CRAN size are owned by the implementation checks, not inferred from the visual review. Reviewer acceptance only permits showing the changes to the user, with no commit, push or deployment authorization.
