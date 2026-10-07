# M4-12: Sequential navigation through explanations

Frozen candidate: `TRX-M4-12-57cc87de8c2a`.
Source: `/private/tmp/spectran-review-oct03/M4-12/source`.
German: <http://127.0.0.1:7545/>. English: <http://127.0.0.1:7546/>.
Baseline: accepted M4-10, committed as `b26980b`.

## Change

Each topic ends with Previous topic and Next topic buttons, the destination titles, and a position indicator. The order comes from the existing ten-topic overview, including the transition from basic concepts to materials. The first previous button and last next button are disabled; the sequence never wraps. The overview has no sequence controls. Existing contextual return navigation is retained.

Topic changes reuse the established navigation and focus helper: the dropdown stays synchronized and focus returns to the explanations heading at the top. Button labels are translated, preserve visible focus styles, and stack below 700 pixels. No new assets, dependencies or browser scripts were added. Scientific content and calculations are unchanged.

## Focused programmer verification

R 4.6.1 (2026-06-24), shiny 1.14.0, shinyjs 2.1.1, htmltools 0.5.9, testthat 3.3.2.

Command run from the project root:

```r
pdf(file = NULL)
source("/private/tmp/spectran-revision/load.R")
testthat::test_file("tests/testthat/test-spectran-explanations.R", reporter = "summary")
```

All 132 expectations pass, without failures, warnings or skips. They cover both directions through all ten topics, sequence boundaries, disabled controls, ignoring zero-valued remounted buttons, dropdown entry, group transitions, and retaining the original return page and initiating link. Existing explanation-content and focus tests also pass.

The full German browser check confirms the disabled previous button on the first topic, the workflow-to-material transition, synchronized dropdown, heading focus and return to scroll position zero. The footer screenshot is in `M4-12-evidence/topic-navigation-desktop.jpg`.

Changed-file `git diff --check` passes. Unrelated existing changes in the working tree are untouched. No broad scientific or package check was repeated for this navigation-only refinement. No new startup-performance claim is made.

Web assets and shipped manual sources total 4,500,430 bytes, applying the existing `.Rbuildignore` exclusion for `man/figures/English`. The added stylesheet is 1,170 bytes larger than the accepted baseline; no image asset is duplicated.

## Independent review

The independent reviewer accepted the addition with no new findings. The visible build identifier matched in both languages. Full German forward and English backward sequences, boundaries, group transitions, dropdown and overview entry, repeated changes, Tab/Shift+Tab, Enter/Space, heading focus and contextual return passed at 1440 x 1000, 390 x 844 and 320 x 844. Long destination titles remain visible and operable. The qualitative verdict was clear, consistent and ready for acceptance. No new numerical UI/UX score was assigned. The report and selected screenshots are retained in `M4-12-evidence/`.

The user subsequently authorized including the reviewer-approved change in the next combined commit.

## Material-validation integration in the combined commit

At the user's request in the article task, the completed Material validation article and its website prerequisites are included alongside this change. The source, style, prepared DIN reference CSV, separate TUB assignment CSV, preparation script and provenance notes are preserved. All ten article/input/configuration files recorded in the accepted `manifest-mv08.json` match their SHA-256 hashes. The article task's R checks and independent MV-08 closure remain its verification basis; no article calculations were rerun or edited here. Generated website files, local settings, renv changes, deployment metadata and temporary plots are excluded from the commit.

The Validation page now embeds the new article between the existing spectral validation and underlying-values pages, using the same full-width iframe presentation. It has a translated heading, description and frame title, lazy loading and the same viewport-relative height. Both-language R rendering confirms all three entries and the exact intended public URL:

<https://litgde.github.io/Spectran/articles/Material_Validation.html>

The combined app showcase is `TRX-M4-13-4409fdc17a7b` at <http://127.0.0.1:7547/>. Its navigation source and stylesheet are unchanged from the independently accepted M4-12. The new Validation-page heading, description and frame were inspected in the full German app. The public article destination currently returns 404 because publication is still pending, as expected for the user-requested future link. This is recorded in `M4-12-evidence/validation-entry-publication-pending.jpg`; no deployment or public availability is claimed.
