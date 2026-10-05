# M4-05: Introduction and illustrated explanation layout

Candidate: `TRX-M4-05-4d0ad8041b6d`.
Frozen source: `/private/tmp/spectran-review-oct03/M4-05/source`.
German preview: <http://127.0.0.1:7531/>. English preview: <http://127.0.0.1:7532/>.
The M4-04 baseline remains unchanged on ports 7529/7530.

## User-directed changes

- Moved the LiTG background paragraph and Toolbox/luox links into the Introduction hero.
- Removed inline enlargement disclosures from all explanation figures. Full-size links remain.
- Added three actual English application screenshots for the material library, result comparison and cumulative light path. Their provenance is recorded in `data-raw/intro-examples-provenance.txt`.
- Made all four example galleries permanently visible, with equal-height card pairs on desktop and stacked cards on mobile.
- Added the Reflection icon to the right of the main material button.
- Made the core-module video a permanent section. Its URL, controls and `preload="none"` are unchanged.
- Reordered the six newly illustrated explanation topics: spectrum, quantities, colour, age, workflow and light path. A lead and essential concepts precede the illustration; interpretation and practical advice follow in a clearly grouped section. The latter places its heading alongside the body at wide widths and above it at narrow widths.

The illustration files, numerical calculations, material workflow and navigation server logic are unchanged. This is a bounded design iteration. Full numerical, export and package acceptance suites are not repeated for this candidate.

## Focused checks

R 4.6.1 (2026-06-24), macOS arm64; shiny 1.14.0, htmltools 0.5.9, shinydashboard 0.7.3.

- Both changed R files parse. Introduction and all ten explanation topics render in both languages.
- At the initial desktop viewport, the two card pairs were respectively 556.867 and 528.070 CSS pixels high, with equal heights within each pair. All twelve gallery images loaded. No horizontal page overflow was detected.
- The German Introduction, three material thumbnails, open video section, quantities topic and its new interpretation section were visually inspected. The return from Explanations restored focus to `intro-to_explanations`.
- New screenshots are native JPEG captures of the running English M4-04 app. They contain no constructed values or retouched application controls.
- `R CMD build --no-build-vignettes --no-manual` succeeded. Changed tracked files passed `git diff --check`.

Reproduction scripts, rendering log, source manifest, size inventory and browser screenshots are in `M4-05-evidence/`. These scripts include machine-specific paths.

## Size boundary

| Component | Bytes |
|---|---:|
| Source tarball | 3,680,214 |
| All web assets | 4,464,569 |
| New example JPEGs, included above | 236,635 |
| Manual sources actually shipped | 32,633 |
| Web assets plus shipped manual sources | 4,497,202 |

The built archive remains below the 10 MB source-package guidance. The web assets plus shipped manual sources remain below 5 MB. The established `.Rbuildignore` excludes the larger historical `man/figures/English` screenshots, raw illustration sources and verification evidence. Size checks use the archive's actual file list, not those excluded development files. Data and manual content are unchanged from M4-04. This is a packaging-size check, not a new CRAN submission or complete `R CMD check` result.

No startup timing benchmark was repeated for this layout iteration. Images remain lazy loaded, the video still uses `preload="none"`, and no graphics are calculated at startup. Always-visible galleries can request their images when they approach the viewport; this differs from M4-04's collapsed examples. The earlier M4-02 timing results are not presented as a fresh measurement of M4-05.

## Independent review

The independent visual task received the exact candidate, both URLs, changed topics, user-directed removal of inline zoom, continuing local transfer authorization and protected navigation passes. Review is restricted to visible UI operation and targeted responsive/keyboard regression checks. Acceptance permits presentation to the user only, with no commit, push or deployment.

M4-05 was accepted with **UI 9.2/10 and UX 9.2/10**, with no in-scope acceptance defect. The reviewer inspected both languages at 1440 pixels, German samples at 1024 and 320 pixels, and English samples at 768 and 390 pixels. All six topics were read above and below their graphics in both languages. Narrow samples covered each changed layout pattern, keyboard activation of full-size links, and return focus. The four older material topics were visited only to confirm removal of inline enlargement. The full report is retained as `M4-05-evidence/independent-review.txt`.

### M4-06 caption correction

Final candidate: `TRX-M4-06-241d6979423d`, German <http://127.0.0.1:7533/> and English <http://127.0.0.1:7534/>. The only product change from M4-05 replaces underscores in visible gallery captions and accessible link names with spaces. This addresses the optional reviewer improvement `M4-UX-I01` (`Export_UI` to `Export UI`) without changing image destinations. An English R render confirms the revised caption.

The rebuilt source archive is 3,680,270 bytes. The web and documentation asset sizes above are unchanged. M4-05's visual coverage is protected; the independent follow-up is restricted to the caption and its image link. Final source manifest, size inventory and screenshots are under `M4-06-evidence/`.

The independent follow-up accepted M4-06 at **UI 9.2/10 and UX 9.2/10**. `M4-UX-I01` was closed by visible evidence: the English caption and accessible name use `Export UI`, Enter opens the correct image, and the German caption remains correct. There are no open in-scope acceptance defects or remaining caption recommendations. See `M4-06-evidence/independent-review.txt`.
