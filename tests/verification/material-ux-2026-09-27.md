# Independent material-module UX review

Status: **B08 independently accepted for user final review on 2026-09-28**.
Candidate `MAT-UX-B08-4d3bf2c8519b` closes R07 and C01. No remaining
acceptance defect was found in the changed material workflow. P01/R06 and
the earlier independent passes remain protected; R03 is withdrawn.
The reviewer delivered a direct completion notification and the coherent
report at `/private/tmp/spectran-ux-review/reviewer/B08-report.md`.
Scheduled checks stay paused. Acceptance permits the user's final review.
No candidate authorizes commit, push, integration, submission or deployment.

The user requested a separate review with the `review-shiny-ux` skill and
coordination through completion before handoff. Reviewer chat:
`01a0e2a9-f5b8-7612-8fe1-b19d2dbc0b63`, local host. The reviewer uses visible
mouse/keyboard journeys against frozen integrated app copies and does not
edit implementation files. The coordinator reads its reports and submits
new candidates. At the user's explicit request on 2026-09-27, scheduled
checks are paused and the reviewer should notify the coordinator when a
coherent review is ready. Acceptance authorizes user final review only.

## Candidates and scope

- B01: `MAT-UX-B01-688636c2e887`; application SHA-256
  `688636c2e8878e25658bab3d6751dae8c84bc858004a803a2572672871e0dfb6`.
- B02: `MAT-UX-B02-6d1788575128`; application SHA-256
  `6d178857512852c478a00073b4c6c8fe7aba9792b5bfe1574c64f5b95815a868`.
  English `http://127.0.0.1:7424/`; German `http://127.0.0.1:7425/`.
- B03: `MAT-UX-B03-aa9a9ddd0a4b`; application SHA-256
  `aa9a9ddd0a4b22ae81989a6fcc3ace4ef72b8cc0aa9d580d8a00983fb210a523`.
  English `http://127.0.0.1:7426/`; German `http://127.0.0.1:7427/`.
- B04: `MAT-UX-B04-9d8c658003a1`; application SHA-256
  `9d8c658003a17589dbc6cc12b5502b1150eedbb18825c7e457c48f30a4b3c70a`.
  English `http://127.0.0.1:7428/`; German `http://127.0.0.1:7429/`.

- B05: `MAT-UX-B05-12ae1d2a4cef`, frozen and superseded before dispatch.
- B06: `MAT-UX-B06-79341c5505fa`; application SHA-256
  `79341c5505fa82cf5e079dc2b002736e3ce0574ec00bf01a34dc83ffc5c6d650`.
  English `http://127.0.0.1:7432/`; German `http://127.0.0.1:7433/`.

The contract, frozen manifests, revision notice and reviewer evidence are in
`/private/tmp/spectran-ux-review`. Production acceptance covers 768, 1024,
1200 and 1440 CSS pixels; usable core at 390; severe failures at 320.
English full workflow and German representative core/export paths are in
scope. Scientific validation and model boundaries are documented separately
in `materials-2026-09-27.md` and `inst/material-model.md`.

## Independent findings

| ID | Finding | Change | Independent status |
| --- | --- | --- | --- |
| MAT-UX-R01 | Reflection tails described as opaque/transparent | B03 makes both tail controls depend on the interaction while preserving raw choices | Closed by independent B03 review in both languages and directions |
| MAT-UX-R02 | Reflection export page/file descriptions used transmission wording | Neutral shared heading; labels follow the selected current/archived mode; reflectance filenames and neutral audit descriptions | Closed by independent B02 review in both languages, including mixed-history archive selection |
| MAT-UX-R03 | Reported wavelength-title crop in a default PNG | Full-resolution reinspection showed complete label and caption | Withdrawn by independent reviewer in B01 report; preview cropping was not an application defect |
| MAT-UX-P01 | Long title/subtitle and panel collisions in PNG exports, reopened for S1 reflection in B06 | B07 gives title/subtitle a measured shared header above the panels in both export writers | Closed by independent B07 review of downloaded current/archived, standalone/combined PNGs |
| MAT-UX-R04 | Shared CSV column label and receipt retain transmission/English wording | B03 uses a neutral coefficient-column label and localized receipt without an incorrect spatial reference | Closed by independent B03 review |
| MAT-UX-R05 | Interaction changes silently reset an uploaded material's scale and custom name | B04 rebuilds Details from current upload decisions and renews interaction-specific acknowledgements | Closed by independent B04 review in both languages/directions, including post-switch application and downloaded audit metadata |
| MAT-UX-R06 | Incomplete reflection uploads triggered a raw coefficient error in the preview | B07 waits for curve readiness before calculating colour, while retaining the partial plot and completion guidance | Closed by independent B07 review in both languages, including tail/gap recovery and invalid replacement |
| MAT-UX-R07 | Compact German standalone G11 export overlaps panel A and the vertical axis unit | B08 places the complete irradiance unit on a second axis-title line in exported figures | Closed by independent B08 review of all 20 actual app-downloaded PNGs, including the exact Halogen/S1/G11 reproduction |
| MAT-UX-C01 | User replaced the two-illuminant material-colour preview with D65 only | B08 shows one D65 card in input, normalization, applied and archived results; numerical coefficient comparison is retained | Closed by independent B08 review in both languages, including zero sources, archived materials and readiness recovery; Y remains unchanged |

The coherent B02 report is
`/private/tmp/spectran-ux-review/reviewer/B02-report.md`. It passes the
German semicolon/decimal-comma percent upload, completed-material CSV
download/reimport, both-language mixed-history exports and keyboard paths.
Responsive B02 sampling covers German export at 1200 and 390 CSS pixels.
The unchanged B01 six-width matrix and earlier catalogue, recovery, receiver,
branch and cumulative-comparison passes are carried forward rather than
claimed as fully rerun on B02. The reviewer explicitly requires a frozen
revision and another independent retest before handoff.

The coherent B03 report is
`/private/tmp/spectran-ux-review/reviewer/B03-report.md`. It closes R01 and
R04, protects R02/P01 and the withdrawn R03, and identifies R05 as the remaining
production-bound blocker. Both languages and directions reset an explicitly
selected Percent scale to Fraction while retaining the same uploaded file;
low coefficients still appear valid and ready. B03 therefore is not accepted.
Its report records keyboard and 1200/390 responsive samples, audit downloads,
and transfer delays that eventually completed successfully. Those delays were
environment interruptions, not application failures or permission rejections.

## Independent B04 acceptance and handoff

The coordinator received the reviewer's direct completion notification on
2026-09-28. The coherent acceptance report is preserved unchanged as
`material-ux-reviewer-B04-2026-09-28.md` in this directory; the original is
`/private/tmp/spectran-ux-review/reviewer/B04-report.md`. R05 is closed,
R01/R02/R04/P01 remain protected closed passes, and R03 remains withdrawn.

Visible retesting covered both interaction directions and both languages
with Fraction and low Percent uploads, retained names/geometry/angles,
mode-appropriate tail labels, renewed acknowledgements, successful application,
and the downloaded German audit metadata. Replacing an upload and returning
to TUB catalogue entries restored the correct fresh-input defaults. B04
sampled German 1200/390 layouts and fresh-session English keyboard recovery;
the report identifies the wider matrix and prior passes carried forward.

The unchanged showcase was restarted at the user's request after the reviewer
verified all 108 manifest files. The coordinator also confirmed all 108 frozen
and author runtime files match the accepted manifest before handoff. English
and German previews remain at ports 7428 and 7429. Scheduled checks remain
paused; this handoff follows the direct completion notification.

Residual limits: this is UX acceptance, with no independent numerical,
full accessibility or CRAN determination. Documented FL11 reference differences
and TUB source-table inconsistencies remain. The F=1 scenario and absence of
room geometry, BRDF and automatic interreflection remain explicit model limits.
Histories persist for one session, with audit export for preservation. Browser
transfer delays and interrupted local servers are documented without attributing
an unestablished cause to the app. Fresh-session keyboard retesting resolved
the earlier ambiguous controller observations; no open UX defect remains.

## Programmer verification

R 4.6.1 with the existing project library, shiny 1.14.0, testthat 3.3.2,
dplyr 1.2.1, tibble 3.3.1, purrr 1.2.2, ggplot2 4.0.3 and gt 1.3.0.
No package installation or full data-raw rebuild.

New regression tests cover English/German reflection tail readiness for both
fraction and percent inputs, completed boundary coefficients, preserved
normalization decisions, current transmission versus archived reflection
export labels, actual bundle filenames, and bilingual audit records. The
existing export-heading assertion was updated to the shared material heading.

Final command: `Rscript --vanilla /private/tmp/spectran-check.R`.
The script uses `devtools::check(document = FALSE, cran = FALSE,
manual = FALSE)` with the project library and temporary check/cache paths.
Final log: `/private/tmp/spectran-ux-b02-check.log`.
Result: **1,376 passed, 0 failed, 0 warnings, 0 skips; R CMD check 0 errors,
0 warnings, 0 notes** after all B02 changes.

Figure QA command:
`Rscript --vanilla /private/tmp/spectran-ux-export-check.R`.
Inputs are the existing synthetic D65/partial-curve fixtures, calculated in R.
English/German transmission and reflection PNGs were generated at default
9 by 5.8 inches, font 15, and compact 8 by 4 inches, font 11, with long source
and material names. Compact versions include coefficient panels and photopic
and melanopic overlays. Representative output inspection confirmed complete
axis labels and separation of long titles from subtitles. Artifacts and
session information: `/private/tmp/spectran-ux-review/programmer-figures`.
These programmer artifacts do not substitute for visible app download tests.

### B03 mode-switch regression

The reviewer reproduced stale tail wording when changing the interaction
without replacing a partial CSV. Coverage controls had no reactive
dependency on the interaction. B03 gives those controls their own labeler
bound to the current mode. The completed curve and raw zero/one/carry choices
are unchanged. Three runtime files changed: `R/transmission.R`,
`R/material_language.R` and `R/transmission_language.R`. The adjacent CSV
column label is now mode-neutral and the receipt is localized.

Expanded tests use actual partial CSV uploads in both languages and scales,
change modes in both directions, inspect both selected tail choices, compare
the completed curve with R's `expect_identical()`, and check missing-choice
readiness/recovery. They reproduced the B02 wording failures and pass on B03.
Focused log: `/private/tmp/spectran-ux-b03-focused.log`.

The full packaged B03 candidate passes **1,476 assertions, 0 failures,
0 warnings, 0 skips; R CMD check 0 errors, 0 warnings, 0 notes**.
Command: `Rscript --vanilla /private/tmp/spectran-check.R`.
Log: `/private/tmp/spectran-ux-b03-check.log`.
The size guard and unchanged-data audit also pass; see the updated size
verification report. B03 is submitted for independent retest, not accepted.

No commit, push, integration or deployment has been performed.

### B04 uploaded-metadata regression

B04 changes only `R/transmission.R` among frozen runtime files. Details now
keeps the current upload's scale, name, definition, scattering selection,
geometry and angle when interaction labels are rebuilt. Reading these values
in `isolate()` avoids rebuilding controls on edits. Mode changes clear
qualification/scattering acknowledgements so readiness requires any relevant
confirmation for the new interaction. Existing file/catalogue reset paths
and coefficient normalization are retained.

The expanded R regression uses the same small values under Fraction and
Percent, so a percent-to-fraction reset would remain in range. It inspects
generated control values as well as server state, completed coefficients,
applied snapshots and serialized audit metadata in both languages/directions.
This closes the earlier test gap: `testServer()` does not rebind generated
control defaults the way a browser does. Separate checks cover retained
measurement details and fresh acknowledgements. The pre-fix run recorded
46 failed expectations with zero errors or warnings. The corrected focused
run passes the UX regression, material-server and transmission-server files.
Logs: `/private/tmp/spectran-ux-b04-before.log` and
`/private/tmp/spectran-ux-b04-focused.log`.

Full command: `Rscript --vanilla /private/tmp/spectran-check.R`.
The packaged candidate passes **1,561 assertions, 0 failures, 0 warnings,
0 skips; R CMD check 0 errors, 0 warnings, 0 notes**.
Log: `/private/tmp/spectran-ux-b04-check.log`. All four size budgets and the
nine-file unchanged-data audit pass, recorded in the size report. Manifest
verification confirms B03 remains unchanged and B04 matches author runtime
files. These programmer checks do not establish independent UX acceptance.

The B04 revision notice requests the original R05 reproduction, bilingual
both-direction scale/name retention, application/audit export, adjacent
acknowledgement recovery, fresh-input defaults, keyboard and responsive
samples. Closed R01/R02/R04/P01 and withdrawn R03 remain protected. The user
has explicitly authorized reviewer completion notifications to the
coordinator; the `finish-spectran-ux-review` scheduled automation is paused.

## Additional package-size requirement

The user subsequently required the extension to retain a size suitable for
CRAN. Packaging exclusions and a repeatable 5 MB release budget were added
without changing any B02 runtime source or assets. See
`cran-size-2026-09-27.md` for measured source/installed sizes, unchanged-data
verification, the negative guard test, and the clean packaged-candidate check.

## B06 programmer evidence and added scope

The complete revision notice is `/private/tmp/spectran-ux-review/B06/revision.md`.
Added user requirements cover approximate reflection colours, DIN material names
and aliases, Light and radiation / alpha-opic / material coefficient tables,
source versus D65 percentage-point comparisons, scaling notices, normative
MDER references, and separate Promotion/History tabs with one shared row
selection, native Show/Restore buttons, disabled unavailable actions, short
N labels and short source/interaction names. Both cumulative comparisons remain
for unscaled paths. All 55 TUB curves remain unchanged.

R CMD check: 0 errors, 0 warnings, 0 notes; 1,727 assertions pass, no skips.
Log: `/private/tmp/spectran-b06-check.log`. Source and installed package sizes:
3,162,654 and 4,080,659 bytes. All four conservative size budgets pass; see
`cran-size-2026-09-27.md`. Eight non-metadata data objects, including every TUB
curve, match B04. Nine shipped data/provenance files match their author inputs.
Source documentation was regenerated and scoped Air/whitespace checks pass.
The maintained APIs used here were checked against the official gt
text_transform and Shiny session reference pages on 2026-09-28.

An author smoke test in a separate German B06 browser session imported the
built-in 6500 K/100 lx daylight, applied G1, promoted N2, opened History, and
used Space on Show N1. N2 remained the active source, N1 became the shared
selection, the source archive notice appeared, Show N1/Restore N2 were disabled,
and focus returned to N1's label. This is functional author evidence, not an
independent acceptance decision. The legacy promotion transition opens Analysis;
returning to the material workflow retains the separate Promotion/History state.

The full R DIN comparison report is at
`/private/tmp/spectran-material-comparison-2026-09-28/DIN-Vergleich.html`.
It reports all 669 material comparisons and 20 source comparisons, including
all deviations and explicit FL11 exceptions. Scientific input spectra and
calculation functions are unchanged by B06's history UI edits. No claim of
universal normative agreement is made. Scheduled coordination stays paused;
reviewer completion notifications remain explicitly user-authorized.


## B06 review and B07 revision

The coherent independent B06 report is
`/private/tmp/spectran-ux-review/reviewer/B06-report.md`. It passes the new
history controls, including grey disabled Show/Restore, shared selection,
branching and keyboard use; result tables, material labels, colours and
scaling recovery also pass within its stated scope. Its six-width matrix
is retained. B06 is not accepted because of P01/R06. R01/R02/R04/R05 remain
closed and R03 remains withdrawn.

B07 is `MAT-UX-B07-2dbe81da2c7a`, application SHA-256
`2dbe81da2c7a475d2ff224b76125195a9e13eef4a1bd61382c46dafd206316b7`. English `http://127.0.0.1:7434/`, German
`http://127.0.0.1:7435/`. Full notice:
`/private/tmp/spectran-ux-review/B07/revision.md`.
Only `R/transmission.R`, `R/transmission_presentation.R` and `NEWS.md`
changed from the B06 runtime manifest. Scientific calculations, data,
history semantics and result tables are unchanged. Test/packaging additions
do not change the frozen runtime. Prior candidates remain unchanged.

## Final programmer release gates, 2026-09-28

- R CMD check: **0 errors, 0 warnings, 0 notes**. All **2,020 assertions**
  pass, with no skipped tests. Final check took 4m 23s. Logs:
  /private/tmp/spectran-b07-check.log and
  /private/tmp/spectran-r-check/Spectran.Rcheck/tests/testthat.Rout.
- Final source archive **3,164,813 bytes**, bundled data **475,503 bytes**,
  documentation/app resources **2,760,565 bytes**, installed package
  **4,083,590 bytes**. All are below 5,000,000 bytes. Size log:
  /private/tmp/spectran-b07-size.log. Source archive MD5:
  331ef0e9155847c71abbe9727dd8f56c.
- Generated Rplots.pdf files are now excluded by .Rbuildignore and rejected
  by the size guard if ever shipped. The new graphics-layout test uses a
  null PDF device rather than writing a repository-side test artifact.
- R 4.6.1 and the existing dependency library. Documentation regenerated;
  Air formatting and scoped whitespace checks pass. The ggtext textbox API
  was checked against https://wilkelab.org/ggtext/reference/element_textbox.html.
- New tests exercise one/two missing tails, a large gap, completion recovery,
  raw malformed coefficients, preserved partial preview and colour readiness,
  in both languages and modes. Figure tests check measured header height and
  width, growth for longer text, immutable snapshots and literal punctuation.
- Programmer-generated PNGs cover S1 reflection and its G11 transmission
  descendant, 9 x 5.8/font 15 and 8 x 4/font 11, each with/without panel B,
  standalone and combined with the coefficient table. Visual inspection
  found separate header/subtitle/panel labels and complete axes/footnotes.
  Script, images and R provenance: /private/tmp/spectran-b07-figures.R and
  /private/tmp/spectran-b07-figures/. These are not independent acceptance.
- Author browser smoke used a separate fresh German B07 session: daylight
  6500 K at 100 lx, Reflection upload material-partial-fraction.csv, preserved
  construction preview/completion note in both input and normalization,
  then carry-first/carry-last restored both colour cards and Apply. The
  session was closed. Initial chooser timing and a control redraw required
  fresh UI observation; the subsequent local upload completed. No state
  injection or artificial numeric validation was used.
- B04/B05/B06 frozen files are unchanged. All 109 B07 manifest files verified;
  all 88 shipped runtime/data files match the checked source archive. Two
  pre-existing app.R build exclusions remain outside the source package.
  Nine data/provenance files match the archive. All 55 TUB examples and all
  scientific curves/functions remain unchanged. R evidence:
  /private/tmp/spectran-b07-data.log.

Please independently decide P01/R06 and state protected passes and residual
risks. B07 remains pending until that coherent decision is received.

The B07 revision notice was successfully delivered to the existing reviewer
chat on 2026-09-28. No scheduled check was created or resumed.

The DIN comparison also has a durable local copy outside the package:
`/Users/zauner/Documents/Gremienarbeit/TWA/Projekte/Spectran/LiTG_Antrag/2026/Spectran_Materialvergleich_2026-09-28/DIN-Vergleich.html`.
R verified all 12 copied report/provenance files byte-for-byte and confirmed
that the data objects are unchanged from B05 except for UI language, with
unchanged scientific calculation files. See `copy-verification.txt` and
`LIESMICH.md` beside the report. This did not recalculate or modify the
reported reference values and does not claim a self-contained reproduction
package. The original licensed DIN input is not copied into that folder or
the R source archive.


## Independent B07 decision and B08 revision

The independent B07 report closes P01 and R06 and requests revision for R07,
the compact German export axis/tag overlap, plus C01, the user's D65-only
material colour preview. Report:
`/private/tmp/spectran-ux-review/reviewer/B07-report.md`.
No other protected finding was reopened. R03 remains withdrawn. The former
two-colour tests remain historical evidence, not a requirement after C01.
The question about Y does not authorize removing Y or adding X/Z.

B08 is `MAT-UX-B08-4d3bf2c8519b`, application SHA-256
`4d3bf2c8519b287bc0d0ba9961695b22342bfdd20d9752ffb3ad9bbe9958b6cc`.
English `http://127.0.0.1:7436/`, German `http://127.0.0.1:7437/`.
Full change description, protected scope and independent retest request:
`/private/tmp/spectran-ux-review/B08/revision.md`.
Export axis units move to a second line; single D65 cards replace the former
pair in every reflection preview. Scientific functions/data, coefficient
comparisons and history semantics remain unchanged.

## B08 programmer release gates, 2026-09-28

- R CMD check: **0 errors, 0 warnings, 0 notes**. All **2,071 assertions**
  pass, with no skipped tests. Duration 4m 30.9s. Command:
  `Rscript --vanilla /private/tmp/spectran-check.R`. Log:
  `/private/tmp/spectran-b08-check.log`. The current test log is
  `/private/tmp/spectran-r-check/Spectran.Rcheck/tests/testthat.Rout`.
- Source archive **3,165,642 bytes**, bundled data **475,503 bytes**,
  documentation/app resources **2,760,522 bytes**, installed package
  **4,082,811 bytes**. Each is below the 5,000,000-byte project budget.
  Source archive `/private/tmp/spectran-r-check/Spectran_1.0.6.tar.gz`,
  MD5 `9155517cedbfba689c5d1e6155ea37c7`. Gate:
  `Rscript --vanilla data-raw/validate_package_size.R /private/tmp/spectran-r-check/Spectran_1.0.6.tar.gz /private/tmp/spectran-r-check/Spectran.Rcheck/Spectran`.
  Log `/private/tmp/spectran-b08-size.log`.
- R 4.6.1, existing project library: shiny 1.14.0, testthat 3.3.2,
  dplyr 1.2.1, tibble 3.3.1, purrr 1.2.2, ggplot2 4.0.3, gt 1.3.0,
  colorSpec 1.8.0 and spacesXYZ 1.6.0. Session record:
  `/private/tmp/spectran-b08-sessionInfo.txt`. Air formatting and scoped
  whitespace checks pass. No dependencies, installations or public API added.
- Targeted colour, UX and presentation regressions pass. A real rendered
  graphics-layout test checks that the complete compact German y-axis titles
  fit their allocated panel height. The existing shared-header tests pass.
  Both-language Shiny server tests cover a single D65 card at zero incident
  illuminance and frozen archive colour after a material draft change.
  Existing strict invalid-curve and tail/gap readiness tests pass.
- Twenty author-generated PNGs cover DE S1 and G11, 9 x 5.8/font 15 and
  8 x 4/font 11, coefficient panel on/off, standalone/combined; plus long
  literal English repeated-reflection titles in both sizes. Visual inspection
  found separated titles, tags, axis wording/units and complete footnotes.
  `/private/tmp/spectran-b08-figures.R`, `/private/tmp/spectran-b08-figures/`
  and `/private/tmp/spectran-b08-figures.log` retain the script, outputs,
  snapshots and R provenance. These are layout fixtures, not independent
  app-generated downloads: the German fixture uses CIE A at 100 lx with
  Halogen journey labels, and the English fixture uses S1 with the long
  literal test name. Independent retest must use the actual UI journeys.
- Author browser smoke used a separate fresh DE B08 session with built-in
  Halogen at 100 lx and S1 Reflection. Input, normalization and applied result
  each show one D65 card, Y and localized approximation/method wording.
  The source/D65 coefficient comparison and result-tab order remain.
  This session was closed; user and reviewer tabs were preserved. No browser
  state injection was used. Archive/zero behavior is server-test evidence,
  not claimed as rerun in this browser smoke.
- All 109 B08 manifest files match the author runtime; all 88 shipped
  runtime/data files match the checked source archive. R CMD build's normal
  DESCRIPTION metadata formatting is outside that byte comparison.
  B04/B05/B06/B07 frozen files are unchanged. Nine data/provenance files are
  byte-identical between author and archive. All 55 TUB examples and the
  eight other data objects remain unchanged from B04. The scientific colour
  function body/formals are unchanged from B07. Commands and evidence:
  `Rscript --vanilla /private/tmp/spectran-b03-data-check.R` and
  `Rscript --vanilla /private/tmp/spectran-b08-verify-data.R`, with logs
  `/private/tmp/spectran-b08-data.log` and `/private/tmp/spectran-b08-science.log`.

At dispatch, R07/C01 awaited independent acceptance. No scheduled check was created or
resumed, and no commit, push, integration, submission or deployment occurred.

The complete B08 revision notice was successfully delivered to the existing
Independent Reviewer chat on 2026-09-28. Its direct completion notification
is requested under the user's explicit authorization. Runtime manifests were
reverified before dispatch; no frozen application file was modified.

## Independent B08 acceptance and final handoff

The Independent Reviewer sent its authorized direct completion notification
on 2026-09-28. The coherent report is
`/private/tmp/spectran-ux-review/reviewer/B08-report.md`. Decision:
**accepted for user final review only**, candidate
`MAT-UX-B08-4d3bf2c8519b`, application SHA-256
`4d3bf2c8519b287bc0d0ba9961695b22342bfdd20d9752ffb3ad9bbe9958b6cc`.

All 20 actual UI-downloaded PNGs pass independent visual inspection:
the exact German built-in Halogen 100 lx, S1 Reflection and unscaled
long-name G11 Transmission journey, current/archived results, both sizes,
panel on/off, standalone/combined, and the long literal English title.
R07 is closed. P01 and immutable archived source/quantity wording remain.
C01 is closed by both-language checks of input, normalization, applied and
archived views, verified zero sources and different active drafts. Single
D65 cards, Y and localized explanations remain; Transmission has no card.
Missing-tail, gap acknowledgement and invalid/replacement recovery protect
R06. Keyboard method access and 768/390 single-card layouts pass. Broader
earlier passes are protected, not claimed as fully replayed on B08.

Residual limits are retained explicitly: intermittent browser-controller
delays eventually completed; decisions used settled Shiny states. An initial
rapid English promotion did not establish zero and was excluded from zero
evidence; a separate source was verified at 0 lx. General Analysis displays
NA/NaN for some undefined zero-source values, an adjacent nonblocking
observation outside the changed material UI, not established as a regression.
The changed material views explain undefined values. The narrow construction
legend remains small/partly clipped within the desktop-first scope. No full
assistive-technology or scientific/DIN revalidation is claimed by the reviewer.
FL11 discrepancies and documented model limitations remain as above.

The accepted German preview is `http://127.0.0.1:7437/`; English is
`http://127.0.0.1:7436/`. The source package and installed package remain
3,165,642 and 4,082,811 bytes, respectively. No runtime edit followed this
acceptance. The follow-up automation remains paused; no commit, push,
integration, deployment or CRAN submission is authorized or performed.

The reviewer report is also retained verbatim in
`tests/verification/material-ux-B08-reviewer-2026-09-28.md`.
The accepted German build was opened in a fresh deliverable browser tab;
its visible B08 identity was verified. Existing user tabs were preserved.
The durable DIN comparison HTML was opened in the coordinator's file panel
(queued until this chat is shown). The saved automation status was verified
as PAUSED. No new review polling or scheduled task was introduced.

## B11 implementation and release for independent review, 2026-09-29

Candidate `MAT-UX-B11-b06d34913f5d`, SHA-256
`b06d34913f5d7fb5e1a2f423cc97bc21789772c36e8f97413b6f6809f100fa59`.
English http://127.0.0.1:7444/ and German http://127.0.0.1:7445/.
B09/B10 were internal candidates and never independently released. The
frozen B11 application is `/private/tmp/spectran-ux-review/B11/Spectran`.

User scope C02 removes the second cumulative actual-outcome table, makes
coefficient results first/default, puts photopic illuminance plus irradiances
in Light and radiation, and separates five EDI rows from the DER group.
The normative effective MDER remains with its DIN reference. Current,
archived, CSV, PNG and combined exports use the same grouping. Direct entry
without a source activates CIE D65 at 100 lx with an explicit basis notice.
Existing sources, including zero, are preserved. C03 adds visible collapsed/
expanded arrows to the native colour-method disclosure. C04 initializes the
material server on its first visit, retains it afterwards and keeps import
consent/promotion/restore connected through stable reactive interfaces.

Full R CMD check passes with 2,227 assertions, no failures, warnings or skips,
and zero errors, warnings or notes. The earlier B09 check exposed three old
UI expectations for changed navigation/wording; those were updated. The B11
integrated test exercises delayed creation, repeated navigation without a
new instance, D65 normalization, promotion, restore, retained branch nodes,
import cancellation and confirmed replacement. No scientific method or
bundled data file changed from accepted B08. Scoped `git diff --check` passes;
unrelated existing renv/activate.R whitespace was preserved.

All four package-size gates pass; see cran-size-2026-09-27.md. Frozen runtime
hashes were checked against the built archive. Check logs and source archive
are retained in the B11 review directory. Startup measurements and their
limitations are recorded in startup-2026-09-29/README.md (original comparison)
and startup-lazy-2026-09-29/README.md (implemented optimization).

Author browser checks establish a direct D65 journey and visible ▶/▼
colour-method arrows with mouse/Space operation in B11. The coefficient-first
tab was exercised in the internal B09/B10 workflow and is covered by B11 R tests.
The previously reused B10 port served a cached older stylesheet; B11 uses
fresh ports and the current rules were verified. The independent reviewer
received the complete B11 revision and updated contract, including expanded
EDI/DER export, first-use, state/consent and six-width regression checks.
Independent acceptance is pending. No preview handoff, commit, push,
integration, deployment or CRAN submission is authorized by these gates.
Scheduled review checks remain paused; reviewer completion notification is
user-authorized and event-driven.

## B11 independent result and B12 focused revision, 2026-09-29

The reviewer directly reported completion in the authorized coordination
workflow. B11 C02/C03 and visible C04 journeys pass, with no new functional
acceptance defect. Evidence includes fresh D65/direct and import-first/zero
journeys, promotion/restore/branches and import consent, seven actual bundles,
16 visually inspected PNGs and R-based CSV/audit structural checks. The six
widths were sampled in the EN zero state, with German desktop checks. These
passes do not imply every locale/state/width combination or new numerical
DIN adjudication. Full report: material-ux-B11-reviewer-2026-09-29.md.

The reviewer also conveyed the user's explicit C05 wording request. B12,
`MAT-UX-B12-b1cf8a553443`, SHA-256
`b1cf8a5534439abb9b84a20bfc5ead8260e4ea5e1dbc6bae6cced3948fe886a4`,
changes exactly the EN/DE cumulative title literals in R/material_language.R
to “Combined material effect (F = 1)” and “Gesamte Materialwirkung (F = 1)”.
Manifest comparison confirms that every other frozen entry is unchanged.
Focused cumulative-UI and language tests pass without failures/errors/warnings;
explicit R string checks, fresh build/install and size checks pass. All 97
shipped checked entries match the frozen manifest. The full-suite baseline
remains B11's clean 2,227-assertion R CMD check; it was not repeated for two
wording literals. No scientific or eligibility calculation changed.

B12 runs at English http://127.0.0.1:7446/ and German
http://127.0.0.1:7447/. The full revision notice requested a focused independent
C05 title and scaling-suppression/recovery check, retaining B11 passes and
residual risks. Final handoff is still withheld pending that decision.

## B12 independent acceptance and user handoff, 2026-09-29

The independent reviewer notified the coordinator directly and accepted
`MAT-UX-B12-b1cf8a553443` for final user review only.
SHA-256: `b1cf8a5534439abb9b84a20bfc5ead8260e4ea5e1dbc6bae6cced3948fe886a4`.
C05 is closed by visible evidence in fresh English and German sessions.
No open acceptance finding remains. Durable report:
`material-ux-B12-reviewer-2026-09-29.md`; screenshot:
`material-ux-B12-final-preview-2026-09-29.png`.

In each language, direct D65 100 lx → G1 → unscaled N2 shows the exact shortened
title and cumulative CSV. A second G1 step explicitly promoted at 200 lx as N3
hides the cumulative table/CSV and shows the scaling/recovery explanation while
preserving its individual archive. Showing N2 restores the shortened table and
CSV without replacing active N3 or deleting the branch. All stages pass.

B11 C02/C03 and observable C04 passes remain protected, along with C01 and
R01/R02/R04/R05/P01/R06/R07; R03 remains withdrawn. B11 is the full-suite R
baseline (2,227 assertions; clean R CMD check). B12 has passing targeted tests,
explicit wording checks, a new build/install, archive/manifest equality and
all four size gates. There were no further runtime changes. A final comparison
confirms that the author workspace matches every accepted frozen-file entry.

The accepted German preview is http://127.0.0.1:7447/ and English is
http://127.0.0.1:7446/. The reviewer retained the German demonstration at
recovered N2 while scaled N3 remains active. The coordinator opens the accepted
preview for the user, preserving the earlier user previews.

Residual boundaries are inherited explicitly: desktop-first narrow layouts;
no exhaustive browser or assistive-technology coverage; no repeated full
responsive/download matrix for two changed strings; intermittent browser-
controller file-transfer delays; and no independent numerical/DIN audit in this
UX review. The existing numerical reference report and its FL11 discrepancies
remain unchanged. No new residual limitation was found in B12.

Review coordination used direct completion notifications. No scheduled query
or automation was created or resumed. No commit, push, integration, deployment
or CRAN submission was performed or authorized by acceptance.

## Explicit user acceptance and development wrap-up, 2026-09-29

After the B12 handoff, the user explicitly instructed: “abgenommen. wrappe den
entwicklungsstand und commite”. The original user message was verified in the
Independent Reviewer chat. This authorizes completion documentation and a local
commit of the accepted development scope. It supersedes the preceding
review-only boundary for these actions; it does not authorize a push,
deployment, integration or CRAN submission.

The durable B12 reviewer report now includes that user decision. The accepted
109-entry manifest is preserved in `material-ux-B12-manifest-2026-09-29.json`.
Every entry matches the author workspace. The sole excluded runtime edit is a
pre-existing removal of a trailing blank line in `R/runApp.R`; R parsing confirms
identical expressions between HEAD and the accepted file. All other runtime
changes belong to the accepted development scope. No runtime edit was made
during this wrap-up.

`material-development-wrap-up-2026-09-29.md` records the scope, retained checks,
package sizes, performance evidence, limitations and deliberately excluded
local changes. The benchmark records and review evidence are versioned as
verification material and remain excluded from the source package by
`.Rbuildignore`. The existing clean B11 full check and B12 focused checks are
retained; no full rerun is needed for these excluded documentation additions.
