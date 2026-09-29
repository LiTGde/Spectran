# Spectran B08 independent UX review

**Decision: accepted for the user's final review.** MAT-UX-R07 and MAT-UX-C01 are closed by independent visible-browser evidence. The compact German export no longer overlaps panel A and the vertical axis label. Reflection now shows one approximate material-colour card under D65 throughout the requested views. No remaining acceptance defect was found in the changed material workflow. This decision does not authorize integration, commit, push, deployment or CRAN submission.

Candidate: **MAT-UX-B08-4d3bf2c8519b**. Application SHA-256 from the frozen revision contract: `4d3bf2c8519b287bc0d0ba9961695b22342bfdd20d9752ffb3ad9bbe9958b6cc`. Visible build identities verified in independent English and German sessions at http://127.0.0.1:7436/ and http://127.0.0.1:7437/.

Review completed 2026-09-28. Scope: [B08 revision](/private/tmp/spectran-ux-review/B08/revision.md), [review contract](/private/tmp/spectran-ux-review/contract.md), and the protected passes in [B07](/private/tmp/spectran-ux-review/reviewer/B07-report.md). Method: visible UI operation under the active [review-shiny-ux skill](/Users/zauner/.codex/skills/review-shiny-ux/SKILL.md) and visual inspection of actual app downloads. No source, configuration or fixture edits, injected application state, scientific recalculation or substitution of author-generated figures.

## Finding decisions

| ID | Decision | Independent evidence |
| --- | --- | --- |
| MAT-UX-R07 | Closed | Exact Halogen 100 lx → S1 Reflection → unscaled promotion with the specified long German name → G11 Transmission. The compact standalone figure separates A from the complete two-line quantity/unit title. All 16 German export variants pass. |
| MAT-UX-C01 | Closed | One D65 colour card in input, normalization, applied and archived results in both languages. No incident-light colour card remains in those settled views. Changed and zero-illuminance sources do not remove D65. Archives retain their material colour with a different draft and active source. |
| MAT-UX-P01 | Protected and sampled, remains closed | Shared title/subtitle, panel tags and panel headings remain separate in all 20 downloaded figures, including the long English literal accumulated title. |
| MAT-UX-R06 | Protected and sampled, remains closed | Incomplete tails and an unacknowledged internal gap retain construction plots and actionable guidance. Completing them restores D65 and Apply. Invalid data are rejected; a valid replacement recovers. |
| MAT-UX-R01/R02/R04/R05 | Protected passes | No contradiction in the adjacent paths exercised. A full replay was not required or represented as completed. |
| MAT-UX-R03 | Withdrawn, unchanged | Reduced preview rendering is not evidence of original-file cropping. |

Y remains displayed. The user has not instructed adding X/Z or removing Y. The earlier request to align two swatches is superseded by the single-D65-card decision. The numerical incident-spectrum/D65 coefficient table remains a separate feature.

## Actual export matrix

Twenty PNGs, two per app-generated ZIP bundle, were downloaded and visually inspected. Each pair contains a standalone result plot and a result plot with the coefficient table. All pass for complete titles, subtitles, axes/units, panel tags, table rows, source text, footnotes and watermarks. This is layout inspection, not numerical adjudication.

| Result | Size in inches / font | Coefficient panel | Evidence directory | Outcome |
| --- | --- | --- | --- | --- |
| Current DE G11 | 9 × 5.8 / 15 | On | [default panel](/private/tmp/spectran-ux-review/reviewer/B08-de-current-default-panel) | Both pass |
| Current DE G11 | 8 × 4 / 11 | On | [compact panel](/private/tmp/spectran-ux-review/reviewer/B08-de-current-compact-panel) | Both pass, exact R07 reproduction |
| Current DE G11 | 9 × 5.8 / 15 | Off | [default without panel](/private/tmp/spectran-ux-review/reviewer/B08-de-current-default-no-panel) | Both pass |
| Current DE G11 | 8 × 4 / 11 | Off | [compact without panel](/private/tmp/spectran-ux-review/reviewer/B08-de-current-compact-no-panel) | Both pass |
| Archived DE S1 / N2 | 9 × 5.8 / 15 | On | [default panel](/private/tmp/spectran-ux-review/reviewer/B08-de-archive-default-panel) | Both pass |
| Archived DE S1 / N2 | 8 × 4 / 11 | On | [compact panel](/private/tmp/spectran-ux-review/reviewer/B08-de-archive-compact-panel) | Both pass |
| Archived DE S1 / N2 | 9 × 5.8 / 15 | Off | [default without panel](/private/tmp/spectran-ux-review/reviewer/B08-de-archive-default-no-panel) | Both pass |
| Archived DE S1 / N2 | 8 × 4 / 11 | Off | [compact without panel](/private/tmp/spectran-ux-review/reviewer/B08-de-archive-compact-no-panel) | Both pass |
| Current EN repeated Reflection | 9 × 5.8 / 15 | On | [long literal name, default](/private/tmp/spectran-ux-review/reviewer/B08-en-literal-default-panel) | Both pass |
| Current EN repeated Reflection | 8 × 4 / 11 | On | [long literal name, compact](/private/tmp/spectran-ux-review/reviewer/B08-en-literal-compact-panel) | Both pass |

The exact German promotion name was `Warmweißes Halogenlicht nach Backstein gelb für den Export mit ausführlicher Quellenbezeichnung`. Archived S1 figures keep `Halogen-/Glühlampenlicht: Halogen` as the incident source and retain Reflection/exitance wording while the current material is G11 Transmission.

The English journey used built-in D65 6500 K at 100 lx and the completed partial fixture, named `[B08] golden *sample* _matte_ & measured surface with a deliberately long descriptive material name`. Applying it, promoting without rescaling and applying again produces the long accumulated title. Literal punctuation and the full source description survive wrapping.

The [download manifest](/private/tmp/spectran-ux-review/reviewer/B08-downloads.tsv) maps each case to its browser-returned ZIP and extracted evidence directory. ZIP extraction and file inventories were non-analytical infrastructure operations.

## D65-only preview and recovery

| Test | Result and evidence |
| --- | --- |
| German input, normalization, applied result | S1 consistently shows one D65 card, visible `#C4A16E` and Y 38.3%, with the approximation note. [Input and method](/private/tmp/spectran-ux-review/reviewer/B08-de-input.txt), [normalization](/private/tmp/spectran-ux-review/reviewer/B08-de-normalization.txt), [applied result](/private/tmp/spectran-ux-review/reviewer/B08-de-applied.txt), [screenshot](/private/tmp/spectran-ux-review/reviewer/B08-de-d65-method.png). |
| English input, normalization, applied result | Completed partial curve consistently shows one D65 card, visible `#DBB47C` and Y 49.2%. [Settled input after source change](/private/tmp/spectran-ux-review/reviewer/B08-en-after-source-change.txt), [normalization and method](/private/tmp/spectran-ux-review/reviewer/B08-en-completed-normalization.txt), [applied result](/private/tmp/spectran-ux-review/reviewer/B08-en-applied.txt). The earlier immediately captured completed-input file is not used as settled-state evidence. |
| German zero source | `B08 Quelle mit 0 lx` is active; L1 retains its single D65 card, visible `#E9EAE5` and Y 81.9%, in [input](/private/tmp/spectran-ux-review/reviewer/B08-de-zero-input.txt), [normalization](/private/tmp/spectran-ux-review/reviewer/B08-de-zero-normalization.txt) and [applied results](/private/tmp/spectran-ux-review/reviewer/B08-de-zero-applied.txt). The [coefficient table](/private/tmp/spectran-ux-review/reviewer/B08-de-zero-coefficients.txt) retains all six D65 values and marks incident-dependent values `Nicht definiert`. |
| English zero source | The active N4 source has visibly verified 0 lx. [Source](/private/tmp/spectran-ux-review/reviewer/B08-en-zero-source.txt), [input](/private/tmp/spectran-ux-review/reviewer/B08-en-zero-input.txt) and [applied result](/private/tmp/spectran-ux-review/reviewer/B08-en-zero-applied.txt) preserve D65 and give understandable Undefined explanations for affected material metrics. |
| German frozen archive | Showing N2 retains S1's single `#C4A16E`/Y 38.3% card while N3 is the active zero source and L1 is the different current material. [Evidence](/private/tmp/spectran-ux-review/reviewer/B08-de-archive-after-zero-source-and-l1-draft.txt). |
| English frozen archive | Showing N2 retains the original single `#DBB47C`/Y 49.2% card while N4 is the active zero source and the gap fixture is the different current draft (`#C2DEDA`/Y 69.0% when completed). [Evidence](/private/tmp/spectran-ux-review/reviewer/B08-en-archive-after-zero-source-and-gap-draft.txt). |
| Transmission | Settled DE G11 [input](/private/tmp/spectran-ux-review/reviewer/B08-de-transmission-input.txt) and [result](/private/tmp/spectran-ux-review/reviewer/B08-de-transmission-result.txt), plus [EN Transmission input](/private/tmp/spectran-ux-review/reviewer/B08-en-transmission-no-colour.txt), contain no Reflection colour card. |
| Missing tails | EN partial fixture retains plot and completion guidance with [two tails](/private/tmp/spectran-ux-review/reviewer/B08-en-partial-input.txt), in [normalization](/private/tmp/spectran-ux-review/reviewer/B08-en-partial-normalization.txt), and with [one remaining tail](/private/tmp/spectran-ux-review/reviewer/B08-en-one-tail-pending.txt). Carry-first/carry-last restores D65 and Apply. |
| Large internal gap | EN `material-large_gap-fraction.csv` requests acknowledgement of 500–525 nm, preserving its plot and completion note in [input](/private/tmp/spectran-ux-review/reviewer/B08-en-gap-input.txt) and [normalization](/private/tmp/spectran-ux-review/reviewer/B08-en-gap-normalization.txt). Space on the checkbox restores the single card and Apply. [Recovery](/private/tmp/spectran-ux-review/reviewer/B08-en-gap-recovered.txt). |
| Invalid replacement and recovery | EN Reflection rejects duplicate 400 nm rows 2/3 and the row 3 value 1.2 with specific correction guidance, without a colour card. [Invalid state](/private/tmp/spectran-ux-review/reviewer/B08-en-invalid.txt). Replacing it with the existing neutral Fraction fixture clears the errors and restores Ready, Apply and a single D65 card, visible Y 50.0%. [Recovery](/private/tmp/spectran-ux-review/reviewer/B08-en-valid-replacement.txt). |

These are displayed values used to identify UI states. They are not independently recalculated scientific results. All upload fixtures already existed under `/private/tmp/spectran-ux-review/fixtures`.

## Keyboard and responsive checks

Both language method disclosures work with Enter and show the D65-only explanation. Keyboard actions also covered completion controls, gap acknowledgement, mode/plot options, Apply, promotion, history Show and exports. English single-card layout was sampled at [768 × 1000](/private/tmp/spectran-ux-review/reviewer/B08-en-d65-768.png) and [390 × 900](/private/tmp/spectran-ux-review/reviewer/B08-en-d65-390.png) CSS pixels. The card, approximation text, disclosure and Apply remain reachable. Document client/scroll width at the narrow sample was 375/375. The temporary viewport override was reset.

The narrow construction-plot legend remains small and partly clipped within the existing desktop-first usable-core boundary; the card and actions remain accessible. No full screen-reader or assistive-technology claim is made. Broader six-width, branching/restoration/scaling, zero-reflectance and numerical-table/download coverage remains protected under [B06](/private/tmp/spectran-ux-review/reviewer/B06-report.md) and B07, not represented as a full B08 replay.

## Residual limits and execution notes

- Several browser-controller download/file-picker calls took hundreds or thousands of seconds, including roughly 1495, 254, 1332 and 2363 seconds. All required files eventually arrived. A download timeout reset the controller once; existing tabs were rebound without reloading the sessions. These are controller/environment observations, not evidence of application calculation time or an app crash.
- Immediate snapshots sometimes contained prior Shiny output. Decisions use settled semantic states. A rapid combined EN field-edit/promotion sequence did not establish the intended zero source; that N3 state was excluded as zero evidence. A subsequent separate field verification and promotion created N4, with 0 lx explicitly confirmed before the zero-source checks.
- A mouse checkbox action targeted an adjacent option during preparation. Visible state exposed it and keyboard correction preceded exports. This review does not claim successful mouse targeting for that action.
- Adjacent non-blocking observation: the general Analysis photometry view shows NA/NaN for some undefined zero-source quantities. The changed material views instead provide localized Undefined explanations. General Analysis presentation was outside the requested B08 changes, and the observation did not block the journey. This review does not establish whether it is a regression.
- Programmer release evidence reports R 4.6.1, 2,071 passing assertions, R CMD check with 0 errors/warnings/notes and all four package-size measures below 5,000,000 bytes. The UX reviewer did not rerun those gates.
- Numerical/DIN adjudication remains outside scope. The programmer's comparison report explicitly retains FL11 discrepancies, including 52 of 81 values exceeding its documented budget. No universal DIN agreement or normative certification is inferred from UX acceptance. Native reflected exitance, F = 1 receiver assumptions, session-local history and approximate screen-colour limitations remain.

## Completion and handoff

The report was published in the review chat and the authorized completion notification was successfully delivered to **Programmer**, chat `01a0e212-10ba-73f0-ab12-2837d984ab1a`, host `local`, on 2026-09-28. The notification included the exact candidate, decision, report path and residual limits. The decision permits the user's final visual review only. Scheduled checks remain paused. Y remains unchanged pending an explicit user decision.

The user's B04 showcase at http://127.0.0.1:7428/ and B07 DE tab at http://127.0.0.1:7435/ were preserved. B08 servers were not restarted or modified by this review. The temporary EN B08 reviewer tab was closed; DE B08 at http://127.0.0.1:7437/ remains open for final user review. All 45 local report links resolve, and the download manifest resolves to 10 bundles containing 20 distinct PNG files.
