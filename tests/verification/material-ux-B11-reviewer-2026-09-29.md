# B11 independent UX review

Candidate: `MAT-UX-B11-b06d34913f5d`.
App SHA-256: `b06d34913f5d7fb5e1a2f423cc97bc21789772c36e8f97413b6f6809f100fa59`.
English: http://127.0.0.1:7444/; German: http://127.0.0.1:7445/.
Frozen contract and revision: `../contract.md`, `../B11/revision.md`.

**Decision: C02, C03 and the visible C04 journeys pass. One user-directed wording revision, C05, remains before final handoff.** No new functional acceptance defect was found in the tested scope. B08 remains the last independently accepted candidate. This report does not authorize implementation integration, commit, push, deployment or submission.

## C05: remove the redundant cumulative-title qualifier

Status: open user requirement, wording only. The user explicitly requested this during B11 review.

Reproduction: create an unscaled material step, promote it, open History and inspect the cumulative table. Observed in both languages, including EN N4 and DE N2:

- EN: `Combined material effect (F = 1, excludes illuminance overrides)`.
- DE: `Gesamte Materialwirkung (F = 1, ohne Beleuchtungsstärke-Skalierungen)`.

Expected:

- EN: `Combined material effect (F = 1)`.
- DE: `Gesamte Materialwirkung (F = 1)`.

The qualifier adds unnecessary text because scaled paths already suppress the table. Keep F = 1, table/CSV eligibility, the unavailable-state explanation and recovery behavior. No numerical change is requested. Evidence: `B11-en-unscaled-sibling.txt`, `B11-de-history.txt`, `B11-en-import-cancel-preserved.txt`.

Please return a frozen revision identifying C05 and its regression scope. A focused EN/DE title check plus scaled suppression and unscaled recovery is sufficient if there are no additional runtime changes. Protect the passes below.

## Meaningful passes

| Area | Visible evidence and tested conditions |
| --- | --- |
| Direct first entry | Fresh EN Introduction exposes Materials. First direct visit creates explicitly labelled automatic D65 at 100 lx. Default G1 applies coherently. |
| Import before first entry | Fresh DE imports built-in Halogen at 100 lx before Materials; the source is retained without an automatic-source notice. A separate fresh EN session imports the built-in incandescent source at verified 0 lx before any material visit. General photometry visibly shows 0 lx; G1 preserves this source. |
| Current/archive tables | Coefficients appear first/default, then Light values / Licht und Strahlung, then Alpha-opic / alpha-opisch. Current G1/S1/G11 and archived L1/S1 sampled. Light tables start with photopic illuminance and contain total plus five alpha-opic irradiances, seven rows altogether. EDI is in the alpha table: five lx rows followed by a separate group of five DER rows and effective MDER. Denominator and DIN reference notes remain. |
| Zero definitions | With the verified zero source, EDI values display zero; DER and undefined ratios are labelled Undefined with visible guidance. D65 coefficients remain available. Transmission has no colour card. `B11-zero-import-photometry.txt`, `B11-zero-coefficient-result.txt`, `B11-zero-alpha.txt`. |
| One cumulative comparison | Unscaled paths show one cumulative material-effect table and CSV. Separate archived single-step results remain. N3, promoted with a verified 200 lx override, suppresses cumulative table/CSV and explains recovery. An ancestor recovers availability; restored N1 and a new unscaled N4 sibling preserve N2/N3 and recover the complete comparison. |
| Navigation and consent | Introduction/Import/Materials revisits preserve source, draft and history. Automatic D65 notice persists through promotion and restore. Import cancellation preserves active N4 and all four nodes. Confirmed explicit incandescent import leaves only visible N1 and clears the automatic notice; subsequent navigation retains it. `B11-en-import-confirmation.txt`, `B11-en-import-cancel-preserved.txt`, `B11-en-import-confirmed.txt`, `B11-en-explicit-source-revisit.txt`. |
| Disclosure arrow | Right/down arrows track collapsed/expanded state. DE input mouse, Enter/Space, forward/reverse Tab and visible focus tested; normalization/current/archive sampled. EN input and normalization keyboard use, plus 390-pixel input expansion/collapse and focus, pass. D65-only appearance and Y remain. |
| Incomplete input recovery | Existing partial synthetic reflection CSV suppresses colour and Apply with specific lower/upper-tail guidance. Choosing lower-tail carry and upper-tail full reflectance restores the D65 card and Apply, and produces a current reflection result. `B11-partial-blocked.txt`, `B11-partial-recovered.txt`, `B11-partial-applied.txt`. |
| Round trip | Actual G1 completed-filter CSV from the downloaded bundle is uploaded through the material chooser, recognized as a complete 401-row curve, with Ready, D65 preview and Apply available in reflection mode. `B11-roundtrip-ready.txt`. This is structural UI recovery, not a numerical equivalence claim. |

C04 is accepted for observable first-use and persistent-session behavior. This review makes no claim to have inspected internal module registration or independently measured startup speed.

## Downloads and visual export inspection

Seven actual bundles are recorded in `B11-downloads.tsv`: EN G1 normal/compact, DE S1 normal/compact, EN archived scaled-node L1 compact, DE G11 with the long promoted source name and material panel compact, and DE archived S1 selected while the different G11 draft/result is active. Normal settings are 9 x 5.8 inches / font 15; compact settings are 8 x 4 / font 11. The combined table was explicitly set to Alpha-opic.

Sixteen actual exported PNGs were visually inspected:

- EN G1 normal: light, alpha and combined alpha; compact: alpha and combined alpha.
- DE S1 normal: light, alpha and combined alpha; compact: alpha and combined alpha.
- EN archived L1 compact: alpha and combined alpha.
- DE G11 long-title compact: result plot and combined alpha, with Melanopsin, V(lambda), incident overlay and material panel.
- DE archived S1 after G11: alpha and combined alpha.

These show the complete last DER/effective-MDER rows, units and notes without clipping. The long G11 title wraps and preserves panel labels. Archived S1 exports retain the S1/Halogen result and reflection terminology despite the active G11 transmission draft. The current plot options are used as the export UI explains. Other bundle PNGs were not all visually inspected.

`B11-export-schema.R` inspects CSV field names, row labels/order, units, metric groups, comparison labels and audit inventories using R 4.6.1 and utils 4.6.1. Inputs are the seven bundle paths in the manifest and `B11-en-cumulative.csv`. Command:

```sh
Rscript --vanilla /private/tmp/spectran-ux-review/reviewer/B11-export-schema.R > /private/tmp/spectran-ux-review/reviewer/B11-export-schema.txt
```

CSV grouping matches the visible tables. The unscaled cumulative CSV contains `material_only_f1`; the scaled N3 audit uses `not_available_after_illuminance_rescaling` with an explanation, while individual snapshots and spectra remain. No scientific values, DIN validity or numerical equivalence were independently recalculated or adjudicated.

## Responsive and keyboard boundary

The fresh EN zero-result alpha table was inspected at 1440, 1200, 1024 and 768 CSS-pixel production widths, 390-pixel usable core and 320-pixel severe-failure width. Production layouts retained tables and controls without page overflow. Narrow tables scroll locally; horizontal scrolling to the right-hand changes/units was exercised at 390. At 320, the continuation/back actions remain reachable and stacked. Long scrolling and local table scrolling are expected for this desktop-first app. The reflection disclosure was additionally operated at 390, with its text and focus visible; a 1024 native screenshot confirms the normal input layout.

The six-width matrix was sampled in EN, not repeated for every German state. German current/archive content and disclosure controls were exercised at the normal desktop viewport. Evidence: `B11-responsive-en-*.png`, the DE input screenshots and text snapshots. Some full-page overview captures have controller scaling/padding; the native 1024 and narrow viewport captures show the actual control layout. The temporary viewport override was reset.

## Protected passes and residual limits

R01/R02/R04/R05/P01/R06/R07 remain protected and closed; R03 remains withdrawn. C01 remains protected. R07's long-title/panel-label export and C01's D65-only/gating behavior were sampled again successfully. The full historical invalid-input matrix, every TUB material, all browser combinations and assistive technologies were not repeated. Large-gap recovery remains a protected earlier pass, not a new B11 test. The unresolved earlier question about Y versus X/Z is not an instruction to add or remove those quantities.

File transactions through the browser controller had severe intermittent delays, including roughly 23 minutes for one archive download and 60 minutes for one return-upload call despite short requested timeouts. They eventually completed; these tool timings are not app calculation-time measurements. Immediate Shiny snapshots could retain prior output and were replaced with settled visible evidence. Several background mouse actions had no effect; keyboard operation completed those actions. No application crash is inferred from these controller interruptions.

Programmer verification in `B11/revision.md` reports 2,227 assertions with no failures/warnings/skips, R CMD check 0 errors/0 warnings/0 notes, all four size budgets below 5,000,000 bytes, and archive agreement with 97 frozen manifest entries. This evidence was read, not independently rerun by the UX reviewer.

Only review artifacts and contract notes were edited. The frozen app, implementation, configuration and fixtures were not modified. No scheduled follow-up was created. The next gate remains user final review of a subsequently accepted frozen candidate after C05.

Communication and cleanup: the coherent decision and C05 request were sent successfully to Programmer (`01a0e212-10ba-73f0-ab12-2837d984ab1a`, local). Review-only DE and zero-source tabs were closed. Existing 7428/7435/7437 previews were preserved, and the already visible B11 EN preview remains open for reference with an ordinary catalogue selection. Its presence is not a final acceptance handoff.
