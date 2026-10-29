# B12 focused independent UX review

**Accepted for final user review. MAT-UX-C05 is closed by visible evidence.** No open acceptance finding remains in this focused revision. This decision permits final user review only, not commit, push, integration, deployment or CRAN submission.

Candidate: `MAT-UX-B12-b1cf8a553443`.
SHA-256: `b1cf8a5534439abb9b84a20bfc5ead8260e4ea5e1dbc6bae6cced3948fe886a4`.
English: http://127.0.0.1:7446/.
German: http://127.0.0.1:7447/.
Frozen revision and contract: `../B12/revision.md`, `../contract.md`.

## Scope and identity

The agreed retest covers the two shortened cumulative titles, suppression of table/CSV after illuminance scaling, and recovery on an unscaled path in both languages. The B11 passes and recorded limits remain protected.

Both fresh browser sessions visibly display the B12 build banner. A read-only comparison of B11/B12 manifest entries identifies only `R/material_language.R` as changed. The programmer reports exactly two changed string literals. An initial copy error in the full hash in revision.md was corrected by the programmer to match manifest.json before acceptance. Build ID, URLs and frozen runtime remained unchanged.

## Reproduction and results

The same visible journey was completed independently in English and German:

1. Open fresh Introduction, enter Materials directly, apply default G1 to the automatic D65 100 lx source, and promote with the untouched default illuminance. This creates unscaled N2 from N1.
2. Open History. One cumulative table is visible, with exactly `Combined material effect (F = 1)` or `Gesamte Materialwirkung (F = 1)`. The old qualifier is absent. The cumulative CSV link is available. Screenshots `B12-en-title.png` and `B12-de-title.png` show the rendered titles.
3. Apply G1 once more. On Promotion, verify the default second-step value, then explicitly enter 200 lx and verify it in a separate visible state before promotion. The new scaled node is N3, with parent N2.
4. Open History for active N3. The cumulative table and CSV link are absent. The existing explanation states why cumulative metrics are unavailable and tells the user to select a pre-scaling or unscaled path. The archived individual material result remains available.
5. Select Show N2 / Anzeigen N2 while N3 remains the active source. The shortened title, cumulative table and CSV link return. N3 remains in the history. This establishes recovery without deleting or resetting the scaled path.

All five stages passed in both languages. Displayed illuminance values were used as UI state identifiers; no scientific result was independently calculated or adjudicated.

Evidence in this directory:

- `B12-en-unscaled.txt`, `B12-de-unscaled.txt` and the corresponding title screenshots.
- `B12-en-scaled-input.txt`, `B12-de-scaled-input.txt` for the verified explicit 200 lx input.
- `B12-en-scaled.txt`, `B12-de-scaled.txt` and corresponding screenshots for suppression and recovery guidance.
- `B12-en-recovered.txt`, `B12-de-recovered.txt` for the restored table/CSV while N3 remains active.

## Protected passes, gates and limits

C02/C03 and the visible C04 journeys passed in B11; C01 and R01/R02/R04/R05/P01/R06/R07 remain closed and protected. R03 remains withdrawn. C05 is now closed. No protected finding was reopened.

The B11 report remains the evidence boundary for the wider input, keyboard, responsive and export matrices. Those matrices and actual file transactions were not repeated for these two strings. The inherited limitations include desktop-first narrow layouts, no comprehensive assistive-technology/browser coverage, intermittent controller file-transfer delays, and no independent numerical/DIN adjudication. No new residual limitation arose in this focused pass. Immediate Shiny snapshots sometimes showed earlier output; final decisions use settled node labels and visible tables.

Programmer evidence in B12/revision.md reports passing targeted cumulative/language tests, explicit title checks, fresh build/install, matching shipped manifest entries and all four 5 MB budgets. B11's clean full R CMD check with 2,227 assertions remains the full-suite baseline. These R/package gates were read, not rerun by the UX reviewer.

Only review notes and the review contract were edited. No app source, configuration or fixture was modified. No viewport override was applied in B12 and no scheduled follow-up was created.

Communication and cleanup: Programmer was successfully notified of acceptance, C05 closure and the inherited residual limits before user handoff. The temporary EN review tab was closed. The accepted DE tab is retained for final user review; older user-facing previews remain preserved. `B12-final-preview.png` shows recovered N2 with the shortened title while scaled N3 remains active.

Subsequent user decision: the user explicitly accepted B12 and requested development wrap-up and a local commit. The instruction was delivered to Programmer. The earlier review-only gate is superseded by that explicit authorization for wrap-up and commit; no push or deployment was requested.
