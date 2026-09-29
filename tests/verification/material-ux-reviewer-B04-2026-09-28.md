# B04 acceptance report

Decision: **accepted for user final review**. MAT-UX-R05 is closed by visible retesting and the downloaded audit artifact. No production-bound acceptance finding remains open. This decision authorizes only the next gate in the review contract, after the coordinator receives this report.

Candidate: **MAT-UX-B04-9d8c658003a1**. Application SHA-256: `9d8c658003a17589dbc6cc12b5502b1150eedbb18825c7e457c48f30a4b3c70a`. English: http://127.0.0.1:7428/. German: http://127.0.0.1:7429/. Frozen application: `/private/tmp/spectran-ux-review/B04/Spectran`. Review completed on 2026-09-28.

The visible English and German banners identified B04. After an environment interruption, all 108 manifest-listed files were checked against their SHA-256 entries with no mismatch, and the same build was restarted at the user's request. The English banner was verified again in a fresh session. No application source, configuration or fixture was changed.

## MAT-UX-R05: closed

The original reproduction used the built-in 6500 K daylight source at 100 lx and `material-partial-fraction.csv`, explicitly interpreted as Percent with a custom name. The same small coefficients remain valid-looking on either offered scale, so this tests retention independently of range rejection. No scientific comparison of those interpretations was calculated.

Both interaction-switch directions were exercised in both languages with Fraction and Percent. The custom name and selected scale remained visible after the switch. Edited geometry and angle also persisted. Selected zero/full tail decisions remained associated with the upload while their labels changed with the interaction. The low-percent preview remained visually consistent through the original German switch, and the settled readiness feedback agreed with the remaining decisions.

Evidence: `R05-B04-de-percent-before.png` and `R05-B04-de-percent-after.png` show the retained custom name, Percent selection and measurement details across Reflexion to Transmission. The English fresh-session repeat is recorded in `B04-en-recovered-after-switch.png` and `B04-en-reflection-recovery.txt`.

Application succeeded after switching: German reflection before the interruption, and English transmission in the fresh session afterwards. The German qualified/scattering result was applied after renewing both acknowledgements at the 390-pixel test width. Its downloaded audit archive is retained as `B04-de-reflection-audit.zip`; the original browser download was `/Users/zauner/Downloads/b04-prozentdaten-audit.zip`.

Structural/text inspection of `metadata/applied-metadata.csv` confirmed the applied snapshot records `reflection`, name `B04 Prozentdaten`, scale `percent`, type `internal`, scattering `yes`, geometry `8°/diffus (synthetischer Test)`, angle `8°`, and both acknowledgements `TRUE`. `metadata/decisions-warnings.csv` records lower tail `zero`, upper tail `one`, and the appropriate English and German reflection descriptions. This verifies saved metadata, not numerical scientific correctness. An additional English audit download was not repeated; the German artifact covers the requested post-switch audit check.

## Adjacent recovery and reset checks

- **Acknowledgement renewal:** changing interaction cleared qualification and scattering acknowledgements while retaining the entered measurement definition, scattering choice, geometry and angle. Readiness identified the missing confirmations. German recovery reached a successful applied reflection result. In the fresh English session, native Space activation renewed each checkbox after Transmission to Reflection and after Reflection to Transmission. Focus remained on the checkbox; Tab moved to the preview disclosure and Shift-Tab returned. The resulting transmission draft applied successfully.
- **Replacement upload:** after a qualified, scattering, custom-named Percent draft, replacing the file with `material-neutral-fraction.csv` produced the new filename-based name, Fraction, total hemispherical reflectance, scattering No, empty geometry and angle, and an available Apply action without obsolete tail requirements. Evidence: `B04-en-replacement-defaults.png`.
- **Catalogue reset:** returning to the TUB catalogue restored catalogue metadata, first for reflection L1 and then transmission G1. G1 displayed its own name, Fraction, total transmittance, scattering No and source measurement geometry. It applied successfully. Evidence: `B04-en-catalogue-reset.txt` and `B04-showcase-restarted.png`.
- **Responsive sample:** the German Details/recovery flow was exercised at the requested 1200-pixel production width and 390-pixel usable-core width. At 390, the long form and normalization controls remained reachable by vertical scrolling; acknowledgement recovery and Apply reached Results. `B04-de-390-ready.png` records the narrow normalization view. English keyboard and applied-result checks were also repeated at the normal desktop viewport after restart. The temporary viewport override was reset. The unaffected full six-width matrix is carried forward, rather than claimed as rerun on B04.

## Closed findings and carried-forward evidence

| ID | B04 status |
| --- | --- |
| MAT-UX-R01 | Protected closed pass. Mode-appropriate selected tail wording was sampled again in both languages; German audit tail descriptions also agree. |
| MAT-UX-R02 | Protected closed pass. B04 sampled the German current reflection export and audit. The B02 current/archived mixed-mode export matrix was not replayed. |
| MAT-UX-R03 | Remains withdrawn. No new evidence challenges the earlier full-resolution reassessment. |
| MAT-UX-R04 | Protected closed pass. Localized upload receipt and material-coefficient wording remain consistent in the sampled upload paths. |
| MAT-UX-R05 | Closed by the retention, recovery, application and audit checks above. |
| MAT-UX-P01 | Protected closed pass from B02. Compact long-title figure exports were not regenerated for this metadata-only revision. |

Carried forward without exhaustive replay: the B01/B02 catalogue inventory and representative materials, legacy catalogue access, receiver limits and F=1 explanations, malformed/gap/metadata recovery, promotion and branching, both cumulative comparisons, German decimal-comma Percent import, completed-CSV round trip, archived exports, and the 1440/1200/1024/768 production, 390 usable-core and 320 severe-failure observations. B04 changed only `R/transmission.R` according to its revision notice.

## Environment interruptions and evidence limits

Early B04 acknowledgement attempts produced ambiguous transient states, failed controller actions and stale accessibility targets while the controls refreshed. The untouched CSV-header checkbox responded to native Space and restored header parsing. The initial observations did not establish an application defect or an acknowledgement pass. The fresh-session keyboard recovery and subsequent application described above supplied the missing positive evidence; no new finding ID is opened for the ambiguous attempts.

After observing that symptom, a limited, read-only diagnostic inspection of the frozen metadata and acknowledgement render code was performed. It suggested a possible refresh/focus lead but did not establish a cause. No acceptance conclusion relies on source code, reactive internals, injected browser state or independent calculations.

The original English upload tool returned after about 1,610 seconds. The German audit download tool eventually returned the completed file after about 32,483 seconds, without an explicit error or cause. When the user later requested a showcase restart, neither port was listening. No evidence establishes why those processes stopped or attributes the transfer delay to the application. These are recorded as environment interruptions. Saved screenshots and the completed audit artifact remain valid for the unchanged candidate.

Restart commands used the existing `B04/run-English.R` and `B04/run-Deutsch.R` launchers with `Rscript --vanilla`, after manifest verification. Sandbox startup was denied at the socket-binding step; approved launches then listened on 127.0.0.1 ports 7428 and 7429. The resumed English source import, fixture upload, recovery, application and reset checks used a fresh session. The English showcase is left open on the built-in G1 applied result, as requested by the restart task. The temporary German review tab is closed; both servers remain running.

No independent scientific, full accessibility or CRAN compliance determination is made. The programmer-reported 1,561 assertions, clean R CMD check, four package-size budgets and byte-identical packaged data were not independently re-audited. Accepted modeling boundaries remain unchanged: explicit F=1 receiver scenarios, no room geometry/BRDF/automatic interreflection simulation, documented FL11 reference differences and TUB source-table inconsistencies, and session-only histories with audit exports for persistence.

## Handoff

The exact candidate above is ready for the user's final review. The coordinator chat `01a0e212-10ba-73f0-ab12-2837d984ab1a`, host `local`, was successfully notified on 2026-09-28 with this decision, report path and residual boundary under the user's completion-notification instruction. Scheduled checks remain paused. This review does not authorize commit, push, integration, submission, deployment or any later milestone.
