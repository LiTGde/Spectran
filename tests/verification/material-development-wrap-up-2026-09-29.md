# Accepted material-module development state

The user accepted B12 on 2026-09-29 and explicitly requested development
wrap-up and a local commit. The instruction was verified in the original user
message in the Independent Reviewer chat
(`01a0e2a9-f5b8-7612-8fe1-b19d2dbc0b63`). No push, deployment, integration or
CRAN submission was requested.

Accepted candidate: `MAT-UX-B12-b1cf8a553443`.
Manifest SHA-256:
`b1cf8a5534439abb9b84a20bfc5ead8260e4ea5e1dbc6bae6cced3948fe886a4`.
The complete manifest is saved as `material-ux-B12-manifest-2026-09-29.json`.
Independent acceptance and the subsequent user decision are recorded in
`material-ux-B12-reviewer-2026-09-29.md`.

## Development scope

- All 55 public TUB material examples are the default catalogue, with stable
  identifiers, source provenance, DIN glazing descriptions and material aliases.
- Transmission and reflection share explicit receiver assumptions, F = 1 by
  default and optional illuminance scaling. Reflection retains its native
  radiant-exitance interpretation. Approximate material colours use D65.
- Results show source-weighted and D65 material coefficients, illuminance and
  irradiances, and separately grouped alpha-opic EDI and DER. The effective MDER
  has an explicit DIN/TS 67600 reference.
- Promotion and history are separate. Row actions select archived and
  cumulative results or restore a source. Immutable snapshots preserve branches.
  Cumulative material effects are suppressed after intermediate illuminance
  scaling, with guidance for recovering an eligible path.
- Direct material entry uses a clearly identified D65 source at 100 lx when
  no source has been imported. Existing imported sources, including zero-valued
  spectra, are preserved.
- The material server initializes once on its first visit. Later navigation
  reuses the same module and history. Plot/table exports, language changes,
  validation recovery and disclosure controls have independent review coverage.
- Public reference fixtures, R validation scripts and package-size gates
  accompany the implementation. Licensed DIN tables are not redistributed.

## Verification retained at wrap-up

| Gate | Result |
|---|---|
| B11 complete test suite | 2,227 assertions; no failures, warnings or skips |
| B11 R CMD check | 0 errors, 0 warnings, 0 notes |
| B12 focused cumulative and language tests | Passed without failures, errors or warnings |
| B12 explicit EN/DE title assertions | Passed |
| B12 fresh source build and installation | Passed |
| B12 source-archive manifest comparison | All 97 shipped checked entries match |
| B12 independent EN/DE acceptance | Passed; no open acceptance finding |
| Author workspace versus accepted manifest | All 109 entries match |

B12 changes only two title literals relative to B11. The full suite/check were
not repeated for that wording change. Wrap-up adds verification documentation
only, excluded from the package. The full history and evidence boundaries are
recorded in `material-ux-2026-09-27.md` and the B11/B12 reviewer reports.

The checked B12 archive has MD5 `a7b1c70d845a599c251a5b034683886c`.
Each package-size budget is 5,000,000 bytes:

| Component | Bytes |
|---|---:|
| Source archive | 3,169,825 |
| Bundled data | 475,503 |
| Documentation and app resources | 2,761,852 |
| Installed package | 4,089,391 |

`cran-size-2026-09-27.md` records the checks and the invocation of
`data-raw/validate_package_size.R`. These are local package checks, not CRAN
submission or acceptance.

## Performance evidence

Five alternating warm-browser repetitions gave a startup median of 1.186 s
before delayed initialization and 0.636 s afterwards, a reduction of 0.550 s
or 46.4%. The first material visit rose from 2.416 s to 2.714 s; revisits stayed
at approximately 0.019 s. The optimization defers work until it is needed.
The local/cache measurement boundaries and R calculations are documented in
`startup-lazy-2026-09-29/README.md`. The initial cause comparison with the
original Spectran is in `startup-2026-09-29/README.md`.

The versioned measurement records, sampling profiles, scripts and environment
records are intentional verification inputs and evidence. They are excluded
from the package. Git does not preserve original file modification times, so
reanalyses of these archived measurements must use the recorded run/phase
columns in the consolidated measurement CSVs rather than infer acquisition
order from checkout timestamps. The original acquisition scripts preserve
the procedure for collecting new runs.

## Retained boundaries

The numerical reference report documents the unresolved FL11 discrepancies
and inconsistent public TUB stone-summary cells. UX acceptance does not
constitute a fresh independent numerical DIN audit. Narrow layouts remain
desktop-oriented; the review was not an exhaustive browser or assistive-
technology certification. Browser-controller file-transfer delays were
distinguished from app calculation time.

The local `.Renviron`, pre-existing `renv.lock` and `renv/activate.R` edits,
deployment configuration and incidental `Rplots.pdf` files are deliberately
excluded from this commit and preserved. The unrelated trailing-blank-line
edit in `R/runApp.R` is also excluded. R parsing verifies that HEAD and the
accepted `R/runApp.R` contain identical expressions, so this whitespace
exception does not change the checked implementation. No package version or
dependency update is part of the wrap-up.

The staged index was compared with all 109 accepted manifest entries: 108 are
byte-identical and the `R/runApp.R` whitespace exception has identical R parse
trees. Code, tests and authored documentation pass the whitespace check.
The unfiltered check additionally flags the original CRLF endings of the two
unchanged TUB source downloads and trailing spaces in verbatim R profiles and
session-information records. Those evidence files are preserved without
normalization. No private configuration, build archive, package-check output
or incidental plot file is staged.
