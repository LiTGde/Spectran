# External validation references

`validation-references.rds` is a small, self-contained list for the ordinary
`testthat` suite. It contains the ten original 380–780 nm spectra, external
expected values from CIE/luox/DIN, the exact exceptions, and source provenance.
It runs offline from a built package. Tests do not need Excel files, article
rendering, a licensed PDF, or a live service.

Two generated text files support review without R:

- `reference-values.csv`: all 706 external reference values, their comparison
  status, and the separately labelled 25 FL11 regression anchors. Numbers use
  17 significant digits to retain the binary fixture's double precision.
- `reference-input-checksums.csv`: the 35 archived input paths and their MD5
  checksums, including the bundled inputs used only for the FL11 anchors.

A unit test checks that these readable exports match the binary fixture. They
are audit copies, not an additional source of expected results. An environment
without R can inspect these files, but executing the scientific verification
still requires R.

The expected values are extracted from the same archived inputs used by
`vignettes/articles/Validation.Rmd` and `Material_Validation.qmd`:

| Source | Coverage | Archived source |
| --- | --- | --- |
| CIE | 120 values: 12 metrics for 10 spectra | CIE S 026 alpha-opic Toolbox v1.049a (2020/11), exported 21 October 2022 |
| luox | 130 values: 13 metrics for 10 spectra; 2 CRI results explicitly inapplicable | Exports dated 21 October 2022, including Robertson (1968) CCT |
| DIN | 456 coefficients: 12 glazing samples under D65 and 27 reflecting materials under 8 illuminants | DIN/TS 67600:2022-08, Tables 6, 9 and 10, with documented TUB specimen assignments |

The DIN data here concern material coefficients in DIN/TS 67600. They are
distinct from the DIN 5031-100 reference used elsewhere in the app. The 32
DIN materials without matched spectra are recorded as uncovered. They cannot
be tested against spectral calculations. Age-corrected metrics are not covered
by these reference sources either.

## Calculations and tolerances

Tests call the app's calculation helpers. Spectran clips negative measured
irradiance to zero through `prepare_spectran_source()`. The original negative
samples remain in the fixture. The archived CIE inputs were also clipped;
historical luox outputs can differ because of their negative-input handling.
Colour tests use the app's strict CIE applicability settings.

Bounds are applied **element by element**, in the recorded units:
`abs(actual - reference) <= absolute + relative * abs(reference)`.
Missing, unmatched and non-finite results fail.

| Comparison | Absolute limit | Relative limit | Rationale |
| --- | --- | --- | --- |
| CIE irradiances and illuminance | `1e-10` | `1e-6` (0.0001%) | Small input/constant precision differences |
| CIE alpha-opic EDI | `1e-10` | `2e-5` (0.002%) | Rounded bundled daylight efficacy constants; consistent with the existing CIE efficacy check |
| luox irradiances, illuminance and EDI | `1e-10` | `5e-4` (0.05%) | Project agreement budget including the documented negative-input difference |
| luox CCT | 0.1 K | 0 | Project agreement budget for the archived colour calculation |
| luox CRI | 0.5 Ra points | 0 | Project agreement budget below one display unit; archived CRI values have coarser precision |
| DIN normal coefficients | 0.001 | 0 | Up to 0.0005 for rounded spectral coefficients plus 0.0005 for published coefficients |
| FL11 regression anchors | `1e-6` | 0 | Frozen explicit sums rounded to six decimals, with numerical slack |

These are numerical reproduction budgets, not measurement uncertainties or
normative conformity limits. They do not establish accuracy for every possible
spectrum. The two luox CRI cases `EE_Spektrum` and `LED_4000K_2` must return `NA`
under Spectran's strict applicability setting. Their published luox values are
retained, not silently removed from the fixture.

## Known FL11 discrepancy

The material-validation article records correspondence between Johannes Zauner
and Dieter Lang on 8 October 2026: the published FL11 photopic reflection
coefficients have a known error and a correction is planned. Precisely 25 cases
exceed 0.001: DIN IDs 401–413 and 416–427, **photopic reflection under FL11 only**.
All 27 FL11 melanopic values and photopic IDs 414/415 keep the normal limit.

For the 25 exceptions, tests assert both that the signed difference from DIN
remains below -0.001 and that the result agrees with a frozen regression anchor
within `1e-6`. The anchors come from explicit weighted sums over the bundled
TUB coefficients, FL11 spectrum and photopic response, without invoking Spectran
metric functions. They are regression evidence, not independent validation or
corrected DIN values. The original DIN numbers remain unchanged. Returning the
erroneous DIN value, changing the calculation, or broadening the exception fails
the checks. Review the exception when an official correction becomes available.

## Rebuilding and running

From the repository root, in the project's R environment:

```r
source("data-raw/validation_references.R")
devtools::test(filter = "reference-")
```

The builder needs `readxl`, already in Suggests. It validates export layouts,
case counts, assignments and numeric values. It writes this fixture and its two
readable audit copies, and records the command, R/readxl versions, input paths and MD5 checksums in
`$provenance`. No Spectran calculation function produces the external expected
values. The FL11 regression anchors are the explicitly labelled exception.

The normal `testthat` run and `R CMD check` load the committed fixture directly;
they never rebuild it. Review source changes, numerical diffs and any changes
to the exception list before committing a regenerated fixture. Do not rebuild
it merely to make a failing regression test pass.
