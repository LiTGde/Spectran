# Transmission and reflection

The material page provides the 27 transmission and 28 reflection examples from
TU Berlin, version 2, as the default catalogue. Existing glazing and filter
collections remain available in transmission mode. CSV coefficients must be
explicitly identified as fractions or percentages; the completed calculation
grid is 380–780 nm in 1 nm steps.

Glazing G1–G12 includes the common DIN Table 6 descriptions and the original
TUB construction codes. German surface labels follow DIN Tables 9–11. S1 is
shown as "Backstein, gelb (TUB: beige)" to preserve the differing source names.
The concise DIN concrete names are used, with TUB pore descriptions in Details.
These label changes do not alter spectral coefficients or their identities.

Without an imported source, opening Transmission / Reflection activates
bundled CIE D65 at 100 lx as the original basis of the light path. A visible
notice identifies this automatic choice until an explicit source is imported.
An existing source, including a zero spectrum, is never replaced automatically.
Later imports retain the usual confirmation before replacing a promoted history.

Negative measured light-spectrum values, for example from measurement noise,
are set to zero before interpolation, scaling, or material calculations. The
source and result show the correction count. Saving and additional material
steps remain available. The source provenance and every subsequent snapshot
retain the original negative measurements, their wavelengths, canonical units
(W m^-2 nm^-1), and the processing stage. The audit ZIP includes these in
`spectra/source-negative-values.csv`, as well as a decision and warning. A new
clean source clears this history of corrections. This does not relax material
coefficient validation, which still requires values from 0 to 1.

Optional material metadata records the measuring instrument (model and
calibration year) and relative measurement error, including any supplied
conditions. These descriptions are retained with snapshots and audit exports;
measurement uncertainty is not propagated into calculated metrics. The
[TU Berlin measurement methods](https://api-depositonce.tu-berlin.de/server/api/core/bitstreams/891785be-774c-4de7-a168-22af70e19302/content),
page 1, identify a Bruins Instruments OMEGA 20 for all samples. No calibration
year or relative measurement error is specified there. Other library records
leave these fields blank when the source does not document them.

The results start with **Transmissionsgrad** or **Reflexionsgrad**, followed by
**Licht und Strahlung** (photopic illuminance first, then irradiances) and
**alpha-opisch** (five EDI values in lux, then a separate DER group containing
five DER values and effective MDER, without action factors). Current results,
archives and dedicated exports use the same grouping. The coefficient table compares six
weighted coefficients under the applied incident spectrum and D65, in percent.
Its signed difference is incident-spectrum weighting minus D65 weighting in
percentage points, not relative percent. A zero incident weighted denominator
leaves the incident coefficient and difference undefined while D65 remains
available. The table CSV contains the same comparison. Audit ZIPs additionally
retain the original full D65 property records and all calculated metrics.

# Approximate reflection colour

Reflectance multiplied by D65 is integrated using the CIE 1931
2-degree colour-matching functions over 380–780 nm. XYZ is divided by the
photopic Y integral of a perfect diffuser under that illuminant. Thus Y = 1
for ideal white and Y = 0.5 for neutral 50% reflectance; material lightness is
preserved. The preview shows only the D65 reference, independent of the
selected source spectrum and its illuminance, including a zero source.
Incomplete curves require completion before a preview is shown.

XYZ is converted to sRGB using the D65 display-white matrix and piecewise
transfer function in the [ICC sRGB interpretation](https://www.w3.org/Graphics/Color/srgb).
Linear channels outside 0–1 are clipped. This is a screen
approximation, without gloss, texture, viewing-angle effects, or colour-appearance
modeling. Assumed spectral tails also affect the preview. The observer is
`colorSpec::xyz1931.1nm`; the [CIE observer reference](https://cie.co.at/datatable/cie-1931-colour-matching-functions-2-degree-observer)
defines the underlying 2-degree functions. No new dependency or bitmap data
is added. Archived colour previews use the frozen material curve under D65.

For transmission the material calculation is `E'λ = Eλ τλ`. For reflection its
native result is spectral radiant exitance, `Mλ = Eλ ρλ`, in W m⁻² nm⁻¹.
Reflection alone does not determine irradiance at another surface or sensor.
Spectran reports receiver metrics under the explicit default `F = 1`: a
uniformly illuminated Lambertian reflecting surface fills the receiver
hemisphere. Under that assumption receiver irradiance equals exitance
numerically. The Lambertian relation `Lλ = Mλ / π` describes radiance; it is not
an extra attenuation factor to apply to the receiver spectrum. The material
measurements do not establish that every example is Lambertian.

Transmission also uses `F = 1` by default: all transmitted light represented by
the measured coefficient reaches the receiver. This may be inappropriate for
scattering samples in a different measurement or receiver geometry.

At promotion, the illuminance field is prefilled with the calculated receiver
illuminance. Keeping it preserves attenuation. Changing it multiplies the
entire receiver spectrum by `target Ev / default Ev`. A value above the default
defines a new receiver scenario, rather than material gain. Zero is accepted;
a zero-photopic spectrum cannot be rescaled to positive illuminance. The
unscaled calculation remains immutable in the history, alongside the target,
factor, material definition, and receiver assumption.

Any chosen sequence of transmissions and reflections can be promoted. This
models a specified light path through passive, nonfluorescent materials. It
does not solve room geometry, BRDFs, fluorescence, or automatically sum room
interreflections.

# Cumulative comparisons

Promotion and History are separate tabs. In History, each row's Show button
selects the same node for the cumulative comparison and archived material
result. It does not change the active source. Restore selects that node and
also makes it the active source for the next material interaction, preserving
later branches. Show is disabled for the displayed node; Restore is disabled
for the active node. The source node has no archived material calculation.
Visible node labels are N1, N2, and so on; the original node IDs and sequence
numbers remain in audit data for reproducibility.

For branches without illuminance rescaling, the history page offers one
comparison with the selected branch's original source. **Combined material effect**
multiplies its spectral material coefficients, recalculates metrics from the
original and final spectra, and omits action factors. If any selected ancestor
includes rescaling, a recovery notice replaces the table and its download.
The cumulative metrics entry in the audit ZIP likewise records that notice;
individual snapshots, scaling metadata and spectra remain available. Percent changes are not added. Restoring an earlier
node preserves other branches, and only the selected node's ancestors contribute.

Effective MDER is final melanopic EDI divided by the original photopic
illuminance. In a single step its reference is that step's incident illuminance.
DIN/TS 67600:2022-08, sections 6.2.4.2 and 6.2.4.4, explicitly describe this
effective MDER, with numerical examples in Tables 6 and 11. It differs from
final MDER, whose denominator is final photopic illuminance. The tables include
this reference so the different denominator remains explicit.
Undefined denominators remain explicitly undefined, including zero-source
comparisons. The cumulative CSV contains this material comparison and branch IDs.
Audit ZIPs additionally contain per-node material curves, unscaled spectra,
promoted spectra, source citations, promotion metadata, and cumulative spectra.

# Reference verification

Scientific verification uses R and the existing Spectran action spectra and
illuminants. Reproduce the local DIN comparison with:

```
Rscript --vanilla data-raw/validate_material_references.R DIN.txt TUB.txt OUTPUT
```

Use page-preserving `pdftotext -layout` extractions of DIN/TS 67600:2022-08 and
the TUB source documentation. Point R to the existing project library. The
script records input hashes, R/package versions, all comparisons, and summary
results in `OUTPUT`. Licensed DIN tables are not distributed with the package.
The automatic package tests include 100 public TUB photopic reference values.

Initial verification used R 4.6.1, dplyr 1.2.1, tibble 3.3.1, purrr 1.2.2,
and the package's existing 1 nm integration. For matching materials, all
non-FL11 DIN comparisons fell within a precision budget that accounts for the
three-decimal coefficient downloads and the three-decimal DIN tables. The
budget is 0.001 for weighted coefficients and effective MDER, and 0.003 for
the ratio MDER; the report separately records the stricter 0.0005 rounding-only
criterion. These are numerical reproduction tolerances, not measurement
uncertainties or a claim of normative certification.

Known reference limitations:

- DIN table 6 matches TUB glazing G1–G12. Tables 9–11 match 27 TUB surfaces;
  TUB's white lacquer L1 is additional. The materials in DIN tables 7 and 8
  do not have identified matching curves in this TUB release.
- DIN's FL11 photopic and effective-MDER values are not reproduced by these
  spectra. Spectran's FL11 shape was checked against the official CIE 2018
  dataset, DOI 10.25039/CIE.DS.ukaymjdn, MD5
  `441613e501ab0a58e62f2669ff005db7`, and agrees to floating-point precision.
  These reference discrepancies remain unresolved and are reported explicitly.
- TUB's CSV repeats the fourth wood heading and shifts subsequent wood labels.
  Spectran uses the column identities in the source PDF's spectral Table 2.
  The original CSV headings and column positions remain in provenance.
- TUB Table 2's five stone integral summaries are inconsistent with its spectral
  column order. The spectral coefficients remain unchanged; those ten integral
  summaries are excluded from the automatic public-reference fixture and remain
  visible as discrepancies in the full reference report. The matching DIN stone
  rows support the assigned spectral identities.

TUB source and licence: https://doi.org/10.14279/depositonce-11893.2,
Creative Commons Attribution 4.0 International. The original CSV files,
attribution, and public-reference fixture are in `inst/extdata/tub`.
