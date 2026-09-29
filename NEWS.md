# Spectran (development version)

* Add the 55 TU Berlin transmission and reflection examples as the default
  material catalogue, with source attribution and measurement metadata.
* Add reflection as spectral radiant exitance and carry receiver spectra forward
  under an explicit default F = 1 assumption. Both interaction modes allow an
  optional target illuminance while preserving the unscaled calculation.
* Compare cumulative material effects with each branch's original source,
  including effective MDER and auditable exports.
* Add public TUB reference tests and a reproducible DIN/TS 67600 comparison
  script. Document unresolved FL11 and source-table discrepancies in
  `inst/material-model.md`.
* Preview approximate reflection material colours under D65, preserving
  relative lightness. Show DIN glazing descriptions and
  retain the TUB alias where material names differ.
* Show source-weighted material coefficients and their D65 comparison first.
  Group EDI and DER separately in the alpha-opic table. Light and radiation
  contains photopic illuminance first, followed by irradiances. Table exports
  and archived results use the same grouping.
* Allow direct entry to Transmission / Reflection before importing a source,
  with automatic daylight D65 at 100 lx and an explicit notice. Existing
  imported sources are preserved.
* Initialize the material server on its first visit instead of at app startup,
  retaining the initialized workflow and history throughout the session.
* Mark the collapsible material-colour explanation with a disclosure arrow.
* Replace cumulative summaries after illuminance rescaling with a notice and
  recovery guidance. Omit cumulative action factors and cite DIN/TS 67600 for
  effective MDER.
* Separate Promotion and History tabs. History row buttons select a node for
  both cumulative and archived results, or restore it as the active source.
  Disable unavailable actions, use short N1/N2 node labels, and remove the
  redundant sequence column and separate node selectors.
* Keep partial material-curve previews available while completion decisions
  are pending. Reserve a shared title/subtitle area above exported result
  panels, measured at the chosen width and font size. Put irradiance units on
  a separate axis-title line to avoid panel-tag collisions in compact exports.

# Spectran 1.0.6

* Update to CRAN

# Spectran 1.0.5

*changes in Citation to reflect backup in Zenodo.org and DOI

# Spectran 1.0.4

*changed CITATION to reflect the name change of the LiTG

*fixed an incorrect unit in the alpha-opic comparison plot

# Spectran 1.0.3

* small changes to prepare the package for CRAN submission

# Spectran 1.0.0

* removed the reference to `OPN4` on the melanopic evaluation #15

* removed the term `quanta` for the more well known term `photons` #13

* made a reference to the complete formula for the age-dependent correction #12 and removed the incorrect references to the CIE S026 #11

* removed the reference to the DIN/SPEC 5031-100 for alpha-opic weighing #10

* changed the grouping separator in tables from `.` to a whitespace, e.g., ` `. #8

* showing Color Rendering Index correctly as CRI when language is set to English #7

* changed the naming conventions for the cones in line with the CIE S026 #3

* added an option to set how the input data is scaled before importing #4

* fixed a bug that rounded every table numeric to three significant digits. #5 #9

* fixed a bug where negative color rendering values would not show in the plot. #16

* outsourced the validation page and added one about the used values.

* added three new color palettes for the spectral Plots. They can be chosen when starting Spectran #14

# Spectran 0.9.2.9000

* Added a `NEWS.md` file to track changes to the package.
