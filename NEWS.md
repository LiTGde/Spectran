# Spectran 2.0.0

## Material workflow

* Redesign Transmission and Reflection around Setup, Results, Light path,
  and Downloads, with progressive disclosure and LiTG styling. Use separate
  transmission and reflection icons and labelled, equal-width mode controls.
  Identify the incident light source with a sun icon with separate straight rays.
* Replace stacked material selectors with a searchable library grouped by
  category, spectral previews, measurement details, source information, and
  explicit completion choices. Offer spectral colours, distinguish facade
  glazing products from glazing examples, and remove missing-value artefacts
  from display names.
* Show named source links before material selection, distinguish collection
  citations from original per-material references, and omit missing metadata
  from the library cards. Keep visible provenance tied to the active source
  when switching between a catalogue material and an uploaded CSV.
* Allow source selection and CSV import within the material workspace,
  including previously saved light-path nodes. Set either photopic illuminance
  or melanopic EDI when selecting a source or continuing a result.
* Save a result independently of continuing with another material. Offer
  optional incident-source names in saved result labels, preserve branches,
  and restore earlier nodes as active sources.
* Show saved results or combined material effects in the light path. Place
  the cumulative spectrum plot above the metric tables, distinguish the
  original spectrum, intermediate steps and final result, and provide PNG and
  CSV downloads. Explain readability limits for long paths.
* Explain the F = 1 receiver assumption in Setup, Results, and the illustrated
  guidance. Keep material effects and receiver geometry distinct.
* Display a modal while the material workspace initializes, closing it
  automatically when ready. Keep the first-use lazy initialization.
* Show required measurement confirmations in separate, always-visible panels.
  Highlight missing material fields with instructions beside each input and
  group optional measurement details separately.
* Set negative light-spectrum measurements to zero before interpolation,
  scaling and material calculations. Show a warning, preserve original values
  and the correction in source provenance and audit exports, and allow saving
  and further material steps. Material-coefficient validation is unchanged.
* Add optional measuring-instrument metadata (model and calibration year) and
  relative measurement error. Preserve both in saved results and audit exports.
  Populate the documented Bruins Instruments OMEGA 20 model for TU Berlin
  records, explicitly leaving undocumented calibration years and errors unknown.
  These descriptive fields do not propagate measurement uncertainty.

## Introduction and guidance

* Modernize the introduction with equal-height feature cards, visible example
  galleries, material-workflow examples and direct links to the app and guidance.
  Open illustrations and screenshots in a lightbox and keep the tutorial visible.
* Add a bilingual illustrated explanations page covering spectra and metrics,
  weighting, EDI and DER, material effects, receiver assumptions, and individual
  steps versus cumulative light paths. Add contextual entry points and
  previous/next topic navigation, and format scientific indices as subscripts.
* Extend About with the material module and its project committee:
  Karin Bieske, Kai Broszio, and Nils Haferkemper.
* Use the consistent brand name "LiTG Spectran" in the introduction, material
  workspace, and explanations.
* Present contextual help as plain links and place the analysis links below
  the plot and table panels.
* Add the material-validation article and checks for package size. Keep
  development screenshots and review records out of the CRAN source archive;
  load explanatory illustrations on demand.
* Exclude nested Quarto preview caches from the source package to retain the
  package-size budget and avoid non-portable archive paths.

## Standard-module plot fixes

* Label spectral Y-axes as "Spectral irradiance" or "Spektrale
  Bestrahlungsstärke", with units on a separate line for compact plots and
  exports. Integrated irradiance labels retain their meaning (#32).
* Draw radiometric and weighted spectral outlines with the same path geometry
  and foreground linewidth. Keep background spectra visually subordinate (#31).
* Wrap titles and comparison subtitles in age plots. At narrow widths, move
  legends and the optional transmission inset below the spectrum and provide
  more vertical space in the app. Apply the same width-aware layout to exports.
  Keep long titles clear of the inset, compact axes and sensitivity labels
  within the figure, and allow disabling the inset without a plotting error.
* Set the package version to 2.0.0 and use the installed version in material
  audit citations.

## Material calculations and data

* Add the 55 TU Berlin transmission and reflection examples as the default
  material catalogue, with source attribution and measurement metadata.
* Add reflection as spectral radiant exitance and carry receiver spectra forward
  under an explicit default F = 1 assumption. Both interaction modes allow an
  optional photopic illuminance or melanopic EDI target while preserving the
  unscaled calculation.
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
* Replace cumulative summaries after light-level rescaling with a notice and
  recovery guidance. Omit cumulative action factors and cite DIN/TS 67600 for
  effective MDER.
* History row buttons select a node for
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
