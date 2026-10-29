# `Spec`ifications for visual and nonvisual data wrangling and plotting

Contains vectors and tables that are the main basis for the calculations
Spectran does and also contain basic naming and coloring conventions.

## Usage

``` r
Specs
```

## Format

`Specs` A list with 7 elements

- AS_wide:

  A table of action spectra for the 5 human receptor types and V(lambda)
  from 1924. The table is in the wide format. The unit of the wavelength
  (Wellenlaenge) is nm, the unit of the action spectrum is 1
  (dimensionless).

- AS_long:

  A table of action spectra for the 5 human receptor types and V(lambda)
  from 1924. The table is in the long format. The unit of the wavelength
  (Wellenlaenge) is nm, the unit of the action spectrum is 1
  (dimensionless).

- Efficacy:

  A named vector of inverse values of efficacy of luminous radiation for
  the 5 human receptor types and V(lambda) from 1924. The unit is lm/W.

- Alpha:

  A list. `names` contains the names of the 5 human receptor types.
  `adjectives` contains these names in their adjective form for every
  languages included in Spectran. `descriptions` contains the
  descriptions used for tables and plotting. `abb` contains the
  abbreviations (if any) used for the equivalent daylight illuminance
  for that receptor type.

- Vlambda:

  A character scalar containing the properly escaped version of
  V(lambda), used e.g. in Plots

- Alpha.ico:

  A character scalar containing the properly escaped version of the sign
  `alpha`, used e.g. in Plots

- Plot:

  A table of Peak Wavelength (nm) values, Names, Abbreviations, and
  colors for the 5 human receptor types and V(lambda) from 1924. This
  table is mainly used for plotting.

## Source

[CIE
S026](https://cie.co.at/publications/cie-system-metrology-optical-radiation-iprgc-influenced-responses-light-0)

[DIN/TS
5031-100](https://www.dinmedia.de/de/vornorm/din-ts-5031-100/343737176)
